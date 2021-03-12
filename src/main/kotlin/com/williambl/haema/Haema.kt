package com.williambl.haema

import com.mojang.brigadier.arguments.DoubleArgumentType
import com.mojang.brigadier.arguments.IntegerArgumentType
import com.williambl.haema.abilities.VampireAbility
import com.williambl.haema.abilities.VampireAbilityArgumentType
import com.williambl.haema.abilities.registerAbilities
import com.williambl.haema.api.WillVampireBurn
import com.williambl.haema.blood.registerBlood
import com.williambl.haema.component.VampireComponent
import com.williambl.haema.component.VampirePlayerComponent
import com.williambl.haema.craft.BookOfBloodRecipe
import com.williambl.haema.effect.registerEffects
import com.williambl.haema.hunter.registerVampireHunter
import com.williambl.haema.ritual.registerRitualTable
import com.williambl.haema.util.registerGameRules
import com.williambl.haema.util.vampiresBurn
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import nerdhub.cardinal.components.api.util.RespawnCopyStrategy
import net.fabricmc.fabric.api.command.v1.CommandRegistrationCallback
import net.fabricmc.fabric.api.event.player.UseBlockCallback
import net.fabricmc.fabric.api.event.player.UseEntityCallback
import net.fabricmc.fabric.api.util.TriState
import net.minecraft.block.BedBlock
import net.minecraft.block.enums.BedPart
import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.command.argument.IdentifierArgumentType
import net.minecraft.entity.LivingEntity
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.text.Text
import net.minecraft.util.ActionResult
import net.minecraft.util.Identifier
import net.minecraft.util.math.Box
import net.minecraft.util.registry.Registry
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import top.theillusivec4.somnus.api.PlayerSleepEvents
import top.theillusivec4.somnus.api.WorldSleepEvents

val logger: Logger = LogManager.getLogger("Haema")

fun init() {
    UseEntityCallback.EVENT.register(UseEntityCallback { player, world, hand, entity, entityHitResult ->
        if ((player as Vampirable).isVampire && entity is LivingEntity && player.isSneaking)
            (player.hungerManager as VampireBloodManager).feed(entity, player)
        else ActionResult.PASS
    })

    UseBlockCallback.EVENT.register(UseBlockCallback { player, world, hand, blockHitResult ->
        val state = world.getBlockState(blockHitResult.blockPos)

        if (state.block !is BedBlock) {
            return@UseBlockCallback ActionResult.PASS
        }

        val pos = if (state.get(BedBlock.PART) == BedPart.HEAD)
            blockHitResult.blockPos
        else
            blockHitResult.blockPos.offset(state.get(BedBlock.FACING))
        val entities = world.getOtherEntities(player, Box(pos)) { it is LivingEntity && it.isSleeping }

        if (entities.isNotEmpty() && (player as Vampirable).isVampire && player.isSneaking) {
            (player.hungerManager as VampireBloodManager).feed(entities[0] as LivingEntity, player)
        } else ActionResult.PASS
    })

    PlayerSleepEvents.CAN_SLEEP_NOW.register(PlayerSleepEvents.CanSleepNow { player, pos ->
        if (player is Vampirable) {
            if (player.isVampire && player.world.isDay) {
                return@CanSleepNow TriState.TRUE
            }
        }

        TriState.DEFAULT
    })

    WorldSleepEvents.WORLD_WAKE_TIME.register(WorldSleepEvents.WorldWakeTime {world, newTime, curTime ->
        if (!world.isDay) {
            newTime
        } else {
            curTime + (13000L - (world.timeOfDay % 24000L))
        }
    })

    WillVampireBurn.EVENT.register(WillVampireBurn { _, world ->
        if (!world.gameRules[vampiresBurn].get()) TriState.FALSE else TriState.DEFAULT
    })
    WillVampireBurn.EVENT.register(WillVampireBurn { player, world ->
        if (world.isDay && !world.isRaining && world.isSkyVisible(player.blockPos)) TriState.TRUE else TriState.DEFAULT
    })
    WillVampireBurn.EVENT.register(WillVampireBurn { player, world ->
        if (player.abilities.creativeMode) TriState.FALSE else TriState.DEFAULT
    })

    registerAbilities()

    registerRitualTable()

    registerEffects()

    registerBlood()

    registerVampireHunter()

    registerGameRules()

    Registry.register(
        Registry.RECIPE_SERIALIZER,
        Identifier("haema:book_of_blood"),
        BookOfBloodRecipe.Serializer
    )

    CommandRegistrationCallback.EVENT.register { dispatcher, isDedicated ->
        dispatcher.register(
            literal("haema")
                .requires { it.hasPermissionLevel(2) }
                .then(literal("convert")
                    .then(argument("targets", EntityArgumentType.players()).executes { context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach(Vampirable.Companion::convert)
                        return@executes 1
                    })
                )
                .then(literal("deconvert")
                    .then(argument("targets", EntityArgumentType.players()).executes { context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach {
                            if (!(it as Vampirable).isPermanentVampire) {
                                it.isVampire = false
                                it.removeBloodManager()
                            }
                        }
                        return@executes 1
                    })
                )
                .then(literal("blood")
                    .then(argument("targets", EntityArgumentType.players()).then(argument("amount", DoubleArgumentType.doubleArg(0.0, 20.0)).executes { context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach {
                            if ((it as Vampirable).isVampire && it.hungerManager is VampireBloodManager) {
                                (it.hungerManager as VampireBloodManager).absoluteBloodLevel = DoubleArgumentType.getDouble(context, "amount")
                            }
                        }
                        return@executes 1
                    })))
                .then(literal("abilities")
                    .then(literal("get").then(argument("targets", EntityArgumentType.players()).executes {  context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach {
                            if ((it as Vampirable).isVampire) {
                                context.source.sendFeedback(it.name.copy().append(" has abilities:"), false)
                                VampireAbility.values().forEach { ability ->
                                    context.source.sendFeedback(Text.of("${ability.name}: ${it.getAbilityLevel(ability)}"), false)
                                }
                            }
                        }
                        return@executes 1
                    }))
                    .then(literal("set").then(argument("targets", EntityArgumentType.players()).then(argument("ability", VampireAbilityArgumentType.ability()).then(
                        argument("level", IntegerArgumentType.integer(0)).executes { context ->
                            EntityArgumentType.getPlayers(context, "targets").forEach {
                                if ((it as Vampirable).isVampire) {
                                    it.setAbilityLevel(VampireAbilityArgumentType.getAbility(context, "ability"), IntegerArgumentType.getInteger(context, "level"))
                                }
                            }
                            return@executes 1
                        }))))
                )
                .then(literal("rituals")
                    .then(literal("get").then(argument("targets", EntityArgumentType.players()).executes { context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach {
                            if ((it as Vampirable).isVampire) {
                                context.source.sendFeedback(it.name.copy().append(" has used rituals:"), false)
                                VampireComponent.entityKey.get(it).ritualsUsed.forEach { ritual ->
                                    context.source.sendFeedback(Text.of(ritual.toString()), false)
                                }
                            }
                        }
                        return@executes 1
                    }))
                    .then(literal("add").then(argument("targets", EntityArgumentType.players()).then(argument("ritual", IdentifierArgumentType.identifier()).executes { context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach {
                            if ((it as Vampirable).isVampire) {
                                it.setHasUsedRitual(IdentifierArgumentType.getIdentifier(context, "ritual"), true)
                            }
                        }
                        return@executes 1
                    })))
                    .then(literal("remove").then(argument("targets", EntityArgumentType.players()).then(argument("ritual", IdentifierArgumentType.identifier()).executes { context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach {
                            if ((it as Vampirable).isVampire) {
                                it.setHasUsedRitual(IdentifierArgumentType.getIdentifier(context, "ritual"), false)
                            }
                        }
                        return@executes 1
                    })))
                )
        )
    }

    logger.info("Everything registered. It's vampire time!")
}

fun registerEntityComponentFactories(registry: EntityComponentFactoryRegistry) {
    registry.registerForPlayers(VampireComponent.entityKey, { player ->  VampirePlayerComponent(player) }, RespawnCopyStrategy.ALWAYS_COPY)
}
