package com.williambl.haema

import com.mojang.brigadier.arguments.DoubleArgumentType
import com.mojang.brigadier.arguments.IntegerArgumentType
import com.williambl.haema.abilities.VampireAbilityArgumentType
import com.williambl.haema.abilities.abilityRegistry
import com.williambl.haema.abilities.registerAbilities
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.api.VampireBurningEvents
import com.williambl.haema.blood.registerBlood
import com.williambl.haema.component.VampireComponent
import com.williambl.haema.component.VampirePlayerComponent
import com.williambl.haema.craft.BookOfBloodRecipe
import com.williambl.haema.criteria.VampireHunterTriggerCriterion
import com.williambl.haema.criteria.registerCriteria
import com.williambl.haema.damagesource.registerDamageSources
import com.williambl.haema.effect.registerEffects
import com.williambl.haema.hunter.VampireHunterSpawner
import com.williambl.haema.hunter.registerVampireHunter
import com.williambl.haema.hunter.structure.registerStructures
import com.williambl.haema.ritual.registerRitualTable
import com.williambl.haema.util.registerGameRules
import com.williambl.haema.util.sunlightDamagesArmour
import com.williambl.haema.util.vampiresBurn
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import dev.onyxstudios.cca.api.v3.entity.RespawnCopyStrategy
import me.lucko.fabric.api.permissions.v0.Permissions
import net.fabricmc.fabric.api.command.v1.CommandRegistrationCallback
import net.fabricmc.fabric.api.entity.event.v1.EntitySleepEvents
import net.fabricmc.fabric.api.event.player.UseBlockCallback
import net.fabricmc.fabric.api.event.player.UseEntityCallback
import net.fabricmc.fabric.api.tag.TagRegistry
import net.fabricmc.fabric.api.util.TriState
import net.minecraft.block.BedBlock
import net.minecraft.block.enums.BedPart
import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.command.argument.IdentifierArgumentType
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.passive.VillagerEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.ActionResult
import net.minecraft.util.Identifier
import net.minecraft.util.math.Box
import net.minecraft.util.registry.Registry
import net.minecraft.village.VillageGossipType
import net.minecraft.world.World
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import top.theillusivec4.somnus.api.WorldSleepEvents

val logger: Logger = LogManager.getLogger("Haema")

fun init() {
    UseEntityCallback.EVENT.register(UseEntityCallback { player, world, hand, entity, entityHitResult ->
        if ((player as Vampirable).isVampire && entity is LivingEntity && player.isSneaking && BloodDrinkingEvents.CANCEL.invoker().canDrink(player, world, hand, entity, entityHitResult))
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

    EntitySleepEvents.ALLOW_SLEEP_TIME.register(EntitySleepEvents.AllowSleepTime { player, pos, vanillaResult ->
        if (player is Vampirable) {
            if (player.isVampire && player.world.isDay) {
                return@AllowSleepTime ActionResult.SUCCESS
            }
        }

        ActionResult.PASS
    })

    WorldSleepEvents.WORLD_WAKE_TIME.register(WorldSleepEvents.WorldWakeTime {world, newTime, curTime ->
        if (!world.isDay) {
            newTime
        } else {
            curTime + (13000L - (world.timeOfDay % 24000L))
        }
    })

    VampireBurningEvents.TRIGGER.register(VampireBurningEvents.Trigger { player, world ->
        if (world.isDay && !world.isRaining && world.isSkyVisible(player.blockPos)) TriState.TRUE else TriState.DEFAULT
    })
    VampireBurningEvents.VETO.register(VampireBurningEvents.Veto { _, world ->
        if (world.gameRules[vampiresBurn].get()) TriState.DEFAULT else TriState.FALSE
    })
    VampireBurningEvents.VETO.register(VampireBurningEvents.Veto { player, _ ->
        if (player.abilities.creativeMode) TriState.FALSE else TriState.DEFAULT
    })
    val vampireProtectiveClothingTag = TagRegistry.item(Identifier("haema:vampire_protective_clothing"))
    VampireBurningEvents.VETO.register(object : VampireBurningEvents.Veto {
        override fun getPriority(): Int = 10

        override fun willVampireBurn(player: PlayerEntity, world: World): TriState {
            return if (player.armorItems.all { vampireProtectiveClothingTag.contains(it.item) }) {
                player.armorItems.forEachIndexed { i, stack ->
                    if (world.random.nextFloat() < 0.025 && world.gameRules[sunlightDamagesArmour].get()) {
                        stack.damage((world.random.nextFloat() * (i + 2)).toInt(), player) {
                            world.playSoundFromEntity(
                                null,
                                player,
                                SoundEvents.ENTITY_GENERIC_BURN,
                                SoundCategory.PLAYERS,
                                1f,
                                1f
                            )
                        }
                    }
                }
                TriState.FALSE
            } else TriState.DEFAULT
        }
    })

    BloodDrinkingEvents.ON_BLOOD_DRINK.register(BloodDrinkingEvents.DrinkBloodEvent { drinker, target, world ->
        if (target is VillagerEntity && !target.isSleeping) {
            target.gossip.startGossip(drinker.uuid, VillageGossipType.MAJOR_NEGATIVE, 20)
            if (drinker.world is ServerWorld) {
                if (drinker is ServerPlayerEntity) {
                    VampireHunterTriggerCriterion.trigger(drinker)
                }
                VampireHunterSpawner.instance.trySpawnNear(
                    drinker.world as ServerWorld,
                    drinker.random,
                    drinker.blockPos
                )
            }
        }
    })

    registerAbilities()

    registerRitualTable()

    registerEffects()

    registerBlood()

    registerVampireHunter()

    registerGameRules()

    registerCriteria()

    registerStructures()

    registerDamageSources()

    Registry.register(
        Registry.RECIPE_SERIALIZER,
        Identifier("haema:book_of_blood"),
        BookOfBloodRecipe.Serializer
    )

    CommandRegistrationCallback.EVENT.register { dispatcher, isDedicated ->
        dispatcher.register(
            literal("haema")
                .then(literal("convert")
                    .requires(Permissions.require("haema.command.convert", 2))
                    .then(argument("targets", EntityArgumentType.players()).executes { context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach(Vampirable.Companion::convert)
                        return@executes 1
                    })
                )
                .then(literal("deconvert")
                    .requires(Permissions.require("haema.command.deconvert", 2))
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
                    .requires(Permissions.require("haema.command.blood", 2))
                    .then(argument("targets", EntityArgumentType.players()).then(argument("amount", DoubleArgumentType.doubleArg(0.0, 20.0)).executes { context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach {
                            if ((it as Vampirable).isVampire && it.hungerManager is VampireBloodManager) {
                                (it.hungerManager as VampireBloodManager).absoluteBloodLevel = DoubleArgumentType.getDouble(context, "amount")
                            }
                        }
                        return@executes 1
                    })))
                .then(literal("abilities")
                    .requires(Permissions.require("haema.command.abilities", 2))
                    .then(literal("get").then(argument("targets", EntityArgumentType.players()).executes {  context ->
                        EntityArgumentType.getPlayers(context, "targets").forEach {
                            if ((it as Vampirable).isVampire) {
                                context.source.sendFeedback(it.name.copy().append(" has abilities:"), false)
                                abilityRegistry.entries.forEach { (key, ability) ->
                                    context.source.sendFeedback(TranslatableText("ability.${key.value.namespace}.${key.value.path}").append(": ${it.getAbilityLevel(ability)}"), false)
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
                    .requires(Permissions.require("haema.command.rituals", 2))
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
