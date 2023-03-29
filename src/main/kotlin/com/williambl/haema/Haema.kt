package com.williambl.haema

import com.mojang.brigadier.arguments.DoubleArgumentType
import com.mojang.brigadier.arguments.IntegerArgumentType
import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.VampireAbilityArgumentType
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.api.VampireBurningEvents
import com.williambl.haema.api.WorldSleepEvents
import com.williambl.haema.blood.BloodModule
import com.williambl.haema.component.EntityVampireComponent
import com.williambl.haema.component.VampireComponent
import com.williambl.haema.core.BookOfBloodItem
import com.williambl.haema.craft.BookOfBloodRecipe
import com.williambl.haema.criteria.VampireHunterTriggerCriterion
import com.williambl.haema.hunter.VampireHunterSpawner
import com.williambl.haema.util.HaemaGameRules
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer
import dev.onyxstudios.cca.api.v3.entity.RespawnCopyStrategy
import me.lucko.fabric.api.permissions.v0.Permissions
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback
import net.fabricmc.fabric.api.entity.event.v1.EntitySleepEvents
import net.fabricmc.fabric.api.event.player.UseBlockCallback
import net.fabricmc.fabric.api.event.player.UseEntityCallback
import net.fabricmc.fabric.api.itemgroup.v1.FabricItemGroup
import net.fabricmc.fabric.api.itemgroup.v1.ItemGroupEvents
import net.fabricmc.fabric.api.util.TriState
import net.minecraft.block.BedBlock
import net.minecraft.block.enums.BedPart
import net.minecraft.command.argument.EntityArgumentType
import net.minecraft.command.argument.IdentifierArgumentType
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.passive.VillagerEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemGroup
import net.minecraft.registry.Registries
import net.minecraft.registry.Registry
import net.minecraft.registry.RegistryKeys
import net.minecraft.registry.tag.TagKey
import net.minecraft.server.command.CommandManager.argument
import net.minecraft.server.command.CommandManager.literal
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.text.Text
import net.minecraft.util.ActionResult
import net.minecraft.util.Identifier
import net.minecraft.util.math.Box

import net.minecraft.village.VillageGossipType
import net.minecraft.world.World
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger

fun id(path: String): Identifier = Identifier("haema", path)

object Haema: ModInitializer, EntityComponentInitializer {
    val LOGGER: Logger = LogManager.getLogger("Haema")
    val ITEM_GROUP: ItemGroup = FabricItemGroup.builder(id("items"))
        .icon { BloodModule.VAMPIRE_BLOOD.defaultStack }
        .build()
    val BOOK_OF_BLOOD: BookOfBloodItem = Registry.register(
        Registries.ITEM,
        id("book_of_blood"),
        BookOfBloodItem(Item.Settings().maxCount(1))
    )

    override fun onInitialize() {
        UseEntityCallback.EVENT.register(UseEntityCallback { player, world, hand, entity, entityHitResult ->
            if ((player).isVampire && entity is LivingEntity && player.isSneaking && BloodDrinkingEvents.CANCEL.invoker().canDrink(player, world, hand, entity, entityHitResult))
                (player.vampireComponent).feed(entity)
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

            if (entities.isNotEmpty() && (player).isVampire && player.isSneaking) {
                (player.vampireComponent).feed(entities[0] as LivingEntity)
            } else ActionResult.PASS
        })

        EntitySleepEvents.ALLOW_SLEEP_TIME.register(EntitySleepEvents.AllowSleepTime { player, pos, vanillaResult ->
            if (player.isVampirable()) {
                if (player.isVampire && player.world.isDay) {
                    return@AllowSleepTime ActionResult.SUCCESS
                }
            }

            ActionResult.PASS
        })

        WorldSleepEvents.WORLD_WAKE_TIME.register(WorldSleepEvents.WorldWakeTime { world, newTime, curTime ->
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
            if (world.gameRules[HaemaGameRules.vampiresBurn].get()) TriState.DEFAULT else TriState.FALSE
        })
        VampireBurningEvents.VETO.register(VampireBurningEvents.Veto { player, _ ->
            if (player.isSpectator || player is PlayerEntity && player.abilities.creativeMode) TriState.FALSE else TriState.DEFAULT
        })
        val vampireProtectiveClothingTag = TagKey.of(RegistryKeys.ITEM, id("vampire_protective_clothing"))
        VampireBurningEvents.VETO.register(object : VampireBurningEvents.Veto {
            override fun getPriority(): Int = 10

            override fun willVampireBurn(player: LivingEntity, world: World): TriState {
                return if (player.armorItems.all { it.isIn(vampireProtectiveClothingTag) }) {
                    player.armorItems.forEachIndexed { i, stack ->
                        if (world.random.nextFloat() < 0.025 && world.gameRules[HaemaGameRules.sunlightDamagesArmour].get()) {
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

        HaemaGameRules.registerGameRules()

        Registry.register(
            Registries.RECIPE_SERIALIZER,
            id("book_of_blood"),
            BookOfBloodRecipe.Serializer
        )

        ItemGroupEvents.modifyEntriesEvent(ITEM_GROUP).register { entries ->
            Registries.ITEM.streamEntries()
                .filter { it.registryKey().value.namespace == "haema" }
                .forEach { entries.add(it.value()) }
        }

        //TODO: redo these commands a bit, use translatabletexts + entities not players + move into separate class(es)
        CommandRegistrationCallback.EVENT.register { dispatcher, registryAccess, environment ->
            dispatcher.register(
                literal("haema")
                    .then(literal("convert")
                        .requires(Permissions.require("haema.command.convert", 2))
                        .then(argument("targets", EntityArgumentType.players()).executes { context ->
                            EntityArgumentType.getPlayers(context, "targets").forEach(::convert)
                            return@executes 1
                        })
                    )
                    .then(literal("deconvert")
                        .requires(Permissions.require("haema.command.deconvert", 2))
                        .then(argument("targets", EntityArgumentType.players()).executes { context ->
                            EntityArgumentType.getPlayers(context, "targets").forEach {
                                if (!(it).isPermanentVampire) {
                                    it.isVampire = false
                                }
                            }
                            return@executes 1
                        })
                    )
                    .then(literal("blood")
                        .requires(Permissions.require("haema.command.blood", 2))
                        .then(literal("get").then(argument("target", EntityArgumentType.entity()).executes { context ->
                            val target = EntityArgumentType.getEntity(context, "target")
                            if (target !is LivingEntity || !target.isVampire) {
                                context.source.sendError(Text.translatable("command.haema.error.not_vampire", target.name))
                                return@executes 0
                            }

                            val blood = target.vampireComponent.blood

                            context.source.sendFeedback(Text.translatable("command.haema.blood.get.feedback", target.name, blood), false)
                            return@executes blood.toInt()
                        }))
                        .then(literal("set").then(argument("targets", EntityArgumentType.entities()).then(argument("value", DoubleArgumentType.doubleArg(0.0, 20.0)).executes { context ->
                            val targets = EntityArgumentType.getEntities(context, "targets")
                            val value = DoubleArgumentType.getDouble(context, "value")

                            targets.forEach { target ->
                                if (target !is LivingEntity || !target.isVampire) {
                                    context.source.sendError(Text.translatable("command.haema.error.not_vampire", target.name))
                                } else {
                                    target.vampireComponent.absoluteBlood = value
                                    context.source.sendFeedback(Text.translatable("command.haema.blood.set.feedback", target.name, target.vampireComponent.blood), true)
                                }
                            }

                            return@executes targets.size
                        })))
                        .then(literal("add").then(argument("targets", EntityArgumentType.entities()).then(argument("value", DoubleArgumentType.doubleArg(-20.0, 20.0)).executes { context ->
                            val targets = EntityArgumentType.getEntities(context, "targets")
                            val value = DoubleArgumentType.getDouble(context, "value")

                            targets.forEach { target ->
                                if (target !is LivingEntity || !target.isVampire) {
                                    context.source.sendError(Text.translatable("command.haema.error.not_vampire", target.name))
                                } else {
                                    target.vampireComponent.addBlood(value)
                                    context.source.sendFeedback(Text.translatable("command.haema.blood.set.feedback", target.name, target.vampireComponent.blood), true)
                                }
                            }

                            return@executes targets.size
                        })))
                        .then(literal("remove").then(argument("targets", EntityArgumentType.entities()).then(argument("value", DoubleArgumentType.doubleArg(-20.0, 20.0)).executes { context ->
                            val targets = EntityArgumentType.getEntities(context, "targets")
                            val value = DoubleArgumentType.getDouble(context, "value")

                            targets.forEach { target ->
                                if (target !is LivingEntity || !target.isVampire) {
                                    context.source.sendError(Text.translatable("command.haema.error.not_vampire", target.name))
                                } else {
                                    target.vampireComponent.removeBlood(value)
                                    context.source.sendFeedback(Text.translatable("command.haema.blood.set.feedback", target.name, target.vampireComponent.blood), true)
                                }
                            }

                            return@executes targets.size
                        })))
                    )
                    .then(literal("abilities")
                        .requires(Permissions.require("haema.command.abilities", 2))
                        .then(literal("get").then(argument("targets", EntityArgumentType.players()).executes {  context ->
                            EntityArgumentType.getPlayers(context, "targets").forEach {
                                if ((it).isVampire) {
                                    context.source.sendFeedback(it.name.copy().append(" has abilities:"), false)
                                    AbilityModule.ABILITY_REGISTRY.entrySet.forEach { (key, ability) ->
                                        context.source.sendFeedback(Text.translatable("ability.${key.value.namespace}.${key.value.path}").append(": ${it.getAbilityLevel(ability)}"), false)
                                    }
                                }
                            }
                            return@executes 1
                        }))
                        .then(literal("set").then(argument("targets", EntityArgumentType.players()).then(argument("ability", VampireAbilityArgumentType.ability()).then(
                            argument("level", IntegerArgumentType.integer(0)).executes { context ->
                                EntityArgumentType.getPlayers(context, "targets").forEach {
                                    if ((it).isVampire) {
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
                                if ((it).isVampire) {
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
                                if ((it).isVampire) {
                                    it.setHasUsedRitual(IdentifierArgumentType.getIdentifier(context, "ritual"), true)
                                }
                            }
                            return@executes 1
                        })))
                        .then(literal("remove").then(argument("targets", EntityArgumentType.players()).then(argument("ritual", IdentifierArgumentType.identifier()).executes { context ->
                            EntityArgumentType.getPlayers(context, "targets").forEach {
                                if ((it).isVampire) {
                                    it.setHasUsedRitual(IdentifierArgumentType.getIdentifier(context, "ritual"), false)
                                }
                            }
                            return@executes 1
                        })))
                    )
            )
        }

        LOGGER.info("Everything registered. It's vampire time!")
    }

    override fun registerEntityComponentFactories(registry: EntityComponentFactoryRegistry) {
        registry.registerForPlayers(VampireComponent.entityKey, { player ->  EntityVampireComponent(player) }, RespawnCopyStrategy.ALWAYS_COPY)
    }
}