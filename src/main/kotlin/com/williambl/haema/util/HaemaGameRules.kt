package com.williambl.haema.util

import com.williambl.haema.ability.component.dash.DashAbilityComponent
import com.williambl.haema.id
import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerEntityEvents
import net.fabricmc.fabric.api.gamerule.v1.CustomGameRuleCategory
import net.fabricmc.fabric.api.gamerule.v1.GameRuleFactory
import net.fabricmc.fabric.api.gamerule.v1.GameRuleRegistry
import net.fabricmc.fabric.api.gamerule.v1.rule.DoubleRule
import net.fabricmc.fabric.api.networking.v1.PlayerLookup
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.world.GameRules

object HaemaGameRules {
    val haemaCategory = CustomGameRuleCategory(
        id("haema"), TranslatableText("gamerule.category.haema").formatted(
            Formatting.BOLD
        ).formatted(Formatting.YELLOW)
    )

    val vampiresBurn: GameRules.Key<GameRules.BooleanRule> = GameRuleRegistry.register(
        "vampiresBurn",
        haemaCategory,
        GameRuleFactory.createBooleanRule(true)
    )

    val vampiresDrown: GameRules.Key<GameRules.BooleanRule> = GameRuleRegistry.register(
        "vampiresDrown",
        haemaCategory,
        GameRuleFactory.createBooleanRule(true)
    )

    val feedCooldown: GameRules.Key<GameRules.IntRule> = GameRuleRegistry.register(
        "feedCooldown",
        haemaCategory,
        GameRuleFactory.createIntRule(10, 0, 24000)
    )

    val dashCooldown: GameRules.Key<GameRules.IntRule> = GameRuleRegistry.register(
        "dashCooldown",
        haemaCategory,
        GameRuleFactory.createIntRule(10, 0, 24000) { server, rule ->
            PlayerLookup.all(server).forEach { DashAbilityComponent.entityKey.get(it).updateDashCooldown(rule.get()) }
        })

    val invisLength: GameRules.Key<GameRules.IntRule> = GameRuleRegistry.register(
        "vampireInvisibilityLength",
        haemaCategory,
        GameRuleFactory.createIntRule(80, 0, 24000) { server, rule ->
            val buf = PacketByteBuf(Unpooled.buffer())
            buf.writeInt(rule.get())
            server.playerManager.sendToAll(
                ServerPlayNetworking.createS2CPacket(
                    id("updateinvislength"),
                    buf
                )
            )
        })

    val vampireHunterNoticeChance: GameRules.Key<DoubleRule> = GameRuleRegistry.register(
        "vampireHunterNoticeChance",
        GameRules.Category.MOBS,
        GameRuleFactory.createDoubleRule(0.1, 0.0, 1.0)
    )

    val playerVampireConversion: GameRules.Key<GameRules.BooleanRule> = GameRuleRegistry.register(
        "playerVampireConversion",
        haemaCategory,
        GameRuleFactory.createBooleanRule(true)
    )

    val sunlightDamagesArmour: GameRules.Key<GameRules.BooleanRule> = GameRuleRegistry.register(
        "sunlightDamagesArmour",
        haemaCategory,
        GameRuleFactory.createBooleanRule(true)
    )

    fun registerGameRules() {
        ServerEntityEvents.ENTITY_LOAD.register(ServerEntityEvents.Load { entity, serverWorld ->
            if (entity is PlayerEntity) {
                val buf = PacketByteBuf(Unpooled.buffer())
                buf.writeInt(serverWorld.gameRules.get(dashCooldown).get())
                ServerPlayNetworking.send(entity as ServerPlayerEntity, id("updatedashcooldown"), buf)
            }
        })

        ServerEntityEvents.ENTITY_LOAD.register(ServerEntityEvents.Load { entity, serverWorld ->
            if (entity is ServerPlayerEntity) {
                val buf = PacketByteBuf(Unpooled.buffer())
                buf.writeInt(serverWorld.gameRules.get(invisLength).get())
                ServerPlayNetworking.send(entity, id("updateinvislength"), buf)
            }
        })
    }
}