package com.williambl.haema.util

import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerEntityEvents
import net.fabricmc.fabric.api.gamerule.v1.CustomGameRuleCategory
import net.fabricmc.fabric.api.gamerule.v1.GameRuleFactory
import net.fabricmc.fabric.api.gamerule.v1.GameRuleRegistry
import net.fabricmc.fabric.api.gamerule.v1.rule.DoubleRule
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.util.Identifier
import net.minecraft.world.GameRules

lateinit var vampiresBurn: GameRules.Key<GameRules.BooleanRule>

lateinit var vampiresDrown: GameRules.Key<GameRules.BooleanRule>

lateinit var feedCooldown: GameRules.Key<GameRules.IntRule>

lateinit var dashCooldown: GameRules.Key<GameRules.IntRule>

lateinit var vampireHunterNoticeChance: GameRules.Key<DoubleRule>

lateinit var playerVampireConversion: GameRules.Key<GameRules.BooleanRule>

val haemaCategory = CustomGameRuleCategory(Identifier("haema:haema"), TranslatableText("gamerule.category.haema").formatted(
    Formatting.BOLD).formatted(Formatting.YELLOW))

fun registerGameRules() {
    vampiresBurn = GameRuleRegistry.register("vampiresBurn", GameRules.Category.PLAYER, GameRuleFactory.createBooleanRule(true))
    vampiresDrown = GameRuleRegistry.register("vampiresDrown", GameRules.Category.PLAYER, GameRuleFactory.createBooleanRule(true))
    feedCooldown = GameRuleRegistry.register("feedCooldown", GameRules.Category.PLAYER, GameRuleFactory.createIntRule(10, 0, 24000))

    dashCooldown = GameRuleRegistry.register("dashCooldown", GameRules.Category.PLAYER, GameRuleFactory.createIntRule(10, 0, 24000) { server, rule ->
        val buf = PacketByteBuf(Unpooled.buffer())
        buf.writeInt(rule.get())
        server.playerManager.sendToAll(ServerPlayNetworking.createS2CPacket(Identifier("haema:updatedashcooldown"), buf))
    })
    ServerEntityEvents.ENTITY_LOAD.register(ServerEntityEvents.Load { entity, serverWorld ->
        if (entity is PlayerEntity) {
            val buf = PacketByteBuf(Unpooled.buffer())
            buf.writeInt(serverWorld.gameRules.get(dashCooldown).get())
            ServerPlayNetworking.send(entity as ServerPlayerEntity, Identifier("haema:updatedashcooldown"), buf)
        }
    })

    vampireHunterNoticeChance = GameRuleRegistry.register("vampireHunterNoticeChance", GameRules.Category.MOBS, GameRuleFactory.createDoubleRule(0.1, 0.0, 1.0))
    playerVampireConversion = GameRuleRegistry.register("playerVampireConversion", GameRules.Category.PLAYER, GameRuleFactory.createBooleanRule(true))
}