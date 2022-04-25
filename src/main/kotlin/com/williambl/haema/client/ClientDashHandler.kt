package com.williambl.haema.client

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.component.dash.DashAbilityComponent
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.id
import com.williambl.haema.util.raytraceForDash
import com.williambl.haema.vampireComponent
import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking
import net.minecraft.client.MinecraftClient
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.util.math.Vec3f
import kotlin.math.abs
import kotlin.math.ln
import kotlin.math.max

object ClientDashHandler: ClientTickEvents.StartTick {
    private var pressedTicks = 0

    override fun onStartTick(client: MinecraftClient) {
        val player = client.player ?: return
        val world = player.world
        if (pressedTicks > 0 && !HaemaClient.DASH_KEY.isPressed && this.canDash(player)) {
            val buf = PacketByteBuf(Unpooled.buffer())
            ClientPlayNetworking.send(id("dash"), buf)
        } else if (HaemaClient.DASH_KEY.isPressed && this.canDash(player)) {
            val target = raytraceForDash(player)
            if (target != null) for (i in 0..9) {
                world.addParticle(
                    DustParticleEffect(Vec3f(0f, 0f, 0f), 1f),
                    target.x - 0.5 + world.random.nextDouble(),
                    target.y + world.random.nextDouble() * 2,
                    target.z - 0.5 + world.random.nextDouble(),
                    0.0,
                    0.5,
                    0.0
                )
            }
        }
        pressedTicks = if (HaemaClient.DASH_KEY.isPressed) pressedTicks + 1 else 0
        val timeSinceDash: Long = world.time - DashAbilityComponent.entityKey.get(player).lastDashed
        val distortAmount = HaemaClient.distortAmount
        if (pressedTicks > 0 && this.canDash(player)) {
            HaemaClient.distortAmount = max(HaemaClient.distortAmount - 0.05f, -0.2f)
        } else if (timeSinceDash <= 8) {
            if (timeSinceDash == 0L) HaemaClient.distortAmount = -1.4f else HaemaClient.distortAmount =
                -0.25f + 0.25f * ln((timeSinceDash / 3f).toDouble()).toFloat()
        } else if (distortAmount != 0f) {
            if (abs(distortAmount) < 0.1) {
                HaemaClient.distortAmount = 0f
            } else {
                HaemaClient.distortAmount = distortAmount - Math.copySign(0.1f, distortAmount)
            }
        }
    }

    fun couldDash(player: PlayerEntity): Boolean {
        val abilityLevel: Int = player.getAbilityLevel(AbilityModule.DASH)
        return abilityLevel > 0 && player.vampireComponent.blood >= 18f
    }

    fun canDash(player: PlayerEntity): Boolean = DashAbilityComponent.entityKey.get(player).canDash()
}