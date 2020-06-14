package com.williambl.haema.client

import com.mojang.blaze3d.systems.RenderSystem
import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import ladysnake.satin.api.event.ShaderEffectRenderCallback
import ladysnake.satin.api.managed.ManagedShaderEffect
import ladysnake.satin.api.managed.ShaderEffectManager
import ladysnake.satin.api.managed.uniform.Uniform1f
import net.fabricmc.fabric.api.network.ClientSidePacketRegistry
import net.minecraft.client.MinecraftClient
import net.minecraft.util.Identifier
import kotlin.math.sin


val VAMPIRE_SHADER: ManagedShaderEffect = ShaderEffectManager.getInstance()
        .manage(Identifier("haema", "shaders/post/vampirevision.json"))


fun init() {
    ClientSidePacketRegistry.INSTANCE.register(Identifier("haema:bloodlevelsync")) { packetContext, packetByteBuf ->
        (packetContext.player as Vampirable).checkBloodManager()
        (packetContext.player.hungerManager as VampireBloodManager).absoluteBloodLevel = packetByteBuf.readDouble()
    }
    ClientSidePacketRegistry.INSTANCE.register(Identifier("haema:vampiresync")) { packetContext, packetByteBuf ->
        (packetContext.player as Vampirable).isVampire = packetByteBuf.readBoolean()
    }
    ShaderEffectRenderCallback.EVENT.register(ShaderEffectRenderCallback {
        if ((MinecraftClient.getInstance().player as Vampirable).isVampire)
            VAMPIRE_SHADER.render(it)
        RenderSystem.enableTexture()
    })
}