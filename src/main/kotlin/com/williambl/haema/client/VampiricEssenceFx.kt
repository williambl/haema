package com.williambl.haema.client

import com.mojang.blaze3d.platform.GlStateManager
import com.mojang.blaze3d.systems.RenderSystem
import com.williambl.haema.id
import com.williambl.haema.vampiremobs.VampireMobsModule
import ladysnake.satin.api.event.ShaderEffectRenderCallback
import ladysnake.satin.api.managed.ManagedFramebuffer
import ladysnake.satin.api.managed.ManagedShaderEffect
import ladysnake.satin.api.managed.ShaderEffectManager
import ladysnake.satin.api.util.RenderLayerHelper
import net.fabricmc.fabric.api.blockrenderlayer.v1.BlockRenderLayerMap
import net.minecraft.client.MinecraftClient
import net.minecraft.client.render.RenderLayer


//TODO: world-space
//TODO: animate
//TODO: depth?!
object VampiricEssenceFx {
    val effect: ManagedShaderEffect = ShaderEffectManager.getInstance().manage(id("shaders/post/vampiric_essence.json"))
    val buffer: ManagedFramebuffer = effect.getTarget("final")

    fun init() {
        val blockRenderLayer: RenderLayer = buffer.getRenderLayer(RenderLayer.getTranslucent())
        RenderLayerHelper.registerBlockRenderLayer(blockRenderLayer)
        BlockRenderLayerMap.INSTANCE.putBlock(VampireMobsModule.VAMPIRIC_ESSENCE, blockRenderLayer)

        ShaderEffectRenderCallback.EVENT.register { tickDelta: Float ->
            val client: MinecraftClient = MinecraftClient.getInstance()
            effect.render(tickDelta)
            client.framebuffer.beginWrite(true)
            RenderSystem.enableBlend()
            RenderSystem.blendFuncSeparate(
                GlStateManager.SrcFactor.SRC_ALPHA,
                GlStateManager.DstFactor.ONE_MINUS_SRC_ALPHA,
                GlStateManager.SrcFactor.ZERO,
                GlStateManager.DstFactor.ONE
            )
            buffer.draw(
                client.window.framebufferWidth,
                client.window.framebufferHeight,
                false
            )
            buffer.clear()
            client.framebuffer.beginWrite(true)
            RenderSystem.disableBlend()
        }
    }
}