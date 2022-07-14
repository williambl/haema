package com.williambl.haema.client

import com.williambl.haema.id
import com.williambl.haema.vampiremobs.VampireMobsModule
import ladysnake.satin.api.event.PostWorldRenderCallbackV2
import ladysnake.satin.api.managed.ManagedCoreShader
import ladysnake.satin.api.managed.ShaderEffectManager
import ladysnake.satin.api.managed.uniform.Uniform4f
import ladysnake.satin.api.util.RenderLayerHelper
import net.fabricmc.fabric.api.blockrenderlayer.v1.BlockRenderLayerMap
import net.minecraft.client.render.Camera
import net.minecraft.client.render.RenderLayer
import net.minecraft.client.util.math.MatrixStack


//TODO: animate
//TODO: fix colour
object VampiricEssenceFx : PostWorldRenderCallbackV2 {
    val effect: ManagedCoreShader = ShaderEffectManager.getInstance().manageCoreShader(id("vampiric_essence"))
    private val colorModulator: Uniform4f = effect.findUniform4f("ColorModulator")

    fun init() {
        val blockRenderLayer: RenderLayer = effect.getRenderLayer(RenderLayer.getTranslucent())
        RenderLayerHelper.registerBlockRenderLayer(blockRenderLayer)
        BlockRenderLayerMap.INSTANCE.putBlock(VampireMobsModule.VAMPIRIC_ESSENCE, blockRenderLayer)

        PostWorldRenderCallbackV2.EVENT.register(this)
    }

    override fun onWorldRendered(matrices: MatrixStack, camera: Camera, tickDelta: Float, nanoTime: Long) {
        colorModulator.set(1.0f, 0.4f, 0.5f, 0.7f)
    }
}