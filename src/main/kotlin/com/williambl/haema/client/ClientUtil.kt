package com.williambl.haema.client

import net.minecraft.client.Minecraft
import net.minecraft.util.ResourceLocation

fun loadEnhancedVisionShader() {
    Minecraft.getMinecraft().entityRenderer.stopUseShader()
    Minecraft.getMinecraft().entityRenderer.loadShader(ResourceLocation("shaders/post/enhanced_vision.json"))
}

fun unloadShader() {
    Minecraft.getMinecraft().entityRenderer.stopUseShader()
}
