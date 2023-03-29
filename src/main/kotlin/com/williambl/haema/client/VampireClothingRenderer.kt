package com.williambl.haema.client

import com.williambl.haema.drip.VampireClothingItem
import com.williambl.haema.id
import net.minecraft.util.Identifier
import software.bernie.geckolib.model.GeoModel
import software.bernie.geckolib.renderer.GeoArmorRenderer

class VampireClothingRenderer: GeoArmorRenderer<VampireClothingItem>(Model()) {
    class Model: GeoModel<VampireClothingItem>() {
        override fun getModelResource(`object`: VampireClothingItem?): Identifier = id("geo/vampire_clothing.geo.json")

        override fun getTextureResource(`object`: VampireClothingItem?): Identifier = id("textures/armour/vampire_clothing.png")

        override fun getAnimationResource(animatable: VampireClothingItem?): Identifier = id("animations/vampire_clothing.animation.json")
    }
}