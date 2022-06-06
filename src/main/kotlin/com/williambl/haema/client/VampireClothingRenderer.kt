package com.williambl.haema.client

import com.williambl.haema.drip.VampireClothingItem
import com.williambl.haema.id
import net.minecraft.util.Identifier
import software.bernie.geckolib3.model.AnimatedGeoModel
import software.bernie.geckolib3.renderers.geo.GeoArmorRenderer

class VampireClothingRenderer: GeoArmorRenderer<VampireClothingItem>(Model()) {
    init {
        this.bodyBone = "armorBody"
        this.rightArmBone = "armorRightArm"
        this.leftArmBone = "armorLeftArm"
        this.rightLegBone = "armorLeftLeg"
        this.leftLegBone = "armorRightLeg"
        this.rightBootBone = "armorLeftBoot"
        this.leftBootBone = "armorRightBoot"
    }
    class Model: AnimatedGeoModel<VampireClothingItem>() {
        override fun getModelLocation(`object`: VampireClothingItem?): Identifier = id("geo/vampire_clothing.geo.json")

        override fun getTextureLocation(`object`: VampireClothingItem?): Identifier = id("textures/armour/vampire_clothing.png")

        override fun getAnimationFileLocation(animatable: VampireClothingItem?): Identifier = id("animations/vampire_clothing.animation.json")
    }
}