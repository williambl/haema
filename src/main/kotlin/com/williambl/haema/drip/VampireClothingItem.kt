package com.williambl.haema.drip

import net.minecraft.entity.EquipmentSlot
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ArmorItem
import net.minecraft.item.ArmorMaterial
import software.bernie.geckolib3.core.IAnimatable
import software.bernie.geckolib3.core.PlayState
import software.bernie.geckolib3.core.builder.AnimationBuilder
import software.bernie.geckolib3.core.controller.AnimationController
import software.bernie.geckolib3.core.event.predicate.AnimationEvent
import software.bernie.geckolib3.core.manager.AnimationData
import software.bernie.geckolib3.core.manager.AnimationFactory

class VampireClothingItem(materialIn: ArmorMaterial, slot: EquipmentSlot, builder: Settings) : ArmorItem(materialIn, slot, builder), IAnimatable {
    private val factory = AnimationFactory(this)

    private fun <P : IAnimatable?> predicate(event: AnimationEvent<P>): PlayState {
        val player = event.getExtraDataOfType(LivingEntity::class.java)[0]
        if (player is PlayerEntity) {
            if (!(player.lastLimbDistance > -0.15f && player.lastLimbDistance < 0.15f)) {
                event.controller.setAnimation(AnimationBuilder().addAnimation("animation.model.walk", true))
                return PlayState.CONTINUE
            }
        }
        event.controller.setAnimation(AnimationBuilder().addAnimation("animation.model.idle", true))
        return PlayState.CONTINUE
    }

    override fun registerControllers(data: AnimationData) {
        data.addAnimationController(AnimationController(this, "controller", 20f, ::predicate))
    }

    override fun getFactory(): AnimationFactory {
        return factory
    }
}