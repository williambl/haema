package com.williambl.haema.drip

import com.williambl.haema.client.VampireClothingRenderer
import net.minecraft.client.render.entity.model.BipedEntityModel
import net.minecraft.entity.EquipmentSlot
import net.minecraft.entity.LivingEntity
import net.minecraft.item.ArmorItem
import net.minecraft.item.ArmorMaterial
import net.minecraft.item.ItemStack
import software.bernie.geckolib.animatable.GeoItem
import software.bernie.geckolib.animatable.client.RenderProvider
import software.bernie.geckolib.constant.DefaultAnimations
import software.bernie.geckolib.core.animatable.instance.AnimatableInstanceCache
import software.bernie.geckolib.core.animation.AnimatableManager
import software.bernie.geckolib.core.animation.AnimationController
import software.bernie.geckolib.core.animation.AnimationState
import software.bernie.geckolib.core.`object`.PlayState
import software.bernie.geckolib.renderer.GeoArmorRenderer
import software.bernie.geckolib.util.GeckoLibUtil
import java.util.function.Consumer
import java.util.function.Supplier

class VampireClothingItem(materialIn: ArmorMaterial, type: Type, builder: Settings) : ArmorItem(materialIn, type, builder), GeoItem {
    private val factory = GeckoLibUtil.createInstanceCache(this)
    private val renderProvider = GeoItem.makeRenderer(this)

    override fun registerControllers(controllers: AnimatableManager.ControllerRegistrar) {
        controllers.add(
            AnimationController(this, 20) { state: AnimationState<VampireClothingItem> ->
                state.controller.setAnimation(if (state.isMoving) DefaultAnimations.WALK else DefaultAnimations.IDLE)
                PlayState.CONTINUE
            }
        )
    }


    override fun getAnimatableInstanceCache(): AnimatableInstanceCache = this.factory

    override fun createRenderer(consumer: Consumer<Any>) {
        consumer.accept(object : RenderProvider {
            private var renderer: GeoArmorRenderer<*>? = null

            override fun getHumanoidArmorModel(
                livingEntity: LivingEntity?,
                itemStack: ItemStack?,
                equipmentSlot: EquipmentSlot?,
                original: BipedEntityModel<LivingEntity?>?
            ): BipedEntityModel<LivingEntity?>? {
                if (renderer == null) renderer = VampireClothingRenderer() // !!! clientside only !!!

                renderer!!.prepForRender(livingEntity, itemStack, equipmentSlot, original)
                return renderer
            }
        })
    }

    override fun getRenderProvider(): Supplier<Any> = this.renderProvider
}