package com.williambl.haema.client

import com.google.common.collect.ImmutableList
import com.williambl.haema.hunter.VampireHunterEntity
import net.fabricmc.api.EnvType
import net.fabricmc.api.Environment
import net.minecraft.client.model.ModelPart
import net.minecraft.client.render.entity.EntityRenderDispatcher
import net.minecraft.client.render.entity.MobEntityRenderer
import net.minecraft.client.render.entity.feature.HeadFeatureRenderer
import net.minecraft.client.render.entity.feature.HeldItemFeatureRenderer
import net.minecraft.client.render.entity.model.CompositeEntityModel
import net.minecraft.client.render.entity.model.CrossbowPosing
import net.minecraft.client.render.entity.model.ModelWithArms
import net.minecraft.client.render.entity.model.ModelWithHead
import net.minecraft.client.util.math.MatrixStack
import net.minecraft.item.Items
import net.minecraft.util.Arm
import net.minecraft.util.Identifier
import net.minecraft.util.math.MathHelper

@Environment(EnvType.CLIENT)
class VampireHunterEntityRenderer(entityRenderDispatcher: EntityRenderDispatcher) : MobEntityRenderer<VampireHunterEntity, VampireHunterModel>(entityRenderDispatcher, VampireHunterModel(0f, 0f, 64, 64), 0.5f) {
    private val texture = Identifier("textures/entity/illager/pillager.png")

    init {
        addFeature(HeadFeatureRenderer(this))
        addFeature(HeldItemFeatureRenderer(this))
    }

    override fun scale(entity: VampireHunterEntity, matrixStack: MatrixStack, f: Float) {
        matrixStack.scale(0.9375f, 0.9375f, 0.9375f)
    }

    override fun getTexture(entity: VampireHunterEntity?): Identifier = texture
}

@Environment(EnvType.CLIENT)
class VampireHunterModel(scale: Float, pivotY: Float, textureWidth: Int, textureHeight: Int) : CompositeEntityModel<VampireHunterEntity>(), ModelWithArms, ModelWithHead {
    private val head: ModelPart = ModelPart(this).setTextureSize(textureWidth, textureHeight)
    private val hat: ModelPart
    private val torso: ModelPart
    private val arms: ModelPart
    private val rightLeg: ModelPart
    private val leftLeg: ModelPart
    private val rightAttackingArm: ModelPart
    private val leftAttackingArm: ModelPart

    override fun getParts(): Iterable<ModelPart> {
        return ImmutableList.of(head, torso, rightLeg, leftLeg, arms, rightAttackingArm, leftAttackingArm)
    }

    override fun setAngles(entity: VampireHunterEntity, f: Float, g: Float, h: Float, i: Float, j: Float) {
        head.yaw = i * 0.017453292f
        head.pitch = j * 0.017453292f
        arms.pivotY = 3.0f
        arms.pivotZ = -1.0f
        arms.pitch = -0.75f
        if (riding) {
            rightAttackingArm.pitch = -0.62831855f
            rightAttackingArm.yaw = 0.0f
            rightAttackingArm.roll = 0.0f
            leftAttackingArm.pitch = -0.62831855f
            leftAttackingArm.yaw = 0.0f
            leftAttackingArm.roll = 0.0f
            rightLeg.pitch = -1.4137167f
            rightLeg.yaw = 0.31415927f
            rightLeg.roll = 0.07853982f
            leftLeg.pitch = -1.4137167f
            leftLeg.yaw = -0.31415927f
            leftLeg.roll = -0.07853982f
        } else {
            rightAttackingArm.pitch = MathHelper.cos(f * 0.6662f + 3.1415927f) * 2.0f * g * 0.5f
            rightAttackingArm.yaw = 0.0f
            rightAttackingArm.roll = 0.0f
            leftAttackingArm.pitch = MathHelper.cos(f * 0.6662f) * 2.0f * g * 0.5f
            leftAttackingArm.yaw = 0.0f
            leftAttackingArm.roll = 0.0f
            rightLeg.pitch = MathHelper.cos(f * 0.6662f) * 1.4f * g * 0.5f
            rightLeg.yaw = 0.0f
            rightLeg.roll = 0.0f
            leftLeg.pitch = MathHelper.cos(f * 0.6662f + 3.1415927f) * 1.4f * g * 0.5f
            leftLeg.yaw = 0.0f
            leftLeg.roll = 0.0f
        }
        if (entity.isHolding(Items.CROSSBOW)) {
            if (entity.isCharging()) {
                CrossbowPosing.charge(rightAttackingArm, leftAttackingArm, entity, true)
            }
            else {
                CrossbowPosing.hold(rightAttackingArm, leftAttackingArm, head, true)
            }
        } else if (entity.isAttacking) {
            // hold arm up in the air
            CrossbowPosing.method_29351(rightAttackingArm, leftAttackingArm, entity, handSwingProgress, h)
        }
    }

    private fun getArm(arm: Arm): ModelPart {
        return if (arm == Arm.LEFT) leftAttackingArm else rightAttackingArm
    }

    override fun getHead(): ModelPart {
        return head
    }

    override fun setArmAngle(arm: Arm, matrices: MatrixStack) {
        getArm(arm).rotate(matrices)
    }

    init {
        head.setPivot(0.0f, 0.0f + pivotY, 0.0f)
        head.setTextureOffset(0, 0).addCuboid(-4.0f, -10.0f, -4.0f, 8.0f, 10.0f, 8.0f, scale)
        hat = ModelPart(this, 32, 0).setTextureSize(textureWidth, textureHeight)
        hat.addCuboid(-4.0f, -10.0f, -4.0f, 8.0f, 12.0f, 8.0f, scale + 0.45f)
        head.addChild(hat)
        hat.visible = false
        val modelPart = ModelPart(this).setTextureSize(textureWidth, textureHeight)
        modelPart.setPivot(0.0f, pivotY - 2.0f, 0.0f)
        modelPart.setTextureOffset(24, 0).addCuboid(-1.0f, -1.0f, -6.0f, 2.0f, 4.0f, 2.0f, scale)
        head.addChild(modelPart)
        torso = ModelPart(this).setTextureSize(textureWidth, textureHeight)
        torso.setPivot(0.0f, 0.0f + pivotY, 0.0f)
        torso.setTextureOffset(16, 20).addCuboid(-4.0f, 0.0f, -3.0f, 8.0f, 12.0f, 6.0f, scale)
        torso.setTextureOffset(0, 38).addCuboid(-4.0f, 0.0f, -3.0f, 8.0f, 18.0f, 6.0f, scale + 0.5f)
        arms = ModelPart(this).setTextureSize(textureWidth, textureHeight)
        arms.setPivot(0.0f, 0.0f + pivotY + 2.0f, 0.0f)
        arms.setTextureOffset(44, 22).addCuboid(-8.0f, -2.0f, -2.0f, 4.0f, 8.0f, 4.0f, scale)
        val modelPart2 = ModelPart(this, 44, 22).setTextureSize(textureWidth, textureHeight)
        modelPart2.mirror = true
        modelPart2.addCuboid(4.0f, -2.0f, -2.0f, 4.0f, 8.0f, 4.0f, scale)
        arms.addChild(modelPart2)
        arms.setTextureOffset(40, 38).addCuboid(-4.0f, 2.0f, -2.0f, 8.0f, 4.0f, 4.0f, scale)
        rightLeg = ModelPart(this, 0, 22).setTextureSize(textureWidth, textureHeight)
        rightLeg.setPivot(-2.0f, 12.0f + pivotY, 0.0f)
        rightLeg.addCuboid(-2.0f, 0.0f, -2.0f, 4.0f, 12.0f, 4.0f, scale)
        leftLeg = ModelPart(this, 0, 22).setTextureSize(textureWidth, textureHeight)
        leftLeg.mirror = true
        leftLeg.setPivot(2.0f, 12.0f + pivotY, 0.0f)
        leftLeg.addCuboid(-2.0f, 0.0f, -2.0f, 4.0f, 12.0f, 4.0f, scale)
        rightAttackingArm = ModelPart(this, 40, 46).setTextureSize(textureWidth, textureHeight)
        rightAttackingArm.addCuboid(-3.0f, -2.0f, -2.0f, 4.0f, 12.0f, 4.0f, scale)
        rightAttackingArm.setPivot(-5.0f, 2.0f + pivotY, 0.0f)
        leftAttackingArm = ModelPart(this, 40, 46).setTextureSize(textureWidth, textureHeight)
        leftAttackingArm.mirror = true
        leftAttackingArm.addCuboid(-1.0f, -2.0f, -2.0f, 4.0f, 12.0f, 4.0f, scale)
        leftAttackingArm.setPivot(5.0f, 2.0f + pivotY, 0.0f)
    }

}