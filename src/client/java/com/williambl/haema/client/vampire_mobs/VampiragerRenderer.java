package com.williambl.haema.client.vampire_mobs;

import com.mojang.blaze3d.vertex.PoseStack;
import com.williambl.haema.vampire_mobs.Vampirager;
import net.minecraft.client.model.AnimationUtils;
import net.minecraft.client.model.ArmedModel;
import net.minecraft.client.model.HeadedModel;
import net.minecraft.client.model.HierarchicalModel;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.PartNames;
import net.minecraft.client.model.geom.PartPose;
import net.minecraft.client.model.geom.builders.CubeDeformation;
import net.minecraft.client.model.geom.builders.CubeListBuilder;
import net.minecraft.client.model.geom.builders.LayerDefinition;
import net.minecraft.client.model.geom.builders.MeshDefinition;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.MobRenderer;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.item.Items;

import static com.williambl.haema.Haema.id;

public class VampiragerRenderer extends MobRenderer<Vampirager, VampiragerRenderer.VampiragerModel> {
    private static ResourceLocation TEXTURE = id("textures/entity/vampirager.png");

    public VampiragerRenderer(EntityRendererProvider.Context context) {
        super(context, new VampiragerModel(context.bakeLayer(VampiragerModel.LAYER)), 0.5f);
        this.addLayer(new CustomHeadLayer<>(this, context.getModelSet(), context.getItemInHandRenderer()));
        this.addLayer(new ItemInHandLayer<>(this, context.getItemInHandRenderer()));
    }

    @Override
    protected void scale(Vampirager livingEntity, PoseStack poseStack, float f) {
        poseStack.scale(0.9375f, 0.9375f, 0.9375f);
    }

    @Override
    public ResourceLocation getTextureLocation(Vampirager entity) {
        return TEXTURE;
    }

    static class VampiragerModel extends HierarchicalModel<Vampirager> implements ArmedModel, HeadedModel {
        public static final ModelLayerLocation LAYER = new ModelLayerLocation(id("vampirager"), "main");
        private final ModelPart root;
        private final ModelPart head;
        private final ModelPart hat;
        private final ModelPart arms;
        private final ModelPart leftLeg;
        private final ModelPart rightLeg;
        private final ModelPart rightAttackingArm;
        private final ModelPart leftAttackingArm;

        VampiragerModel(ModelPart root) {
            this.root = root;
            this.head = root.getChild(PartNames.HEAD);
            this.hat = this.head.getChild(PartNames.HAT);
            this.arms = root.getChild(PartNames.ARMS);
            this.leftLeg = root.getChild(PartNames.LEFT_LEG);
            this.rightLeg = root.getChild(PartNames.RIGHT_LEG);
            this.rightAttackingArm = root.getChild(PartNames.RIGHT_ARM);
            this.leftAttackingArm = root.getChild(PartNames.LEFT_ARM);
        }

        private ModelPart getArm(HumanoidArm arm) {
            return arm == HumanoidArm.LEFT ? this.leftAttackingArm : this.rightAttackingArm;
        }

        @Override
        public void translateToHand(HumanoidArm humanoidArm, PoseStack poseStack) {
            this.getArm(humanoidArm).translateAndRotate(poseStack);
        }

        @Override
        public ModelPart getHead() {
            return this.head;
        }

        @Override
        public ModelPart root() {
            return this.root;
        }

        @Override
        public void setupAnim(Vampirager entity, float f, float g, float h, float i, float j) {
            this.head.yRot = i * 0.017453292f;
            this.head.xRot = j * 0.017453292f;
            this.arms.y = 3.0f;
            this.arms.z = -1.0f;
            this.arms.xRot = -0.75f;
            if (this.riding) {
                this.rightAttackingArm.xRot = -0.62831855f;
                this.rightAttackingArm.yRot = 0.0f;
                this.rightAttackingArm.zRot = 0.0f;
                this.leftAttackingArm.xRot = -0.62831855f;
                this.leftAttackingArm.yRot = 0.0f;
                this.leftAttackingArm.zRot = 0.0f;
                this.rightLeg.xRot = -1.4137167f;
                this.rightLeg.yRot = 0.31415927f;
                this.rightLeg.zRot = 0.07853982f;
                this.leftLeg.xRot = -1.4137167f;
                this.leftLeg.yRot = -0.31415927f;
                this.leftLeg.zRot = -0.07853982f;
            } else {
                this.rightAttackingArm.xRot = Mth.cos(f * 0.6662f + 3.1415927f) * 2.0f * g * 0.5f;
                this.rightAttackingArm.yRot = 0.0f;
                this.rightAttackingArm.zRot = 0.0f;
                this.leftAttackingArm.xRot = Mth.cos(f * 0.6662f) * 2.0f * g * 0.5f;
                this.leftAttackingArm.yRot = 0.0f;
                this.leftAttackingArm.zRot = 0.0f;
                this.rightLeg.xRot = Mth.cos(f * 0.6662f) * 1.4f * g * 0.5f;
                this.rightLeg.yRot = 0.0f;
                this.rightLeg.zRot = 0.0f;
                this.leftLeg.xRot = Mth.cos(f * 0.6662f + 3.1415927f) * 1.4f * g * 0.5f;
                this.leftLeg.yRot = 0.0f;
                this.leftLeg.zRot = 0.0f;
            }
            if (entity.isHolding(Items.CROSSBOW)) {
                AnimationUtils.animateCrossbowHold(this.rightAttackingArm, this.leftAttackingArm, this.head, true);
            } else if (entity.isAggressive()) {
                // hold arm up in the air
                AnimationUtils.swingWeaponDown(this.rightAttackingArm, this.leftAttackingArm, entity, this.attackTime, h);
            }
        }

        public static LayerDefinition createBodyLayer() {
            var modelData = new MeshDefinition();
            var modelPartData = modelData.getRoot();
            var modelPartData2 = modelPartData.addOrReplaceChild(
                    PartNames.HEAD,
                    CubeListBuilder.create().texOffs(0, 0).addBox(-4.0f, -10.0f, -4.0f, 8.0f, 10.0f, 8.0f),
                    PartPose.offset(0.0f, 0.0f, 0.0f)
            );
            modelPartData2.addOrReplaceChild(
                    PartNames.HAT,
                    CubeListBuilder.create().texOffs(32, 0).addBox(-4.0f, -10.0f, -4.0f, 8.0f, 10.0f, 8.0f, new CubeDeformation(0.45f)),
                    PartPose.ZERO
            );
            modelPartData2.addOrReplaceChild(
                    PartNames.NOSE,
                    CubeListBuilder.create().texOffs(24, 0).addBox(-1.0f, -1.0f, -6.0f, 2.0f, 4.0f, 2.0f),
                    PartPose.offset(0.0f, -2.0f, 0.0f)
            );
            modelPartData.addOrReplaceChild(
                    PartNames.BODY,
                    CubeListBuilder.create().texOffs(16, 20).addBox(-4.0f, 0.0f, -3.0f, 8.0f, 12.0f, 6.0f).texOffs(0, 38)
                            .addBox(-4.0f, 0.0f, -3.0f, 8.0f, 18.0f, 6.0f, new CubeDeformation(0.5f)),
                    PartPose.offset(0.0f, 0.0f, 0.0f)
            );
            modelPartData.addOrReplaceChild(
                    PartNames.RIGHT_LEG,
                    CubeListBuilder.create().texOffs(0, 22).addBox(-2.0f, 0.0f, -2.0f, 4.0f, 12.0f, 4.0f),
                    PartPose.offset(-2.0f, 12.0f, 0.0f)
            );
            modelPartData.addOrReplaceChild(
                    PartNames.LEFT_LEG,
                    CubeListBuilder.create().texOffs(0, 22).mirror().addBox(-2.0f, 0.0f, -2.0f, 4.0f, 12.0f, 4.0f),
                    PartPose.offset(2.0f, 12.0f, 0.0f)
            );
            modelPartData.addOrReplaceChild(
                    PartNames.RIGHT_ARM,
                    CubeListBuilder.create().texOffs(40, 46).addBox(-3.0f, -2.0f, -2.0f, 4.0f, 12.0f, 4.0f),
                    PartPose.offset(-5.0f, 2.0f, 0.0f)
            );
            modelPartData.addOrReplaceChild(
                    PartNames.LEFT_ARM,
                    CubeListBuilder.create().texOffs(40, 46).mirror().addBox(-1.0f, -2.0f, -2.0f, 4.0f, 12.0f, 4.0f),
                    PartPose.offset(5.0f, 2.0f, 0.0f)
            );
            return LayerDefinition.create(modelData, 64, 64);
        }
    }
}
