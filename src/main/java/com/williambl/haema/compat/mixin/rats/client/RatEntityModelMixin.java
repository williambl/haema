package com.williambl.haema.compat.mixin.rats.client;

import com.williambl.haema.VampirableKt;
import ladysnake.ratsmischief.client.model.RatEntityModel;
import ladysnake.ratsmischief.common.entity.RatEntity;
import net.minecraft.util.Identifier;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import software.bernie.geckolib3.model.provider.GeoModelProvider;

import static com.williambl.haema.HaemaKt.id;

@Mixin(RatEntityModel.class)
public abstract class RatEntityModelMixin extends GeoModelProvider<RatEntity> {
    private static final Identifier VAMPIRAT_TEXTURE = id("textures/entity/vampirat.png");

    @Inject(method = "getTextureLocation(Lladysnake/ratsmischief/common/entity/RatEntity;)Lnet/minecraft/util/Identifier;", at=@At("HEAD"), cancellable = true)
    void useVampireTexture(RatEntity rat, CallbackInfoReturnable<Identifier> cir) {
        if (VampirableKt.isVampire(rat)) {
            cir.setReturnValue(VAMPIRAT_TEXTURE);
        }
    }
}
