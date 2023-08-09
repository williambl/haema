package com.williambl.haema.vampire.ability.powers.dash;

import com.mojang.brigadier.Command;
import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.minecraft.core.Direction;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.ClipContext;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.Shapes;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static net.minecraft.commands.Commands.literal;

public record DashAbilityPower(VExpression cooldown, VExpression canDash, List<String> keybinds) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<DashAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY).fieldOf("cooldown").forGetter(DashAbilityPower::cooldown),
            DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).fieldOf("can_dash").forGetter(DashAbilityPower::canDash),
            Codec.STRING.listOf().fieldOf("keybinds").forGetter(DashAbilityPower::keybinds)
    ).apply(instance, DashAbilityPower::new)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    public void dash(LivingEntity entity) {
        if (!DFunctions.<Boolean>evaluate(this.canDash(), DFunctions.createEntityContext(entity))) {
            return;
        }

        var target = raytraceForDash(entity);
        if (target == null) {
            return;
        }

        //TODO effects

        entity.teleportTo(target.x, target.y, target.z);
    }

    public void dashWithTarget(LivingEntity entity, Vec3 target) {
        if (!DFunctions.<Boolean>evaluate(this.canDash(), DFunctions.createEntityContext(entity))) {
            return;
        }

        //TODO effects

        entity.teleportTo(target.x, target.y, target.z);
    }

    private @Nullable Vec3 raytraceForDash(LivingEntity entity) {
        var eyes = entity.getEyePosition(0f);
        var dir = entity.getLookAngle();
        var rayEnd = eyes.add(dir.x * 16, dir.y * 16, dir.z * 16);
        var result = entity.level().clip(new ClipContext(eyes, rayEnd, ClipContext.Block.COLLIDER, ClipContext.Fluid.NONE, entity));
        var dashPos = Vec3.atBottomCenterOf(result.getDirection() == Direction.DOWN ? result.getBlockPos().below(2) : result.getBlockPos().relative(result.getDirection()));
        var width = entity.getBbWidth();
        var height = entity.getBbHeight();
        var freePos = entity.level().findFreePosition(
                entity,
                Shapes.create(AABB.ofSize(dashPos, width, 0, width).expandTowards(0.0, 1.0, 0.0).inflate(1.0E-6)),
                dashPos,
                width,
                height,
                width);
        return freePos.orElse(null);
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }

    public static void init() {
        CommandRegistrationCallback.EVENT.register((dispatcher, reg, env) -> {
            dispatcher.register(literal("dash")
                    .executes(context -> {
                        var player = context.getSource().getPlayerOrException();
                        var power = new DashAbilityPower(VExpression.value(StandardVTypes.NUMBER, (double) 0.0), VExpression.value(StandardVTypes.BOOLEAN, true), List.of());
                        power.dash(player);
                        return Command.SINGLE_SUCCESS;
                    }));
        });
    }
}
