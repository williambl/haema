package com.williambl.haema.vampire.ability.powers.reinforcements;

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes;
import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.actions.Action;
import com.williambl.actions.Actions;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.dfunc.api.DTypes;
import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.haema.api.vampire.ability.powers.drinking.EntityDrinkTargetCallback;
import com.williambl.haema.content.blood.VampireBackedBloodStorage;
import com.williambl.haema.vampire_mobs.HaemaVampireMobs;
import com.williambl.haema.vampire_mobs.VampiricZombie;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.lang.VValue;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.transaction.Transaction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.projectile.ProjectileUtil;
import net.minecraft.world.phys.EntityHitResult;

import java.util.List;
import java.util.Optional;

public record SpawnReinforcementsAbilityPower(VExpression amountToSpawn, VExpression canSpawn, VExpression onSpawn, VExpression onSpawnIndividual, List<String> keybinds) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<SpawnReinforcementsAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY).fieldOf("amount_to_spawn").forGetter(SpawnReinforcementsAbilityPower::amountToSpawn),
            DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).fieldOf("can_spawn").forGetter(SpawnReinforcementsAbilityPower::canSpawn),
            DFunctions.resolvedExpressionCodec(StandardVTypes.LIST.with(0, Actions.ACTION_TYPE.get()), DFunctions.ENTITY).fieldOf("on_spawn").forGetter(SpawnReinforcementsAbilityPower::onSpawn),
            DFunctions.resolvedExpressionCodec(StandardVTypes.LIST.with(0, Actions.ACTION_TYPE.get()), DFunctions.ENTITY_TARGET).fieldOf("on_spawn_individual").forGetter(SpawnReinforcementsAbilityPower::onSpawnIndividual),
            Codec.STRING.listOf().fieldOf("keybinds").forGetter(SpawnReinforcementsAbilityPower::keybinds)
    ).apply(instance, SpawnReinforcementsAbilityPower::new)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source, boolean isActive) {
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    public boolean trySpawnReinforcements(LivingEntity entity, ServerLevel level) {
        var ctx = DFunctions.createEntityContext(entity);
        if (!DFunctions.<Boolean>evaluate(this.canSpawn(), ctx)) {
            return false;
        }

        int amountToSpawn = (int) (double) DFunctions.<Double>evaluate(this.amountToSpawn(), ctx);
        for (int i = 0; i < amountToSpawn; i++) {
            var spawned = HaemaVampireMobs.VampireMobEntityTypes.VAMPIRIC_ZOMBIE.spawn(level, entity.blockPosition(), MobSpawnType.REINFORCEMENT);
            spawned.setOwner(entity);
            var ctx2 = DFunctions.createEntityTargetContext(entity, spawned);
            DFunctions.<List<Action>>evaluate(this.onSpawnIndividual(), ctx2)
                    .forEach(Action::runAction);
        }

        DFunctions.<List<VValue>>evaluate(this.onSpawn(), ctx).stream()
                .map(VValue::<Action>getUnchecked)
                .forEach(Action::runAction);

        return true;
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }
}
