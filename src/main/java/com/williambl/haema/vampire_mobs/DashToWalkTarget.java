package com.williambl.haema.vampire_mobs;

import com.mojang.datafixers.util.Pair;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.ability.powers.dash.DashAbilityPower;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.phys.Vec3;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;

import java.util.List;

public class DashToWalkTarget<E extends PathfinderMob> extends ExtendedBehaviour<E> {
    private Vec3 nodePos;
    private List<DashAbilityPower> powers;
    private double sqrMinDashDistance = 5*5;
    private double sqrMaxDashDistance = 16*16;

    public DashToWalkTarget<E> minDistanceToDash(double distance) {
        this.sqrMinDashDistance = distance*distance;
        return this;
    }

    public DashToWalkTarget<E> maxDistanceToDash(double distance) {
        this.sqrMaxDashDistance = distance*distance;
        return this;
    }

    @Override
    protected boolean checkExtraStartConditions(ServerLevel level, E entity) {
        var abilities = VampireAbilitiesComponent.KEY.getNullable(entity);
        if (abilities == null) {
            return false;
        }

        this.powers = abilities.getPowersOfClass(DashAbilityPower.class).stream()
                .filter(d -> DFunctions.evaluate(d.canDash(), DFunctions.createEntityContext(entity))).toList();
        if (this.powers.isEmpty()) {
            return false;
        }


        var path = entity.getNavigation().getPath();
        if (path == null || path.getNodeCount() == 0) {
            return false;
        }

        this.nodePos = null;
        for (int i = path.getNodeCount()-1; i >= 0; i--) {
            this.nodePos = path.getEntityPosAtNode(entity, i);
            double sqrDist = entity.distanceToSqr(this.nodePos);
            if (sqrDist < this.sqrMinDashDistance) {
                this.nodePos = null;
                break;
            }

            if (sqrDist <= this.sqrMaxDashDistance) {
                break;
            }

            if (i == 0) {
                this.nodePos = null;
            }
        }

        return this.nodePos != null;
    }

    @Override
    protected void start(E entity) {
        this.powers.get(0).dashWithTarget(entity, this.nodePos);
        this.nodePos = null;
        this.powers = null;
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return List.of(
                Pair.of(MemoryModuleType.WALK_TARGET, MemoryStatus.VALUE_PRESENT)
        );
    }
}
