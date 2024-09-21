package com.williambl.haema.vampire_mobs;

import com.mojang.datafixers.util.Pair;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.ability.powers.dash.DashAbilityPower;
import com.williambl.haema.vampire.ability.powers.dash.EntityChargingDashComponent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.phys.Vec3;
import net.tslat.smartbrainlib.api.core.behaviour.DelayedBehaviour;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;

import java.util.List;

public class DashToWalkTarget<E extends PathfinderMob> extends DelayedBehaviour<E> {
    private Vec3 nodePos;
    private int nextNodeIdx = -1;
    private List<DashAbilityPower> powers;
    private double sqrMinDashDistance = 5*5;
    private double sqrMaxDashDistance = 16*16;

    public DashToWalkTarget(int delayTicks) {
        super(delayTicks);
    }

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

        this.powers = abilities.getEnabledPowersOfClass(DashAbilityPower.class).stream()
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
            this.nextNodeIdx = i;
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
                this.nextNodeIdx = -1;
            }
        }

        return this.nodePos != null;
    }

    @Override
    protected void start(E entity) {
        EntityChargingDashComponent.KEY.maybeGet(entity).ifPresent(c -> c.setChargingDash(true));
    }

    @Override
    protected void doDelayedAction(E entity) {
        super.doDelayedAction(entity);
        EntityChargingDashComponent.KEY.maybeGet(entity).ifPresent(c -> c.setChargingDash(false));
        this.powers.get(0).dashWithTarget(entity, this.nodePos);
        this.nodePos = null;
        this.powers = null;
        var path = entity.getNavigation().getPath();
        if (path != null && path.getNodeCount() > this.nextNodeIdx && this.nextNodeIdx > path.getNextNodeIndex()) {
            path.setNextNodeIndex(this.nextNodeIdx);
        }
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return List.of(
                Pair.of(MemoryModuleType.WALK_TARGET, MemoryStatus.VALUE_PRESENT)
        );
    }
}
