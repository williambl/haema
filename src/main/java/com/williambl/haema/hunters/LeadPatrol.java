package com.williambl.haema.hunters;

import com.mojang.datafixers.util.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.ai.behavior.BlockPosTracker;
import net.minecraft.world.entity.ai.behavior.EntityTracker;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.entity.ai.memory.WalkTarget;
import net.minecraft.world.entity.monster.PatrollingMonster;
import net.minecraft.world.level.levelgen.Heightmap;
import net.minecraft.world.phys.Vec3;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;
import net.tslat.smartbrainlib.util.BrainUtils;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class LeadPatrol<E extends PatrollingMonster> extends ExtendedBehaviour<E> {
    private float speedModifier = 1f;
    private int closeEnoughDistance = 10;

    private @Nullable BlockPos target;

    public LeadPatrol() {
    }

    public LeadPatrol<E> closeEnoughDistance(int closeEnoughDistance) {
        this.closeEnoughDistance = closeEnoughDistance;
        return this;
    }

    public LeadPatrol<E> speedModifier(float speedModifier) {
        this.speedModifier = speedModifier;
        return this;
    }

    @Override
    protected boolean checkExtraStartConditions(ServerLevel level, E entity) {
        if (!entity.isPatrolLeader()) {
            return false;
        }

        BlockPos patrolTarget = entity.getPatrolTarget();
        //noinspection ConstantValue
        if (patrolTarget == null) {
            this.target = null;
            return false;
        }

        if (patrolTarget.closerToCenterThan(entity.position(), this.closeEnoughDistance)) {
            entity.findPatrolTarget();
            patrolTarget = entity.getPatrolTarget();
        }

        // from PatrollingMonster$LongDistancePatrolGoal
        Vec3 vec3 = Vec3.atBottomCenterOf(patrolTarget);
        Vec3 vec32 = entity.position();
        Vec3 vec33 = vec32.subtract(vec3);
        vec3 = vec33.yRot(90.0F).scale(0.4).add(vec3);
        Vec3 vec34 = vec3.subtract(vec32).normalize().scale(10.0).add(vec32);
        BlockPos blockPos = BlockPos.containing(vec34);
        this.target = entity.level().getHeightmapPos(Heightmap.Types.MOTION_BLOCKING_NO_LEAVES, blockPos);
        if (this.target.closerToCenterThan(entity.position(), this.closeEnoughDistance)) {
            entity.findPatrolTarget();
            return false;
        }
        return true;
    }

    @Override
    protected void start(E entity) {
        if (this.target != null) {
            BrainUtils.setMemory(entity, MemoryModuleType.WALK_TARGET, new WalkTarget(new BlockPosTracker(this.target), this.speedModifier, this.closeEnoughDistance));
        }
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return List.of(
                Pair.of(MemoryModuleType.LOOK_TARGET, MemoryStatus.REGISTERED),
                Pair.of(MemoryModuleType.WALK_TARGET, MemoryStatus.VALUE_ABSENT));
    }
}
