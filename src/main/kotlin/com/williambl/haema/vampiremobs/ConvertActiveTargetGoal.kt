package com.williambl.haema.vampiremobs

import com.williambl.haema.isVampirable
import com.williambl.haema.isVampire
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.goal.ActiveTargetGoal
import net.minecraft.entity.mob.MobEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.server.network.ServerPlayerEntity
import java.util.function.Predicate

class ConvertActiveTargetGoal<T : LivingEntity?> : ActiveTargetGoal<T> {
    constructor(mob: MobEntity, targetClass: Class<T>, checkVisibility: Boolean) : super(
        mob,
        targetClass,
        checkVisibility
    ) {
    }

    constructor(
        mob: MobEntity,
        targetClass: Class<T>,
        checkVisibility: Boolean,
        targetPredicate: Predicate<LivingEntity>
    ) : super(mob, targetClass, checkVisibility, targetPredicate) {
    }

    constructor(mob: MobEntity, targetClass: Class<T>, checkVisibility: Boolean, checkCanNavigate: Boolean) : super(
        mob,
        targetClass,
        checkVisibility,
        checkCanNavigate
    ) {
    }

    constructor(
        mob: MobEntity,
        targetClass: Class<T>,
        reciprocalChance: Int,
        checkVisibility: Boolean,
        checkCanNavigate: Boolean,
        targetPredicate: Predicate<LivingEntity>
    ) : super(mob, targetClass, reciprocalChance, checkVisibility, checkCanNavigate, targetPredicate) {
    }

    override fun canStart(): Boolean {
        if (!mob.isVampire) {
            return false
        }

        return super.canStart()
    }

    override fun findClosestTarget() {
        targetEntity = if (targetClass != PlayerEntity::class.java && targetClass != ServerPlayerEntity::class.java) {
            mob.world.getClosestEntity(
                mob.world.getEntitiesByClass(targetClass, getSearchBox(this.followRange)) { livingEntity: T ->
                    livingEntity != null && !livingEntity.isVampire && livingEntity.isVampirable()
                },
                targetPredicate, mob, mob.x, mob.eyeY, mob.z
            )
        } else {
            mob.world.getClosestPlayer(targetPredicate, mob, mob.x, mob.eyeY, mob.z)
        }
    }
}