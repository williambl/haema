package com.williambl.haema.util

import com.williambl.haema.component.EntityVampireComponent
import com.williambl.haema.isVampire
import com.williambl.haema.vampireComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.goal.ActiveTargetGoal
import net.minecraft.entity.mob.HostileEntity
import net.minecraft.entity.mob.MobEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.server.network.ServerPlayerEntity
import java.util.function.Predicate

class DrinkBloodActiveTargetGoal<T : LivingEntity?> : ActiveTargetGoal<T> {
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

        if (mob.vampireComponent.blood >= 20) {
            return false
        }

        if (mob.vampireComponent.blood >= 15) {
            return mob is HostileEntity
        }

        return super.canStart()
    }

    override fun findClosestTarget() {
        targetEntity = if (targetClass != PlayerEntity::class.java && targetClass != ServerPlayerEntity::class.java) {
            mob.world.getClosestEntity(
                mob.world.getEntitiesByClass(targetClass, getSearchBox(this.followRange)) { livingEntity: T ->
                    livingEntity != null && !livingEntity.isVampire && (livingEntity.type.isIn(EntityVampireComponent.goodBloodTag) || livingEntity.type.isIn(EntityVampireComponent.mediumBloodTag) || livingEntity.type.isIn(EntityVampireComponent.poorBloodTag))
                },
                targetPredicate, mob, mob.x, mob.eyeY, mob.z
            )
        } else {
            mob.world.getClosestPlayer(targetPredicate, mob, mob.x, mob.eyeY, mob.z)
        }
    }
}