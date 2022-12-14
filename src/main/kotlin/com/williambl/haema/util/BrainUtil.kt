package com.williambl.haema.util

import com.williambl.haema.vampiremobs.VampireMobsModule
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.mob.MobEntity
import net.tslat.smartbrainlib.api.util.BrainUtils

fun setMultiTarget(entity: LivingEntity, target: List<LivingEntity>) {
    if (entity is MobEntity) {
        entity.target = target.firstOrNull();
    }

    if (target.isEmpty()) {
        BrainUtils.clearMemory(entity, VampireMobsModule.ATTACK_TARGETS_MEMORY);
    } else {
        BrainUtils.setMemory(entity, VampireMobsModule.ATTACK_TARGETS_MEMORY, target);
    }
}
