package com.williambl.haema.vampiremobs

import net.minecraft.entity.LivingEntity

interface MultiTargeter {
    val targets: Set<LivingEntity>
}