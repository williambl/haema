package com.williambl.haema.vampiremobs

import net.minecraft.entity.LivingEntity

interface OwnedMob {
    var owner: LivingEntity?
}