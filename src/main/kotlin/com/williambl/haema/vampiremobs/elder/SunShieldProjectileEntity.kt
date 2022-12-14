package com.williambl.haema.vampiremobs.elder

import com.williambl.haema.vampiremobs.VampireMobsModule
import net.minecraft.entity.EntityType
import net.minecraft.entity.FlyingItemEntity
import net.minecraft.entity.projectile.ProjectileEntity
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.particle.DustParticleEffect
import net.minecraft.util.hit.HitResult
import net.minecraft.world.World

class SunShieldProjectileEntity(entityType: EntityType<out ProjectileEntity>, world: World) : ProjectileEntity(
    entityType,
    world
), FlyingItemEntity {
    override fun initDataTracker() {
    }

    override fun getStack(): ItemStack {
        return Items.FIRE_CHARGE.defaultStack
    }

    override fun hasNoGravity(): Boolean {
        return true
    }

    override fun tick() {
        super.tick()
        if (this.world.isClient) {
            this.world.addParticle(DustParticleEffect(DustParticleEffect.RED, 3f), this.x, this.y, this.z, 0.0, 0.0, 0.0)
        } else if (this.age >= 60) {
            this.world.setBlockState(this.blockPos, VampireMobsModule.SUN_SHIELD_BLOCK.defaultState)
            this.discard()
        }
    }

    override fun onCollision(hitResult: HitResult) {
        if (hitResult.type != HitResult.Type.MISS) {
            this.world.setBlockState(this.blockPos, VampireMobsModule.SUN_SHIELD_BLOCK.defaultState)
            this.discard()
        }
    }
}