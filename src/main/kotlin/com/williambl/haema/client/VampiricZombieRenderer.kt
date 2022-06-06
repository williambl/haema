package com.williambl.haema.client

import com.williambl.haema.id
import net.minecraft.client.render.entity.EntityRendererFactory
import net.minecraft.client.render.entity.ZombieEntityRenderer
import net.minecraft.entity.mob.ZombieEntity
import net.minecraft.util.Identifier

class VampiricZombieRenderer(context: EntityRendererFactory.Context) : ZombieEntityRenderer(context) {
    override fun getTexture(zombieEntity: ZombieEntity?): Identifier = texture

    companion object {
        val texture = id("textures/entity/vampiric_zombie.png")
    }
}