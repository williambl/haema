package com.williambl.haema.vampiremobs.elder.behaviour

import com.williambl.haema.vampiremobs.VampireMobsModule
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.brain.MemoryModuleType
import net.minecraft.entity.ai.brain.sensor.SensorType
import net.minecraft.server.world.ServerWorld
import net.tslat.smartbrainlib.api.core.sensor.ExtendedSensor
import net.tslat.smartbrainlib.util.BrainUtils

class SunVisibilitySensor<E: LivingEntity>: ExtendedSensor<E>(){
    override fun memoriesUsed(): List<MemoryModuleType<*>> = listOf(VampireMobsModule.SKY_VISIBLE_MEMORY)

    override fun type(): SensorType<out ExtendedSensor<*>> = VampireMobsModule.SKY_VISIBLE_SENSOR

    override fun sense(level: ServerWorld, entity: E) {
        if (level.isSkyVisible(entity.blockPos)) {
            BrainUtils.setMemory(entity, VampireMobsModule.SKY_VISIBLE_MEMORY, com.mojang.datafixers.util.Unit.INSTANCE)
        } else {
            BrainUtils.clearMemory(entity, VampireMobsModule.SKY_VISIBLE_MEMORY)
        }
    }
}