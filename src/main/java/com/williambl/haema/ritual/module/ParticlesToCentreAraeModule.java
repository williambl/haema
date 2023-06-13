package com.williambl.haema.ritual.module;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.api.ritual.RitualArae;
import com.williambl.haema.api.ritual.module.AraeModule;
import net.minecraft.core.BlockPos;
import net.minecraft.core.particles.ParticleOptions;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

import java.util.List;

//TODO create vec3 dfunctions?
public record ParticlesToCentreAraeModule(int interval, ParticleOptions particle, List<Vec3> positions, Vec3 centre, double minSpeed, double maxSpeed) implements AraeModule {
    private static final KeyDispatchDataCodec<ParticlesToCentreAraeModule> CODEC = new KeyDispatchDataCodec<>(RecordCodecBuilder.create(instance -> instance.group(
            Codec.intRange(0, Integer.MAX_VALUE).fieldOf("interval").forGetter(ParticlesToCentreAraeModule::interval),
            ParticleTypes.CODEC.fieldOf("particle").forGetter(ParticlesToCentreAraeModule::particle),
            Vec3.CODEC.listOf().fieldOf("positions").forGetter(ParticlesToCentreAraeModule::positions),
            Vec3.CODEC.fieldOf("centre").forGetter(ParticlesToCentreAraeModule::centre),
            Codec.DOUBLE.fieldOf("minSpeed").forGetter(ParticlesToCentreAraeModule::minSpeed),
            Codec.DOUBLE.fieldOf("maxSpeed").forGetter(ParticlesToCentreAraeModule::maxSpeed)
    ).apply(instance, ParticlesToCentreAraeModule::new)));

    @Override
    public void onAraeCreated(RitualArae arae, ServerLevel level, BlockPos pos) {

    }

    @Override
    public void onAraeTick(RitualArae arae, Level level, BlockPos pos) {
        if (level.getGameTime() + level.getRandom().nextInt(this.interval/10) % this.interval < this.interval/3.0) {
            return;
        }

        if (level.isClientSide()) {
            var centreWorldSpace = Vec3.atLowerCornerOf(pos).add(this.centre);
            for (var particlePos : this.positions) {
                var posWorldSpace = Vec3.atLowerCornerOf(pos).add(particlePos);
                double speed = this.minSpeed + (this.maxSpeed - this.minSpeed) * level.getRandom().nextDouble();
                Vec3 direction = centreWorldSpace.subtract(posWorldSpace).normalize().scale(speed);

                level.addParticle(this.particle, posWorldSpace.x(), posWorldSpace.y(), posWorldSpace.z(), direction.x(), direction.y(), direction.z());
            }
        }
    }

    @Override
    public void onAraeDestroyed(RitualArae arae, ServerLevel level, BlockPos pos) {

    }

    @Override
    public KeyDispatchDataCodec<? extends AraeModule> codec() {
        return CODEC;
    }
}
