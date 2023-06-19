package com.williambl.haema.ritual.ritual;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.api.ritual.ritual.Ritual;
import com.williambl.haema.api.ritual.ritual.RitualAction;
import com.williambl.haema.api.ritual.ritual.RitualContainer;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.EntityEvent;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobSpawnType;

public record SpawnEntityRitualAction(EntityType<?> entityType) implements RitualAction {
    public static final KeyDispatchDataCodec<SpawnEntityRitualAction> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            BuiltInRegistries.ENTITY_TYPE.byNameCodec().fieldOf("entity_type").forGetter(SpawnEntityRitualAction::entityType)
    ).apply(instance, SpawnEntityRitualAction::new)));

    @Override
    public void run(Ritual ritual, RitualContainer container) {
        var entity = this.entityType().spawn(container.level(), container.altarPos().above(), MobSpawnType.MOB_SUMMONED);
        if (entity != null) {
            container.level().broadcastEntityEvent(entity, EntityEvent.POOF);
        }
    }

    @Override
    public KeyDispatchDataCodec<? extends RitualAction> codec() {
        return CODEC;
    }
}
