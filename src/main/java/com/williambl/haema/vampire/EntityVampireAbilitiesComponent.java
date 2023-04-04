package com.williambl.haema.vampire;

import com.mojang.datafixers.util.Pair;
import com.mojang.serialization.Codec;
import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.LivingEntity;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class EntityVampireAbilitiesComponent implements VampireAbilitiesComponent {
    private final LinkedHashSet<VampireAbility> abilities = new LinkedHashSet<>();
    private final LivingEntity entity;
    
    private static final String ABILITIES_KEY = "abilities";
    private static final Codec<List<ResourceKey<VampireAbility>>> ABILITIES_CODEC = ResourceKey.codec(VampireAbility.REGISTRY_KEY).listOf();

    public EntityVampireAbilitiesComponent(LivingEntity entity) {
        this.entity = entity;
    }

    @Override
    public Set<VampireAbility> getAbilities() {
        return Set.copyOf(this.abilities);
    }

    @Override
    public void addAbility(VampireAbility ability) {
        this.abilities.add(ability);
    }

    @Override
    public void removeAbility(VampireAbility ability) {
        this.abilities.remove(ability);
    }

    @Override
    public boolean hasAbility(VampireAbility ability) {
        return this.abilities.contains(ability);
    }

    @Override
    public void readFromNbt(CompoundTag tag) {
        var registries = this.entity.level.registryAccess();
        var sourceRegistry = registries.registryOrThrow(VampireAbility.REGISTRY_KEY);
        this.abilities.clear();
        if (tag.contains(ABILITIES_KEY)) {
            var abilities = ABILITIES_CODEC.decode(NbtOps.INSTANCE, tag.get(ABILITIES_KEY))
                    .resultOrPartial(e -> Haema.LOGGER.warn("Error decoding Vampire Abilities for entity {}: {}", this.entity.getScoreboardName(), e))
                    .map(Pair::getFirst)
                    .orElseGet(List::of)
                    .stream()
                    .map(sourceRegistry::get)
                    .toList();
            this.abilities.addAll(abilities);
        }
    }

    @Override
    public void writeToNbt(CompoundTag tag) {
        var registries = this.entity.level.registryAccess();
        var sourceRegistry = registries.registryOrThrow(VampireAbility.REGISTRY_KEY);
        var abilityKeys = this.abilities.stream()
                .map(sourceRegistry::getResourceKey)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .toList();
        ABILITIES_CODEC.encodeStart(NbtOps.INSTANCE, abilityKeys)
                .resultOrPartial(e -> Haema.LOGGER.warn("Error encoding Vampire Abilities for entity {}: {}", this.entity.getScoreboardName(), e))
                .ifPresent(t -> tag.put(ABILITIES_KEY, t));
    }

    @Override
    public void tick() {
        for (var ability : this.abilities) {
            for (var power : ability.powers()) {
                power.tick(this.entity, ability);
            }
        }
    }
}
