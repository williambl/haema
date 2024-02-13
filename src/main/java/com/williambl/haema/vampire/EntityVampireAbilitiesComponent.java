package com.williambl.haema.vampire;

import com.mojang.datafixers.util.Pair;
import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.minecraft.core.Holder;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class EntityVampireAbilitiesComponent implements VampireAbilitiesComponent {
    private final ArrayList<Holder.Reference<VampireAbility>> abilities = new ArrayList<>();
    private final ArrayList<Holder.Reference<VampireAbility>> enabledAbilities = new ArrayList<>();
    private final HashMap<Class<? extends VampireAbilityPower>, List<VampireAbilityPower>> powers = new HashMap<>();
    private final LivingEntity entity;
    private @Nullable Holder.Reference<VampireAbility> activeAbility;
    
    private static final String ABILITIES_KEY = "abilities";
    private static final String ACTIVE_ABILITY_KEY = "active_ability";
    private static final Codec<List<ResourceKey<VampireAbility>>> ABILITIES_CODEC = ResourceKey.codec(VampireAbility.REGISTRY_KEY).listOf();

    public EntityVampireAbilitiesComponent(LivingEntity entity) {
        this.entity = entity;
    }

    @Override
    public Set<Holder.Reference<VampireAbility>> getAbilities() {
        return Set.copyOf(this.abilities);
    }

    @Override
    public Set<Holder.Reference<VampireAbility>> getEnabledAbilities() {
        return Set.copyOf(this.enabledAbilities);
    }

    @Override
    public boolean isAbilityEnabled(Holder<VampireAbility> ability) {
        return ability instanceof Holder.Reference<VampireAbility> ref && this.enabledAbilities.contains(ref);
    }

    @Override
    public boolean addAbility(Holder<VampireAbility> ability) {
        if (!(ability instanceof Holder.Reference<VampireAbility> ref)) {
            return false;
        }

        if (this.abilities.contains(ability)) {
            return false;
        }
        if (ability.value().conflicts().stream().anyMatch(c -> this.abilities.stream().anyMatch(h -> h.key() == c))) {
            return false;
        }
        if (!ability.value().prerequisites().stream().allMatch(c -> this.abilities.stream().anyMatch(h -> h.key() == c))) {
            return false;
        }

        boolean isSuperceded = this.abilities.stream().anyMatch(h -> h.value().supercedes().stream().anyMatch(r -> r == ref.key()));

        this.abilities.add(ref);
        for (var power : ability.value().powers()) {
            if (!isSuperceded) {
                power.apply(this.entity, ability.value());
                this.powers.computeIfAbsent(power.getClass(), k -> new ArrayList<>()).add(power);
            }
        }

        this.recalculateEnabledAbilities(true);
        KEY.sync(this.entity);

        return true;
    }

    @Override
    public boolean removeAbility(Holder<VampireAbility> ability) {
        if (!(ability instanceof Holder.Reference<VampireAbility> ref)) {
            return false;
        }

        this.abilities.remove(ability);
        for (var power : ability.value().powers()) {
            if (this.powers.computeIfAbsent(power.getClass(), k -> new ArrayList<>()).remove(power)) {
                power.remove(this.entity, ability.value());
            }
        }
        if (ability == this.activeAbility) {
            this.activeAbility = null;
        }

        this.recalculateEnabledAbilities(true);
        KEY.sync(this.entity);

        return true;
    }

    @Override
    public boolean hasAbility(Holder<VampireAbility> ability) {
        return ability instanceof Holder.Reference<VampireAbility> ref && this.abilities.contains(ref);
    }

    @Override
    public <T extends VampireAbilityPower> List<T> getEnabledPowersOfClass(Class<T> clazz) {
        return this.powers.getOrDefault(clazz, List.of()).stream().map(clazz::cast).toList();
    }

    @Override
    public boolean setActiveAbility(Holder<VampireAbility> ability) {
        if (ability instanceof Holder.Reference<VampireAbility> ref && this.abilities.contains(ref)) {
            this.activeAbility = ref;
            return true;
        }

        return false;
    }

    @Override
    public Optional<Holder<VampireAbility>> getActiveAbility() {
        return Optional.ofNullable(this.activeAbility);
    }

    @Override
    public void readFromNbt(CompoundTag tag) {
        var registries = this.entity.level().registryAccess();
        var sourceRegistry = registries.registryOrThrow(VampireAbility.REGISTRY_KEY);
        this.abilities.clear();
        this.enabledAbilities.clear();
        this.powers.clear();
        this.activeAbility = null;
        if (tag.contains(ABILITIES_KEY)) {
            var abilities = sourceRegistry.holderByNameCodec().listOf().decode(NbtOps.INSTANCE, tag.get(ABILITIES_KEY))
                    .resultOrPartial(e -> Haema.LOGGER.warn("Error decoding Vampire Abilities for entity {}: {}", this.entity.getScoreboardName(), e))
                    .map(Pair::getFirst)
                    .orElseGet(List::of)
                    .stream()
                    .filter(Holder.Reference.class::isInstance)
                    .<Holder.Reference<VampireAbility>>map(Holder.Reference.class::cast)
                    .distinct()
                    .toList();
            this.abilities.addAll(abilities);
            for (var ability : this.abilities) {
                for (var power : ability.value().powers()) {
                    this.powers.computeIfAbsent(power.getClass(), k -> new ArrayList<>()).add(power);
                }
            }
            if (tag.contains(ACTIVE_ABILITY_KEY, Tag.TAG_STRING)) {
                ResourceLocation.read(tag.getString(ACTIVE_ABILITY_KEY))
                        .flatMap(resLoc -> sourceRegistry.getHolder(ResourceKey.create(sourceRegistry.key(), resLoc))
                                .map(DataResult::success)
                                .orElseGet(() -> DataResult.error(() -> "Active vampire ability %s does not exist".formatted(resLoc))))
                        .flatMap(a -> this.abilities.contains(a) ? DataResult.success(a) : DataResult.error(() -> "Active vampire ability %s is not on vampire %s".formatted(a.key(), this.entity.getScoreboardName())))
                        .resultOrPartial(s -> Haema.LOGGER.warn("Error decoding Active Vampire Ability for entity {}: {}", this.entity.getScoreboardName(), s))
                        .ifPresent(a -> this.activeAbility = a);
            }
        }

        this.recalculateEnabledAbilities(false);
    }

    @Override
    public void writeToNbt(CompoundTag tag) {
        var registries = this.entity.level().registryAccess();
        var sourceRegistry = registries.registryOrThrow(VampireAbility.REGISTRY_KEY);
        sourceRegistry.holderByNameCodec().listOf().encodeStart(NbtOps.INSTANCE, List.copyOf(this.abilities))
                .resultOrPartial(e -> Haema.LOGGER.warn("Error encoding Vampire Abilities for entity {}: {}", this.entity.getScoreboardName(), e))
                .ifPresent(t -> tag.put(ABILITIES_KEY, t));
        if (this.activeAbility != null) {
            var key = this.activeAbility.key();
            tag.putString(ACTIVE_ABILITY_KEY, key.toString());
        }
    }

    @Override
    public void writeSyncPacket(FriendlyByteBuf buf, ServerPlayer recipient) {
        buf.writeCollection(this.abilities, (b, h) -> b.writeResourceKey(h.key()));
        buf.writeOptional(Optional.ofNullable(this.activeAbility), (b, h) -> b.writeVarInt(this.abilities.indexOf(h)));
        buf.writeCollection(this.enabledAbilities, (b, h) -> b.writeVarInt(this.abilities.indexOf(h)));
    }

    @Override
    public void applySyncPacket(FriendlyByteBuf buf) {
        var registries = this.entity.level().registryAccess();
        var sourceRegistry = registries.registryOrThrow(VampireAbility.REGISTRY_KEY);
        this.abilities.clear();
        this.abilities.addAll(buf.readList(b -> sourceRegistry.getHolder(b.readResourceKey(sourceRegistry.key())))
                .stream()
                .filter(Optional::isPresent)
                .map(Optional::get).toList());
        this.activeAbility = buf.readOptional(b -> this.abilities.get(b.readVarInt())).orElse(null);
        this.enabledAbilities.clear();
        this.enabledAbilities.addAll(buf.readList(b -> this.abilities.get(b.readVarInt())));
    }

    @Override
    public boolean shouldSyncWith(ServerPlayer player) {
        return this.entity == player; //TODO check if necessary
    }
    @Override
    public void tick() {
        for (var ability : this.enabledAbilities) {
            for (var power : ability.value().powers()) {
                power.tick(this.entity, ability.value(), ability == this.activeAbility);
            }
        }
    }

    private void recalculateEnabledAbilities(boolean runRemoveAndAdd) {
        this.enabledAbilities.addAll(this.abilities);
        for (var ability : this.abilities) {
            for (var superceded : ability.value().supercedes()) {
                this.enabledAbilities.removeIf(a -> a.key() == superceded);
            }
        }
        for (var ability : this.abilities) {
            if (!this.enabledAbilities.contains(ability)) {
                for (var power : ability.value().powers()) {
                    if (this.powers.computeIfAbsent(power.getClass(), k -> new ArrayList<>()).remove(power) && runRemoveAndAdd) {
                        power.remove(this.entity, ability.value());
                    }
                }
            } else {
                for (var power : ability.value().powers()) {
                    if (!this.powers.computeIfAbsent(power.getClass(), k -> new ArrayList<>()).contains(power)) {
                        if (runRemoveAndAdd) {
                            power.apply(this.entity, ability.value());
                        }
                        this.powers.get(power.getClass()).add(power);
                    }
                }
            }
        }
    }
}
