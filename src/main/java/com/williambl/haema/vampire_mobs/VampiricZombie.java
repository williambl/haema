package com.williambl.haema.vampire_mobs;

import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.*;
import net.minecraft.world.entity.monster.Zombie;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.ServerLevelAccessor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public class VampiricZombie extends Zombie implements OwnableEntity {
    public static final String OWNER_UUID_TAG = "ownerUUID";
    private @Nullable UUID ownerUUID;
    private @Nullable LivingEntity cachedOwner;

    public VampiricZombie(EntityType<? extends VampiricZombie> entityType, Level level) {
        super(entityType, level);
    }

    public void setOwner(LivingEntity owner) {
        this.ownerUUID = owner.getUUID();
        this.cachedOwner = owner;
    }

    @Nullable
    @Override
    public UUID getOwnerUUID() {
        return this.ownerUUID;
    }

    @Nullable
    @Override
    public LivingEntity getOwner() {
        if (this.ownerUUID == null) {
            return null;
        }

        if (this.cachedOwner != null && this.cachedOwner.getUUID().equals(this.ownerUUID)) {
            return this.cachedOwner;
        }

        if (this.level() instanceof ServerLevel level) {
            this.cachedOwner = level.getEntity(this.ownerUUID) instanceof LivingEntity living ? living : null;
            return this.cachedOwner;
        }

        return null;
    }

    private Optional<LivingEntity> getOwnerOpt() {
        return Optional.ofNullable(this.getOwner());
    }

    @Nullable
    @Override
    public SpawnGroupData finalizeSpawn(ServerLevelAccessor serverLevelAccessor, DifficultyInstance difficultyInstance, MobSpawnType mobSpawnType, @Nullable SpawnGroupData spawnGroupData, @Nullable CompoundTag compoundTag) {
        var res = super.finalizeSpawn(serverLevelAccessor, difficultyInstance, mobSpawnType, spawnGroupData, compoundTag);
        var converted = VampireComponent.KEY.get(this).tryConvert(serverLevelAccessor.registryAccess().registryOrThrow(VampirismSource.REGISTRY_KEY).get(HaemaVampireMobs.VampireMobVampirismSources.VAMPIRIC_ZOMBIE_SPAWN));
        if (!converted) {
            Haema.LOGGER.warn("Failed to set vampirager {} as vampire", this);
        }
        return res;
    }

    @Override
    public void readAdditionalSaveData(CompoundTag compoundTag) {
        super.readAdditionalSaveData(compoundTag);
        if (compoundTag.contains(OWNER_UUID_TAG)) {
            this.ownerUUID = compoundTag.getUUID(OWNER_UUID_TAG);
        }
    }

    @Override
    public void addAdditionalSaveData(CompoundTag compoundTag) {
        super.addAdditionalSaveData(compoundTag);
        if (this.ownerUUID != null) {
            compoundTag.putUUID(OWNER_UUID_TAG, this.ownerUUID);
        }
    }

    @Override
    protected void customServerAiStep() {
        super.customServerAiStep();
        if (!this.isDeadOrDying()
                && !this.level().isClientSide()
                && this.getOwnerOpt().filter(Entity::isAlive).isPresent()
                && !this.hasEffect(MobEffects.WITHER)) {
            this.addEffect(new MobEffectInstance(MobEffects.WITHER, 10 * 20));
        }
    }

    @Override
    public boolean canAttack(LivingEntity target) {
        var owner = this.getOwnerOpt();
        boolean canAttack = super.canAttack(target);
        boolean isOwner = owner.filter(o -> o == target).isPresent();
        boolean canOwnerAttack = owner.map(o -> o.canAttack(target)).orElse(true);
        return canAttack && !isOwner && canOwnerAttack;
    }

    @Override
    public boolean doHurtTarget(Entity entity) {
        boolean res = super.doHurtTarget(entity);
        if (res) {
            var drinkingPowers = VampireAbilitiesComponent.KEY.maybeGet(this)
                    .map(c -> c.getEnabledPowersOfClass(DrinkingAbilityPower.class))
                    .orElse(List.of());
            for (var power : drinkingPowers) {
                if (power.tryDrink(this, entity)) {
                    return true;
                }
            }
        }

        return res;
    }

    @Override
    protected @NotNull ItemStack getSkull() {
        return ItemStack.EMPTY;
    }
}
