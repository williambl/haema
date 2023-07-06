package com.williambl.haema.hunters;

import com.williambl.haema.api.vampire.VampireApi;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.tags.BiomeTags;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.entity.SpawnPlacements;
import net.minecraft.world.entity.monster.PatrollingMonster;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.CustomSpawner;
import net.minecraft.world.level.NaturalSpawner;
import net.minecraft.world.level.entity.EntityTypeTest;
import net.minecraft.world.level.levelgen.Heightmap;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class VampireHunterSpawner implements CustomSpawner {
    private int ticksUntilNextSpawn = 0;

    @Override
    public int tick(@NotNull ServerLevel level, boolean spawnMonsters, boolean spawnAnimals) {
        if (!spawnMonsters) {
            return 0;
        }
        //TODO check gamerule

        var random = level.getRandom();
        if (--this.ticksUntilNextSpawn > 0) {
            return 0;
        }

        this.ticksUntilNextSpawn += 12000 + random.nextInt(1200);
        if (random.nextInt(5) != 0) {
            return 0;
        }

        var vampires = level.getEntities(EntityTypeTest.forClass(LivingEntity.class), l -> VampireApi.isVampire(l) && !l.isSpectator());
        var vampireCount = vampires.size();
        if (vampireCount < 1) {
            return 0;
        }

        var randomVampire = vampires.get(random.nextInt(vampireCount));

        if (level.isCloseToVillage(randomVampire.blockPosition(), 2)) {
            return 0;
        }

        return trySpawnNear(level, random, randomVampire.blockPosition());
    }

    public static int trySpawnNear(ServerLevel level, RandomSource random, BlockPos blockPos) {
        int dx = (24 + random.nextInt(24)) * (random.nextBoolean() ? -1 : 1);
        int dz = (24 + random.nextInt(24)) * (random.nextBoolean() ? -1 : 1);
        var mutable = blockPos.mutable().move(dx, 0, dz);
        if (!level.isLoaded(mutable)) {
            return 0;
        }

        var biome = level.getBiome(mutable);
        if (biome.is(BiomeTags.WITHOUT_PATROL_SPAWNS)) {
            return 0;
        }

        int amountSpawned = 0;
        int localDifficulty = (int) Math.ceil(level.getCurrentDifficultyAt(mutable).getEffectiveDifficulty()) + 1;
        SpawnGroupData data = null;
        for (int i = 0; i < localDifficulty; i++) {
            mutable.setY(level.getHeight(Heightmap.Types.MOTION_BLOCKING_NO_LEAVES, mutable.getX(), mutable.getZ()));
            boolean isLeader = data == null;
            var entity = trySpawnOne(level, mutable, random, isLeader, data);
            if (entity != null) {
                amountSpawned++;
                if (isLeader) {
                    data = new VampireHunter.VampireHunterGroupData(entity.getUUID());
                }
            }

            mutable.move(random.nextIntBetweenInclusive(-5, 5), 0, random.nextIntBetweenInclusive(-5, 5));
        }

        return amountSpawned;
    }

    private static @Nullable VampireHunter trySpawnOne(ServerLevel level, BlockPos pos, RandomSource random, boolean isLeader, @Nullable SpawnGroupData groupData) {
        if (!NaturalSpawner.isSpawnPositionOk(
                SpawnPlacements.getPlacementType(HaemaHunters.HunterEntityTypes.VAMPIRE_HUNTER),
                level,
                pos,
                HaemaHunters.HunterEntityTypes.VAMPIRE_HUNTER)) {
            return null;
        }

        if (!PatrollingMonster.checkPatrollingMonsterSpawnRules(HaemaHunters.HunterEntityTypes.VAMPIRE_HUNTER, level, MobSpawnType.PATROL, pos, random)) {
            return null;
        }

        var entity = HaemaHunters.HunterEntityTypes.VAMPIRE_HUNTER.create(level);
        if (entity == null) {
            return null;
        }

        if (isLeader) {
            entity.setPatrolLeader(true);
            entity.findPatrolTarget();
        }

        entity.setPos(pos.getX(), pos.getY(), pos.getZ());
        entity.finalizeSpawn(level, level.getCurrentDifficultyAt(pos), MobSpawnType.PATROL, groupData, null);
        level.addFreshEntityWithPassengers(entity);
        return entity;
    }
}
