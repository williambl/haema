package com.williambl.haema.hunters;

import com.williambl.haema.api.vampire.VampireApi;
import com.williambl.haema.api.vampire.ability.powers.drinking.EntityDrinkTargetCallback;
import com.williambl.haema.api.vampire.ability.powers.drinking.OnVampireDrinkCallback;
import net.fabricmc.fabric.api.entity.event.v1.ServerEntityCombatEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerTickEvents;
import net.fabricmc.fabric.api.gamerule.v1.CustomGameRuleCategory;
import net.fabricmc.fabric.api.gamerule.v1.GameRuleFactory;
import net.fabricmc.fabric.api.gamerule.v1.GameRuleRegistry;
import net.fabricmc.fabric.api.gamerule.v1.rule.DoubleRule;
import net.fabricmc.fabric.api.object.builder.v1.entity.FabricDefaultAttributeRegistry;
import net.fabricmc.fabric.api.object.builder.v1.entity.FabricEntityTypeBuilder;
import net.minecraft.core.Registry;
import net.minecraft.core.UUIDUtil;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.tags.TagKey;
import net.minecraft.world.Difficulty;
import net.minecraft.world.damagesource.DamageType;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.ai.gossip.GossipType;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.GameRules;

import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.IntStream;

import static com.williambl.haema.Haema.id;

public class HaemaHunters {
    public static void init() {
        HunterEntityTypes.init();
        HunterItems.init();
        HunterMemoryModuleTypes.init();
        HunterGameRules.init();
        HunterTags.init();

        var spawner = new VampireHunterSpawner();
        ServerTickEvents.END_SERVER_TICK.register(server ->
                server.getAllLevels().forEach(level ->
                        spawner.tick(
                                level,
                                server.getWorldData().getDifficulty() != Difficulty.PEACEFUL,
                                server.isSpawningAnimals()
                        ))
        );

        //todo test this ig
        OnVampireDrinkCallback.EVENT.register((vampire, target) -> {
            if (target instanceof Villager villager && !villager.isSleeping()) {
                villager.getGossips().add(vampire.getUUID(), GossipType.MAJOR_NEGATIVE, 20);
                if (vampire.level() instanceof ServerLevel level) {
                    /*if (drinker is ServerPlayerEntity) { TODO advancements
                        VampireHunterTriggerCriterion.trigger(drinker)
                    }*/
                    VampireHunterSpawner.trySpawnNear(
                            level,
                            vampire.getRandom(),
                            vampire.blockPosition());
                }
            }
        });
    }


    public static class HunterEntityTypes {
        public static final EntityType<VampireHunter> VAMPIRE_HUNTER = Registry.register(BuiltInRegistries.ENTITY_TYPE, id("vampire_hunter"), FabricEntityTypeBuilder.createMob().spawnGroup(MobCategory.CREATURE).entityFactory(VampireHunter::new).dimensions(EntityDimensions.fixed(0.6f, 1.95f)).trackRangeBlocks(128).trackedUpdateRate(3).spawnableFarFromPlayer().build());

        public static void init() {
            FabricDefaultAttributeRegistry.register(VAMPIRE_HUNTER, VampireHunter.createHunterAttributes());
        }
    }

    public static class HunterItems {
        public static final VampireHunterContractItem VAMPIRE_HUNTER_CONTRACT = Registry.register(BuiltInRegistries.ITEM, id("vampire_hunter_contract"), new VampireHunterContractItem(new Item.Properties().stacksTo(1)));

        public static void init() {
            ServerEntityCombatEvents.AFTER_KILLED_OTHER_ENTITY.register((world, entity, killedEntity) -> {
                if (entity instanceof Player player && VampireApi.isVampire(killedEntity)) {
                    IntStream.range(0, player.getInventory().getContainerSize()).mapToObj(player.getInventory()::getItem)
                            .filter(stack -> stack.getItem() == VAMPIRE_HUNTER_CONTRACT
                                    && !VampireHunterContractItem.isFulfilled(stack)
                                    && VampireHunterContractItem.getContractTarget(stack).filter(p -> !Objects.equals(p.getId(), killedEntity.getUUID())).isEmpty())
                            .findFirst()
                            .ifPresent(stack -> VampireHunterContractItem.fulfilContract(stack, entity));
                }
            });
        }
    }

    public static class HunterMemoryModuleTypes {
        public static final MemoryModuleType<UUID> LEADER = Registry.register(BuiltInRegistries.MEMORY_MODULE_TYPE, id("leader"), new MemoryModuleType<>(Optional.of(UUIDUtil.CODEC)));

        public static void init() {}
    }

    public static class HunterGameRules {
        public static final CustomGameRuleCategory CATEGORY = new CustomGameRuleCategory(id("vampire_hunters"), Component.translatable("gamerule.category.vampire_hunters"));
        public static final GameRules.Key<GameRules.BooleanValue> PATROLS_ENABLED = GameRuleRegistry.register(id("vampire_hunter_spawning/patrols/enabled").toString(), CATEGORY, GameRuleFactory.createBooleanRule(true));
        public static final GameRules.Key<DoubleRule> HUNTER_DEATH_NOTICE_CHANCE = GameRuleRegistry.register(id("vampire_hunter_spawning/on_entity_killed/chance").toString(), CATEGORY, GameRuleFactory.createDoubleRule(0.1, 0.0, 1.0));

        public static void init() {}
    }

    public static class HunterTags {
        public static final TagKey<DamageType> ALERTS_HUNTERS = TagKey.create(Registries.DAMAGE_TYPE, id("alerts_hunters"));

        public static void init() {}
    }
}
