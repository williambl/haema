package com.williambl.haema.hunters;

import com.williambl.haema.api.vampire.VampireApi;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import net.fabricmc.fabric.api.entity.event.v1.ServerEntityCombatEvents;
import net.fabricmc.fabric.api.object.builder.v1.entity.FabricDefaultAttributeRegistry;
import net.fabricmc.fabric.api.object.builder.v1.entity.FabricEntityTypeBuilder;
import net.fabricmc.fabric.impl.object.builder.FabricEntityType;
import net.minecraft.core.Registry;
import net.minecraft.core.UUIDUtil;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.*;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.sensing.SensorType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;

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
}
