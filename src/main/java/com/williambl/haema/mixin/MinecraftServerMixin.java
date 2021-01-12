package com.williambl.haema.mixin;

import com.google.common.collect.ImmutableList;
import com.williambl.haema.entity.VampireHunterEntity;
import com.williambl.haema.entity.VampireHunterSpawner;
import net.minecraft.entity.EntityType;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.Identifier;
import net.minecraft.util.registry.Registry;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.ModifyVariable;

import java.util.List;

@Mixin(MinecraftServer.class)
public class MinecraftServerMixin {
    @SuppressWarnings({"unchecked", "rawtypes"})
    @ModifyVariable(method = "createWorlds", at = @At(value = "INVOKE_ASSIGN", target = "Lcom/google/common/collect/ImmutableList;of(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Lcom/google/common/collect/ImmutableList;"))
    List addVampireHunterSpawner(List list) {
        return ImmutableList.builder().addAll(list).add(new VampireHunterSpawner((EntityType<? extends VampireHunterEntity>) Registry.ENTITY_TYPE.get(new Identifier("haema:vampire_hunter")))).build();
    }
}
