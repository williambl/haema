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
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(MinecraftServer.class)
public class MinecraftServerMixin {
    //TODO: PR to FAPI
    @SuppressWarnings("unchecked")
    @Redirect(method = "createWorlds", at = @At(value = "INVOKE", target = "Lcom/google/common/collect/ImmutableList;of(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Lcom/google/common/collect/ImmutableList;"))
    <E> ImmutableList<E> addVampireHunterSpawner(E e1, E e2, E e3, E e4, E e5) {
        return ImmutableList.of(e1, e2, e3, e4, e5, (E) new VampireHunterSpawner((EntityType<? extends VampireHunterEntity>) Registry.ENTITY_TYPE.get(new Identifier("haema:vampire_hunter"))));
    }
}
