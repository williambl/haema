package com.williambl.haema.mixin;

import com.google.common.collect.ImmutableList;
import com.williambl.haema.hunter.VampireHunterSpawner;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.gen.Spawner;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.ModifyVariable;

import java.util.List;

@Mixin(MinecraftServer.class)
public class MinecraftServerMixin {
    @ModifyVariable(method = "createWorlds", at = @At(value = "INVOKE_ASSIGN", target = "Lcom/google/common/collect/ImmutableList;of(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Lcom/google/common/collect/ImmutableList;"))
    List<Spawner> addVampireHunterSpawner(List<Spawner> list) {
        return ImmutableList.<Spawner>builder().addAll(list).add(new VampireHunterSpawner()).build();
    }
}
