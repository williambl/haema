package com.williambl.haema.ability

import com.williambl.haema.ability.component.InvisibilityAbilityComponent
import com.williambl.haema.ability.component.EntityInvisibilityAbilityComponent
import com.williambl.haema.criteria.UseDashCriterion
import com.williambl.haema.id
import com.williambl.haema.mixin.RegistryAccessor
import com.williambl.haema.ritual.RitualTableScreenHandler
import com.williambl.haema.util.raytraceForDash
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer
import dev.onyxstudios.cca.api.v3.entity.RespawnCopyStrategy
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.networking.v1.PacketSender
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking
import net.minecraft.command.argument.ArgumentTypes
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions
import net.minecraft.server.MinecraftServer
import net.minecraft.server.network.ServerPlayNetworkHandler
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.util.registry.DefaultedRegistry
import net.minecraft.util.registry.Registry
import net.minecraft.util.registry.RegistryKey

object AbilityModule: ModInitializer, EntityComponentInitializer {
    val ABILITY_REGISTRY_KEY: RegistryKey<Registry<VampireAbility>> = RegistryAccessor.createRegistryKey("haema:ability")
    val ABILITY_REGISTRY: DefaultedRegistry<VampireAbility> = RegistryAccessor.create(ABILITY_REGISTRY_KEY, "haema:none") { NONE } // TODO: a) does this work b) does NONE need to be registered c) does fabric have an api for this? think so

    val NONE: VampireAbility = Registry.register(ABILITY_REGISTRY, id("none"), VampireAbility())
    val STRENGTH: VampireAbility = Registry.register(ABILITY_REGISTRY, id("strength"), VampireAbility(3, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.STRENGTH)))
    val DASH: VampireAbility = Registry.register(ABILITY_REGISTRY, id("dash"), VampireAbility(3, ItemStack(Items.FEATHER)))
    val INVISIBILITY: VampireAbility = Registry.register(ABILITY_REGISTRY, id("invisibility"), VampireAbility(2, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.INVISIBILITY)))
    val IMMORTALITY: VampireAbility = Registry.register(ABILITY_REGISTRY, id("immortality"), VampireAbility(1, ItemStack(Items.TOTEM_OF_UNDYING)))
    val VISION: VampireAbility = Registry.register(ABILITY_REGISTRY, id("vision"), VampireAbility(1, ItemStack(Items.ENDER_EYE)))
    val MIST_FORM: VampireAbility = Registry.register(ABILITY_REGISTRY, id("mist_form"), VampireAbility(1, ItemStack(Items.COBWEB)))

    override fun onInitialize() {
        ArgumentTypes.register(
            "haema:ability",
            VampireAbilityArgumentType::class.java,
            VampireAbilityArgumentType.Serialiser
        )

        ServerPlayNetworking.registerGlobalReceiver(id("transferlevels")) { server: MinecraftServer, player: ServerPlayerEntity, networkHandler: ServerPlayNetworkHandler, buf: PacketByteBuf, sender: PacketSender ->
            if (player.currentScreenHandler.syncId == buf.readVarInt() && player.currentScreenHandler is RitualTableScreenHandler) {
                val amount = buf.readVarInt()
                val from = buf.readVarInt()
                val to = buf.readVarInt()
                val currentAmountFrom = (player.currentScreenHandler as RitualTableScreenHandler).getProperty(from)
                val currentAmountTo = (player.currentScreenHandler as RitualTableScreenHandler).getProperty(to)

                if (currentAmountFrom - amount >= 0 && currentAmountTo + amount <= ABILITY_REGISTRY[to].maxLevel) {
                    player.currentScreenHandler.setProperty(from, currentAmountFrom - amount)
                    player.currentScreenHandler.setProperty(to, currentAmountTo + amount)
                }
            }
        }

        ServerPlayNetworking.registerGlobalReceiver(id("dash")) { server: MinecraftServer, player: ServerPlayerEntity, networkHandler: ServerPlayNetworkHandler, buf: PacketByteBuf, sender: PacketSender ->
            val world = player.world
            val target = raytraceForDash(player)

            //TODO: optimise?
            //TODO: don't trust the client
            if (target != null) {
                val rand = world.random
                for (j in 0 until 3) {
                    val x: Double = (target.x - player.x) * rand.nextDouble() + player.x - 0.5
                    val y: Double = (target.y - player.y) * rand.nextDouble() + player.y + 1
                    val z: Double = (target.z - player.z) * rand.nextDouble() + player.z - 0.5
                    (world as ServerWorld).spawnParticles(
                        DustParticleEffect.DEFAULT,
                        x, y, z,
                        10,
                        0.5, 1.0, 0.5,
                        0.0
                    )
                }
                world.playSound(
                    null,
                    target.x,
                    target.y,
                    target.z,
                    SoundEvents.ENTITY_GHAST_SHOOT,
                    SoundCategory.PLAYERS,
                    1f,
                    1.5f
                )
                player.teleport(target.x, target.y, target.z)
                UseDashCriterion.trigger(player)
            }
        }
    }

    override fun registerEntityComponentFactories(registry: EntityComponentFactoryRegistry) {
        registry.registerForPlayers(InvisibilityAbilityComponent.entityKey, { player -> EntityInvisibilityAbilityComponent(player) }, RespawnCopyStrategy.NEVER_COPY)
    }
}