package com.williambl.haema.ability

import com.williambl.haema.ability.component.dash.DashAbilityComponent
import com.williambl.haema.ability.component.dash.EntityDashAbilityComponent
import com.williambl.haema.ability.component.invisibility.EntityInvisibilityAbilityComponent
import com.williambl.haema.ability.component.invisibility.InvisibilityAbilityComponent
import com.williambl.haema.ability.component.mist_form.EntityMistFormAbilityComponent
import com.williambl.haema.ability.component.mist_form.MistFormAbilityComponent
import com.williambl.haema.ability.component.strength.EntityStrengthAbilityComponent
import com.williambl.haema.ability.component.strength.StrengthAbilityComponent
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.id
import com.williambl.haema.ritual.RitualTableScreenHandler
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer
import dev.onyxstudios.cca.api.v3.entity.RespawnCopyStrategy
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.command.v2.ArgumentTypeRegistry
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder
import net.fabricmc.fabric.api.event.registry.RegistryAttribute
import net.fabricmc.fabric.api.networking.v1.PacketSender
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DefaultParticleType
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions
import net.minecraft.registry.DefaultedRegistry
import net.minecraft.registry.Registries
import net.minecraft.registry.Registry
import net.minecraft.server.MinecraftServer
import net.minecraft.server.network.ServerPlayNetworkHandler
import net.minecraft.server.network.ServerPlayerEntity



object AbilityModule: ModInitializer, EntityComponentInitializer {
    val ABILITY_REGISTRY: DefaultedRegistry<VampireAbility> = FabricRegistryBuilder.createDefaulted(VampireAbility::class.java, id("ability"), id("none")).attribute(RegistryAttribute.SYNCED).buildAndRegister()

    val NONE: VampireAbility = Registry.register(ABILITY_REGISTRY, id("none"), VampireAbility())
    val STRENGTH: VampireAbility = Registry.register(ABILITY_REGISTRY, id("strength"), VampireAbility(3, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.STRENGTH)))
    val DASH: VampireAbility = Registry.register(ABILITY_REGISTRY, id("dash"), VampireAbility(3, ItemStack(Items.FEATHER)))
    val INVISIBILITY: VampireAbility = Registry.register(ABILITY_REGISTRY, id("invisibility"), VampireAbility(2, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.INVISIBILITY)))
    val IMMORTALITY: VampireAbility = Registry.register(ABILITY_REGISTRY, id("immortality"), VampireAbility(1, ItemStack(Items.TOTEM_OF_UNDYING)))
    val VISION: VampireAbility = Registry.register(ABILITY_REGISTRY, id("vision"), VampireAbility(1, ItemStack(Items.ENDER_EYE)))
    val MIST_FORM: VampireAbility = Registry.register(ABILITY_REGISTRY, id("mist_form"), VampireAbility(1, ItemStack(Items.COBWEB)))

    val MIST_PARTICLE = Registry.register(Registries.PARTICLE_TYPE, id("mist"), object : DefaultParticleType(false) {})

    override fun onInitialize() {
        ArgumentTypeRegistry.registerArgumentType(
            id("ability"),
            VampireAbilityArgumentType::class.java,
            VampireAbilityArgumentType.SERIALISER
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
            server.execute {
                DashAbilityComponent.entityKey.get(player).dash()
            }
        }

        ServerPlayNetworking.registerGlobalReceiver(id("mist_form")) { server, player, networkHandler, buf, sender ->
            server.execute {
                if (player.getAbilityLevel(MIST_FORM) > 0) {
                    MistFormAbilityComponent.entityKey[player].toggleMistForm()
                }
            }
        }

        ServerPlayNetworking.registerGlobalReceiver(id("expand_mist_form")) { server, player, networkHandler, buf, sender ->
            server.execute {
                MistFormAbilityComponent.entityKey.get(player).run {
                    if (isInMistForm) {
                        expandMist()
                    }
                }
            }
        }
    }

    override fun registerEntityComponentFactories(registry: EntityComponentFactoryRegistry) {
        registry.registerForPlayers(InvisibilityAbilityComponent.entityKey, { player -> EntityInvisibilityAbilityComponent(player) }, RespawnCopyStrategy.NEVER_COPY)
        registry.registerForPlayers(StrengthAbilityComponent.entityKey, { player -> EntityStrengthAbilityComponent(player) }, RespawnCopyStrategy.NEVER_COPY)
        registry.registerForPlayers(MistFormAbilityComponent.entityKey, { player -> EntityMistFormAbilityComponent(player) }, RespawnCopyStrategy.NEVER_COPY)
        registry.registerForPlayers(DashAbilityComponent.entityKey, { player -> EntityDashAbilityComponent(player) }, RespawnCopyStrategy.NEVER_COPY)
    }
}