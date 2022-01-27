package com.williambl.haema.ability.component.mist_form

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.id
import net.fabricmc.fabric.api.networking.v1.PacketByteBufs
import net.fabricmc.fabric.api.networking.v1.PlayerLookup
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking
import net.minecraft.entity.LivingEntity
import net.minecraft.nbt.NbtCompound
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.network.ServerPlayerEntity
import virtuoel.pehkui.api.*
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

class EntityMistFormAbilityComponent(val entity: LivingEntity): MistFormAbilityComponent {
    private val syncCallback = { _: KProperty<*>, _: Any?, _: Any? ->
        if (!entity.world.isClient) {
            MistFormAbilityComponent.entityKey.sync(entity)
        }
    }

    private var mistFormTicks: Int = 0

    override var isInMistForm: Boolean by Delegates.observable(false, syncCallback)

    override fun toggleMistForm() {
        super.toggleMistForm()
        MODIFY_HEIGHT_TYPE.getScaleData(entity).run {
            targetScale = if (isInMistForm) 0.2f else 1.0f
            scaleTickDelay = 3
        }
        if (isInMistForm) {
            PlayerLookup.tracking(entity).forEach { p ->
                ServerPlayNetworking.send(
                    p,
                    id("start_mist_form"),
                    PacketByteBufs.create().writeVarInt(entity.id)
                )
            }
        }
    }

    override fun shouldRenderAsFullMistForm(): Boolean = isInMistForm && mistFormTicks > 3

    override fun serverTick() {
        if (isInMistForm) {
            mistFormTicks++
        } else {
            mistFormTicks = 0
        }
    }

    override fun clientTick() {
        if (isInMistForm) {
            mistFormTicks++
        } else {
            mistFormTicks = 0
        }

        if (isInMistForm) {
            val rand = entity.random
            val dims = entity.getDimensions(entity.pose)
            for (i in 0..30) {
                entity.world.addParticle(
                    AbilityModule.MIST_PARTICLE,
                    entity.x + rand.nextGaussian() * dims.width/2,
                    entity.randomBodyY,
                    entity.z + rand.nextGaussian() * dims.width/2,
                    rand.nextGaussian() * 0.12,
                    rand.nextGaussian() * 0.12,
                    rand.nextGaussian() * 0.12
                )
            }
        }
    }

    override fun writeSyncPacket(buf: PacketByteBuf, recipient: ServerPlayerEntity) {
        buf.writeBoolean(isInMistForm)
    }

    override fun applySyncPacket(buf: PacketByteBuf) {
        isInMistForm = buf.readBoolean()
    }

    override fun writeToNbt(tag: NbtCompound) {
        tag.putBoolean("isInMistForm", isInMistForm)
    }

    override fun readFromNbt(tag: NbtCompound) {
        isInMistForm = tag.getBoolean("isInMistForm")
    }

    companion object {
        val HEIGHT_MULTIPLIER: ScaleModifier? = ScaleRegistries.register(ScaleRegistries.SCALE_MODIFIERS, id("mist_form_height_multiplier"), TypedScaleModifier { MODIFY_HEIGHT_TYPE })
        val MODIFY_HEIGHT_TYPE: ScaleType =
            ScaleRegistries.register(
                ScaleRegistries.SCALE_TYPES,
                id("mist_form_modify_height"),
                ScaleType.Builder.create().addDependentModifier(HEIGHT_MULTIPLIER)
                    .affectsDimensions().build()
            )
        init {
            ScaleTypes.HEIGHT.defaultBaseValueModifiers.add(HEIGHT_MULTIPLIER)
        }
    }
}
