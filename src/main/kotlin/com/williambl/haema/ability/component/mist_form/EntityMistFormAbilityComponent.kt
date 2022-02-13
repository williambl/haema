package com.williambl.haema.ability.component.mist_form

import com.williambl.haema.Haema
import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.effect.MistFormEffect
import com.williambl.haema.effect.VampiricStrengthEffect
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.id
import net.fabricmc.fabric.api.networking.v1.PacketByteBufs
import net.fabricmc.fabric.api.networking.v1.PlayerLookup
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.nbt.NbtCompound
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.math.Box
import virtuoel.pehkui.api.*
import kotlin.math.PI
import kotlin.math.sin
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

    private var bigMistActivationAge: Int by Delegates.observable(-1, syncCallback)

    override fun activateBigMist() {
        this.bigMistActivationAge = this.entity.age
    }

    override fun getBigMistBoundingBox(): Box =
        this.entity.boundingBox.expand(this.getBoundingBoxExpansionFactor())

    private fun isDoingBigMist(): Boolean = this.bigMistActivationAge >= 0 && (this.entity.age-this.bigMistActivationAge) <= BIG_MIST_SUSTAIN_TIME + BIG_MIST_LERP_TIME * 2

    override fun serverTick() {
        if (isInMistForm) {
            mistFormTicks++
            entity.addStatusEffect(StatusEffectInstance(MistFormEffect.instance, 80, 0, false, false, true))
            this.attackInBox(this.entity.boundingBox)

            if (isDoingBigMist()) {
                this.attackInBox(getBigMistBoundingBox())
            }
        } else {
            mistFormTicks = 0
            entity.removeStatusEffect(MistFormEffect.instance)
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

            if (isDoingBigMist()) {
                val boundingBox = getBigMistBoundingBox()

                Haema.LOGGER.debug(this.entity.age-this.bigMistActivationAge)

                val particleCount = ((boundingBox.xLength/this.entity.boundingBox.xLength) * 60).toInt()

                for (i in 0..particleCount) {
                    entity.world.addParticle(
                        AbilityModule.MIST_PARTICLE,
                        boundingBox.minX + rand.nextDouble() * boundingBox.xLength,
                        boundingBox.minY + rand.nextDouble() * boundingBox.yLength,
                        boundingBox.minZ + rand.nextDouble() * boundingBox.xLength,
                        rand.nextGaussian() * 0.12,
                        rand.nextGaussian() * 0.12,
                        rand.nextGaussian() * 0.12
                    )
                }
            } else {
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
    }

    override fun writeSyncPacket(buf: PacketByteBuf, recipient: ServerPlayerEntity) {
        buf.writeBoolean(isInMistForm)
        buf.writeVarInt(bigMistActivationAge)
    }

    override fun applySyncPacket(buf: PacketByteBuf) {
        isInMistForm = buf.readBoolean()
        bigMistActivationAge = buf.readVarInt()
    }

    override fun writeToNbt(tag: NbtCompound) {
        tag.putBoolean("isInMistForm", isInMistForm)
    }

    override fun readFromNbt(tag: NbtCompound) {
        isInMistForm = tag.getBoolean("isInMistForm")
    }

    private fun attackInBox(box: Box) {
        this.entity.world.getOtherEntities(this.entity, box).forEach {
            if (it !is LivingEntity) return@forEach

            it.addStatusEffect(StatusEffectInstance(StatusEffects.POISON, 20))
            it.addStatusEffect(StatusEffectInstance(StatusEffects.BLINDNESS, 10))
        }
    }

    private fun getBoundingBoxExpansionFactor(): Double = (this.entity.age-this.bigMistActivationAge).let { t ->
        if (t <= BIG_MIST_LERP_TIME) {
            (sin((t/BIG_MIST_LERP_TIME)*PI-(PI/2.0))+1.0)*(MAX_BOUNDING_BOX_RADIUS/2.0)
        } else if (t <= BIG_MIST_LERP_TIME+ BIG_MIST_SUSTAIN_TIME) {
            MAX_BOUNDING_BOX_RADIUS
        } else if (t <= BIG_MIST_LERP_TIME*2+ BIG_MIST_SUSTAIN_TIME) {
            (sin((t/BIG_MIST_LERP_TIME)*PI+PI)+1.0)*(MAX_BOUNDING_BOX_RADIUS/2.0)
        } else {
            0.0
        }
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

        const val BIG_MIST_LERP_TIME = 80.0
        const val BIG_MIST_SUSTAIN_TIME = 20.0
        const val MAX_BOUNDING_BOX_RADIUS = 10.0

        init {
            ScaleTypes.HEIGHT.defaultBaseValueModifiers.add(HEIGHT_MULTIPLIER)
        }
    }
}
