package com.williambl.haema.ability.component.mist_form

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.criteria.UseMistCriterion
import com.williambl.haema.effect.EffectsModule
import com.williambl.haema.effect.MistFormEffect
import com.williambl.haema.id
import com.williambl.haema.isVampire
import net.fabricmc.fabric.api.networking.v1.PacketByteBufs
import net.fabricmc.fabric.api.networking.v1.PlayerLookup
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.effect.StatusEffectInstance
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
    private var mistFormCooldownTime: Long by Delegates.observable(0, syncCallback)

    private var mistExpansionTime: Long by Delegates.observable(-1, syncCallback)
    private var mistExpansionCooldownTime: Long by Delegates.observable(0, syncCallback)

    override fun toggleMistForm() {
        if (!this.canUseMistForm()) {
            return
        }

        super.toggleMistForm()
        MODIFY_HEIGHT_TYPE.getScaleData(entity).run {
            targetScale = if (isInMistForm) 0.2f else 1.0f
            scaleTickDelay = 3
        }
        if (isInMistForm) {
            if (entity is ServerPlayerEntity) {
                UseMistCriterion.trigger(entity)
            }

            PlayerLookup.tracking(entity).forEach { p ->
                ServerPlayNetworking.send(
                    p,
                    id("start_mist_form"),
                    PacketByteBufs.create().writeVarInt(entity.id)
                )
            }
        }
        this.mistFormCooldownTime = (this.entity.world.time + MIST_FORM_COOLDOWN)
    }

    override fun shouldRenderAsFullMistForm(): Boolean = isInMistForm && mistFormTicks > 3

    override fun canUseMistForm(): Boolean = this.isInMistForm || this.mistFormCooldownTime < this.entity.world.time

    override fun expandMist() {
        if (!this.canExpandMist()) {
            return
        }

        this.mistExpansionTime = this.entity.world.time
        this.mistExpansionCooldownTime = (this.entity.world.time + MIST_EXPANSION_COOLDOWN + MIST_EXPANSION_SUSTAIN_TIME + MIST_EXPANSION_LERP_TIME * 2).toLong()
    }

    private fun getBigMistBoundingBox(): Box = this.entity.boundingBox.expand(this.getBoundingBoxExpansionFactor())

    override fun isMistExpanded(): Boolean = this.mistExpansionTime >= 0 && (this.entity.world.time-this.mistExpansionTime) <= MIST_EXPANSION_SUSTAIN_TIME + MIST_EXPANSION_LERP_TIME * 2

    override fun canExpandMist(): Boolean = this.mistExpansionCooldownTime < this.entity.world.time

    override fun serverTick() {
        if (!this.entity.isVampire || this.entity.isSpectator) {
            this.isInMistForm = false
            MODIFY_HEIGHT_TYPE.getScaleData(entity).run {
                targetScale = 1.0f
                scaleTickDelay = 3
            }
        }

        if (isInMistForm) {
            mistFormTicks++
            entity.addStatusEffect(StatusEffectInstance(MistFormEffect.instance, 80, 0, false, false, true))
            this.attackInBox(this.entity.boundingBox)

            if (isMistExpanded()) {
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

            if (isMistExpanded()) {
                val boundingBox = getBigMistBoundingBox()

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
        buf.writeVarLong(mistExpansionTime)
        buf.writeVarLong(mistExpansionCooldownTime)
        buf.writeVarLong(mistFormCooldownTime)
    }

    override fun applySyncPacket(buf: PacketByteBuf) {
        isInMistForm = buf.readBoolean()
        mistExpansionTime = buf.readVarLong()
        mistExpansionCooldownTime = buf.readVarLong()
        mistFormCooldownTime = buf.readVarLong()
    }

    override fun writeToNbt(tag: NbtCompound) {
        tag.putBoolean("isInMistForm", isInMistForm)
    }

    override fun readFromNbt(tag: NbtCompound) {
        isInMistForm = tag.getBoolean("isInMistForm")
    }

    private fun attackInBox(box: Box) {
        this.entity.world.getOtherEntities(this.entity, box).forEach {
            if (it !is LivingEntity || it.hasStatusEffect(EffectsModule.MISTED)) return@forEach

            it.addStatusEffect(StatusEffectInstance(EffectsModule.MISTED, 50))
        }
    }

    private fun getBoundingBoxExpansionFactor(): Double = (this.entity.world.time-this.mistExpansionTime).let { t ->
        if (t <= MIST_EXPANSION_LERP_TIME) {
            (sin((t/MIST_EXPANSION_LERP_TIME)*PI-(PI/2.0))+1.0)*(MIST_EXPANSION_FACTOR/2.0)
        } else if (t <= MIST_EXPANSION_LERP_TIME+ MIST_EXPANSION_SUSTAIN_TIME) {
            MIST_EXPANSION_FACTOR
        } else if (t <= MIST_EXPANSION_LERP_TIME*2+ MIST_EXPANSION_SUSTAIN_TIME) {
            (sin((t/MIST_EXPANSION_LERP_TIME)*PI+PI)+1.0)*(MIST_EXPANSION_FACTOR/2.0)
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

        const val MIST_EXPANSION_LERP_TIME = 80.0
        const val MIST_EXPANSION_SUSTAIN_TIME = 20.0
        const val MIST_EXPANSION_FACTOR = 10.0

        const val MIST_EXPANSION_COOLDOWN = 600

        const val MIST_FORM_COOLDOWN = 1200

        init {
            ScaleTypes.HEIGHT.defaultBaseValueModifiers.add(HEIGHT_MULTIPLIER)
        }
    }
}
