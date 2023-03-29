package com.williambl.haema.util

import net.minecraft.block.Block
import net.minecraft.block.BlockState
import net.minecraft.block.Blocks
import net.minecraft.registry.Registries
import net.minecraft.registry.tag.TagKey
import net.minecraft.state.property.Property
import net.minecraft.util.math.BlockPos

import net.minecraft.world.BlockView
import vazkii.patchouli.api.IStateMatcher
import vazkii.patchouli.api.TriPredicate
import java.util.*

class MultiTagMatcher constructor(
    private val tags: Collection<TagKey<Block>>,
    private val props: Map<Property<*>, Any>
) :
    IStateMatcher {
    override fun getDisplayedState(ticks: Long): BlockState {
        val all = tags.flatMap { Registries.BLOCK.iterateEntries(it) }
        return if (all.isEmpty()) {
            Blocks.BEDROCK.defaultState // show something impossible
        } else {
            val idx = ticks / 20 % all.size
            all[idx.toInt()].value().defaultState
        }
    }

    override fun getStatePredicate(): TriPredicate<BlockView, BlockPos, BlockState> {
        return TriPredicate { w: BlockView?, p: BlockPos?, s: BlockState ->
            tags.any {
                s.isIn(it)
            } && checkProps(s)
        }
    }

    private fun checkProps(state: BlockState): Boolean {
        for ((prop, value) in props) {
            if (state.contains(prop) && state[prop] != value) {
                return false
            }
        }
        return true
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) {
            return true
        }
        if (other == null || javaClass != other.javaClass) {
            return false
        }
        other as MultiTagMatcher
        return this.tags.all { other.tags.any { that -> it.id == that.id } } && this.props == other.props
    }

    override fun hashCode(): Int {
        return Objects.hash(tags.map { it.id.toString() }.reduce { acc, id -> acc + id }, props)
    }
}