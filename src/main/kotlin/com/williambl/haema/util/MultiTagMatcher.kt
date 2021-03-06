package com.williambl.haema.util

import net.minecraft.block.Block
import net.minecraft.block.BlockState
import net.minecraft.block.Blocks
import net.minecraft.state.property.Property
import net.minecraft.tag.Tag.Identified
import net.minecraft.util.math.BlockPos
import net.minecraft.world.BlockView
import vazkii.patchouli.api.IStateMatcher
import vazkii.patchouli.api.TriPredicate
import java.util.*

class MultiTagMatcher constructor(
    private val tags: Collection<Identified<Block>>,
    private val props: Map<Property<*>, Any>
) :
    IStateMatcher {
    override fun getDisplayedState(ticks: Int): BlockState {
        val all = tags.flatMap { it.values() }
        return if (all.isEmpty()) {
            Blocks.BEDROCK.defaultState // show something impossible
        } else {
            val idx = ticks / 20 % all.size
            all[idx].defaultState
        }
    }

    override fun getStatePredicate(): TriPredicate<BlockView, BlockPos, BlockState> {
        return TriPredicate { w: BlockView?, p: BlockPos?, s: BlockState ->
            tags.any { it.contains(
                s.block
            )} && checkProps(s)
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

    override fun equals(o: Any?): Boolean {
        if (this === o) {
            return true
        }
        if (o == null || javaClass != o.javaClass) {
            return false
        }
        o as MultiTagMatcher
        return this.tags.all { o.tags.any { that -> it.id == that.id } } && this.props == o.props
    }

    override fun hashCode(): Int {
        return Objects.hash(tags.map { it.id.toString() }.reduce { acc, id -> acc + id }, props)
    }
}