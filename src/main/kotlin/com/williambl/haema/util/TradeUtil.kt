package com.williambl.haema.util

import net.minecraft.village.TradeOffers
import net.minecraft.village.VillagerProfession

fun addTradesToProfession(profession: VillagerProfession, level: Int, vararg newTrades: TradeOffers.Factory) {
    val trades = TradeOffers.PROFESSION_TO_LEVELED_TRADE[profession]?.get(level)?.toMutableList() ?: mutableListOf()
    trades.addAll(newTrades)
    TradeOffers.PROFESSION_TO_LEVELED_TRADE[profession]?.put(level, trades.toTypedArray())
}