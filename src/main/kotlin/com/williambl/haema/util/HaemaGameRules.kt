package com.williambl.haema.util

import net.fabricmc.fabric.api.gamerule.v1.rule.DoubleRule
import net.minecraft.world.GameRules

lateinit var vampiresBurn: GameRules.Key<GameRules.BooleanRule>

lateinit var feedCooldown: GameRules.Key<GameRules.IntRule>

lateinit var dashCooldown: GameRules.Key<GameRules.IntRule>

lateinit var vampireHunterNoticeChance: GameRules.Key<DoubleRule>

lateinit var playerVampireConversion: GameRules.Key<GameRules.BooleanRule>
