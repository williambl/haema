package com.williambl.haema.vampiremobs

interface ReinforcementSpawner {
    val timesSpawnedReinforcements: Int
    fun spawnReinforcements()
}