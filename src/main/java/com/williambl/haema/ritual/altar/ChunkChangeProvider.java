package com.williambl.haema.ritual.altar;

// injected onto ServerLevel
public interface ChunkChangeProvider {
    long lastChunkChange(int chunkX, int chunkZ);
}
