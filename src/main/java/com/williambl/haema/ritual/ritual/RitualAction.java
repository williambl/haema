package com.williambl.haema.ritual.ritual;

import net.minecraft.util.KeyDispatchDataCodec;

public interface RitualAction {

    KeyDispatchDataCodec<? extends RitualAction> codec();
}
