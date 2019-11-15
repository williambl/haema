package com.williambl.haema.objectholder;

import com.williambl.haema.common.capability.ICapabilityVampirism;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;

public class ModCapabilityHolder {

    @CapabilityInject(ICapabilityVampirism.class)
    public static Capability<ICapabilityVampirism> vampirism;
}
