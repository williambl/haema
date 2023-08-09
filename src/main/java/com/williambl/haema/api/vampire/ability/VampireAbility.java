package com.williambl.haema.api.vampire.ability;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;

import java.util.List;
import java.util.Set;

import static com.williambl.haema.Haema.id;

/**
 * A vampire ability. This has a collection of 'powers', which are granted to the player when the ability is applied.
 * <p>
 * If you are familiar with Origins, this is <em>somewhat</em> similar to an Origin, in that it holds a list of powers.
 * However, a player may have many abilities applied at once, unlike Origins, which can only have one per 'layer'.
 *
 * @param enabled           whether the ability should be available in-game
 * @param canPlayerModify   a predicate for whether a player should be able to apply/remove this ability
 * @param prerequisites     the abilities that must be applied for this ability to be applied
 * @param conflicts         the abilities that cannot be applied at the same time as this ability
 * @param supercedes        the abilities that are superceded by this one (they will no longer have an effect when this one is active)
 * @param powers            the powers that are granted when this ability is applied
 */
public record VampireAbility(boolean enabled,
                             VExpression canPlayerModify,
                             Set<ResourceKey<VampireAbility>> prerequisites,
                             Set<ResourceKey<VampireAbility>> conflicts,
                             Set<ResourceKey<VampireAbility>> supercedes, //TODO do something with this
                             List<VampireAbilityPower> powers) {

    public static final ResourceKey<Registry<VampireAbility>> REGISTRY_KEY = ResourceKey.createRegistryKey(id( "vampire_ability"));

    public static final Codec<VampireAbility> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            Codec.BOOL.fieldOf("enabled").forGetter(VampireAbility::enabled),
            DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).fieldOf("can_player_modify").forGetter(VampireAbility::canPlayerModify),
            ResourceKey.codec(REGISTRY_KEY).listOf().fieldOf("prerequisites").xmap(Set::copyOf, List::copyOf).forGetter(VampireAbility::prerequisites),
            ResourceKey.codec(REGISTRY_KEY).listOf().fieldOf("conflicts").xmap(Set::copyOf, List::copyOf).forGetter(VampireAbility::conflicts),
            ResourceKey.codec(REGISTRY_KEY).listOf().fieldOf("supercedes").xmap(Set::copyOf, List::copyOf).forGetter(VampireAbility::supercedes),
            VampireAbilityPower.POWER_CODEC.listOf().fieldOf("powers").forGetter(VampireAbility::powers)
    ).apply(instance, VampireAbility::new));
}
