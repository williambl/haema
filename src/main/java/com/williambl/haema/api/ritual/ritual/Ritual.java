package com.williambl.haema.api.ritual.ritual;

import com.mojang.datafixers.util.Either;
import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.DFContext;
import com.williambl.dfunc.api.context.DFContextSpec;
import com.williambl.dfunc.api.functions.DPredicates;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.RitualArae;
import net.minecraft.core.*;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.FluidTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.level.material.Fluid;
import xyz.nucleoid.codecs.MoreCodecs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static com.williambl.haema.Haema.id;

//TODO FUCK THIS, MAKE IT NOT A RECIPE, FUCK RECIPES, ALL MY HOMIES HATE RECIPES
public record Ritual(
        TagKey<Fluid> fluid,
        List<Ingredient> ingredients,
        HolderSet<RitualArae> acceptableAraes,
        DFunction<Boolean> canPlayerUse,
        List<RitualAction> actions,
        RitualTrigger trigger
) {
    public static final ResourceKey<Registry<Ritual>> REGISTRY_KEY = ResourceKey.createRegistryKey(id( "ritual"));

    public static final Codec<Ritual> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            TagKey.codec(Registries.FLUID).fieldOf("fluid").forGetter(Ritual::fluid),
            MoreCodecs.INGREDIENT.listOf().fieldOf("ingredients").forGetter(Ritual::ingredients),
            RegistryCodecs.homogeneousList(RitualArae.REGISTRY_KEY).fieldOf("acceptable_araes").forGetter(Ritual::acceptableAraes),
            DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY), Function.identity()).fieldOf("can_player_use").forGetter(Ritual::canPlayerUse),
            RitualAction.ACTION_CODEC.listOf().fieldOf("actions").forGetter(Ritual::actions),
            RitualTrigger.TRIGGER_CODEC.fieldOf("trigger").forGetter(Ritual::trigger)
    ).apply(instance, Ritual::new));

    public boolean matches(RitualContainer container, RegistryAccess registries) {
        return this.canPlayerUse().apply(DFContext.entity(container.player()))
                && this.isAraeAcceptable(container.arae(), registries.registryOrThrow(RitualArae.REGISTRY_KEY))
                && HaemaUtil.allMatchOne(container.itemsCopy(), this.ingredients()) != null
                && container.fluid().is(this.fluid());
    }

    public boolean isAraeAcceptable(RitualArae toCheck, Registry<RitualArae> registry) {
        return this.acceptableAraes().contains(registry.wrapAsHolder(toCheck));
    }

    public void runActions(RitualContainer container) {
        this.actions().forEach(r -> r.run(this, container));
    }

    public static Iterable<Ritual> getRituals(RegistryAccess registries) {
        return registries.registryOrThrow(REGISTRY_KEY);
    }

    public static <T> Stream<Ritual> findRituals(RegistryAccess registries, Class<T> triggerClass, Predicate<T> triggerPredicate) {
        return registries.registryOrThrow(REGISTRY_KEY).stream()
                .filter(r -> triggerClass.isInstance(r.trigger()))
                .filter(r -> triggerPredicate.test(triggerClass.cast(r.trigger())));
    }

    public static <T> Stream<Ritual> findRituals(RegistryAccess registries, Class<T> triggerClass, Predicate<T> triggerPredicate, RitualContainer container) {
        return findRituals(registries, triggerClass, triggerPredicate)
                .filter(r -> r.matches(container, registries));
    }

    public static class Builder {
        private final HolderLookup.Provider registries;
        private final List<Ingredient> ingredients = new ArrayList<>();
        private final List<RitualAction> actions = new ArrayList<>();
        private DFunction<Boolean> canPlayerUse = DPredicates.CONSTANT.factory().apply(true);
        private TagKey<Fluid> fluid = FluidTags.WATER;
        private Either<TagKey<RitualArae>, List<ResourceKey<RitualArae>>> acceptableAraes = null;
        private RitualTrigger trigger = null;

        public static Builder ritual(HolderLookup.Provider registries) {
            return new Builder(registries);
        }

        private Builder(HolderLookup.Provider registries) {
            this.registries = registries;
        }

        public Builder fluid(TagKey<Fluid> fluid) {
            this.fluid = fluid;
            return this;
        }

        @SafeVarargs
        public final Builder acceptableAraes(ResourceKey<RitualArae>... araes) {
            this.acceptableAraes = Either.right(Arrays.asList(araes));
            return this;
        }

        public Builder acceptableAraes(Collection<ResourceKey<RitualArae>> araes) {
            this.acceptableAraes = Either.right(List.copyOf(araes));
            return this;
        }

        public Builder acceptableAraes(TagKey<RitualArae> tag) {
            this.acceptableAraes = Either.left(tag);
            return this;
        }

        public Builder canPlayerUse(DFunction<Boolean> dfunction) {
            this.canPlayerUse = dfunction;
            return this;
        }

        public Builder trigger(RitualTrigger trigger) {
            this.trigger = trigger;
            return this;
        }

        public Builder ingredient(Ingredient ingredient) {
            this.ingredients.add(ingredient);
            return this;
        }

        public Builder action(RitualAction action) {
            this.actions.add(action);
            return this;
        }

        private void ensureValid(ResourceLocation resourceLocation) {
            if (this.acceptableAraes == null) {
                throw new IllegalStateException("No acceptable araes are set for ritual "+resourceLocation);
            }
            if (this.trigger == null) {
                throw new IllegalStateException("No trigger is set for ritual "+resourceLocation);
            }
        }

        @SuppressWarnings("deprecation")
        public Holder<Ritual> build(BiFunction<ResourceKey<Ritual>, Ritual, Holder<Ritual>> registrar, ResourceLocation name) {
            this.ensureValid(name);
            var araeRegistry = this.registries.lookupOrThrow(RitualArae.REGISTRY_KEY);
            return registrar.apply(
                    ResourceKey.create(REGISTRY_KEY, name),
                    new Ritual(
                            this.fluid,
                            this.ingredients,
                            // note: emptyNamed is deprecated + marked only visible for testing
                            this.acceptableAraes.map(
                                    t -> HolderSet.emptyNamed(araeRegistry, t),
                                    l -> HolderSet.direct(l.stream().map(k -> Holder.Reference.createStandAlone(araeRegistry, k)).toList())),
                            this.canPlayerUse,
                            this.actions,
                            this.trigger));
        }
    }
}
