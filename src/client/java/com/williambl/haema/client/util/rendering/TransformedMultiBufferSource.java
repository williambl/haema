package com.williambl.haema.client.util.rendering;

import com.mojang.blaze3d.vertex.DefaultedVertexConsumer;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.williambl.haema.client.vampire.ability.powers.vision.GlowEffectManager;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.OverlayTexture;

import java.util.function.Supplier;
import java.util.function.UnaryOperator;

/**
 * A {@link VertexConsumerProvider} which provides vertex consumers which render auras. It only renders for render layers
 * which affect the outline of an entity.
 *
 * @see net.minecraft.client.render.OutlineVertexConsumerProvider
 */
public final class TransformedMultiBufferSource implements MultiBufferSource {
    private final Supplier<RenderType> defaultRenderType;
    private final UnaryOperator<RenderType> renderTypeTransformer;
    private final MultiBufferSource provider;
    private final int r;
    private final int g;
    private final int b;
    private final int a;

    /**
     * Create a new AuraVertexConsumerProvider with a given base VertexConsumerProvider and aura colour.
     *
     * @param provider  the base VertexConsumerProvider
     * @param r         the aura's red component
     * @param g         the aura's green component
     * @param b         the aura's blue component
     * @param a         the aura's alpha component
     */
    public TransformedMultiBufferSource(Supplier<RenderType> defaultRenderType, UnaryOperator<RenderType> renderTypeTransformer, MultiBufferSource provider, int r, int g, int b, int a) {
        this.defaultRenderType = defaultRenderType;
        this.renderTypeTransformer = renderTypeTransformer;
        this.provider = provider;
        this.r = r;
        this.g = g;
        this.b = b;
        this.a = a;
    }

    /**
     * Gets a vertex consumer which renders an aura with the default texture.
     *
     * @return the vertex consumer
     */
    public VertexConsumer getBuffer() {
        return new AuraVertexConsumer(provider.getBuffer(this.defaultRenderType.get()), r, g, b, a);
    }

    @Override
    public VertexConsumer getBuffer(RenderType type) {
        if (type.outline().isPresent()) {
            return new AuraVertexConsumer(provider.getBuffer(this.renderTypeTransformer.apply(type)), r, g, b, a);
        } else {
            return new DummyVertexConsumer();
        }
    }

    /**
     * A dummy vertex consumer which does nothing. Used for render layers which do not affect the outline of an entity.
     */
    private final static class DummyVertexConsumer implements VertexConsumer {
        @Override
        public VertexConsumer vertex(double x, double y, double z) {
            return this;
        }

        @Override
        public VertexConsumer color(int red, int green, int blue, int alpha) {
            return this;
        }

        @Override
        public VertexConsumer uv(float u, float v) {
            return this;
        }

        @Override
        public VertexConsumer overlayCoords(int u, int v) {
            return this;
        }

        @Override
        public VertexConsumer uv2(int u, int v) {
            return this;
        }

        @Override
        public VertexConsumer normal(float x, float y, float z) {
            return this;
        }

        @Override
        public void endVertex() {
        }

        @Override
        public void defaultColor(int red, int green, int blue, int alpha) {
        }

        @Override
        public void unsetDefaultColor() {
        }
    }

    /**
     * The aura vertex consumer.
     */
    private final static class AuraVertexConsumer extends DefaultedVertexConsumer {
        private final VertexConsumer delegate;
        private double x;
        private double y;
        private double z;
        private float u;
        private float v;

        AuraVertexConsumer(VertexConsumer delegate, int red, int green, int blue, int alpha) {
            this.delegate = delegate;
            super.defaultColor(red, green, blue, alpha);
        }

        @Override
        public void defaultColor(int red, int green, int blue, int alpha) {
        }

        @Override
        public void unsetDefaultColor() {
        }

        @Override
        public VertexConsumer vertex(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
            return this;
        }

        @Override
        public VertexConsumer color(int red, int green, int blue, int alpha) {
            return this;
        }

        @Override
        public VertexConsumer uv(float u, float v) {
            this.u = u;
            this.v = v;
            return this;
        }

        @Override
        public VertexConsumer overlayCoords(int u, int v) {
            return this;
        }

        @Override
        public VertexConsumer uv2(int u, int v) {
            return this;
        }

        @Override
        public VertexConsumer normal(float x, float y, float z) {
            return this;
        }

        @Override
        public void vertex(float x, float y, float z, float red, float green, float blue, float alpha, float u, float v, int overlay, int light, float normalX, float normalY, float normalZ) {
            this.delegate.vertex(x, y, z).color(this.defaultR, this.defaultG, this.defaultB, this.defaultA).uv(u, v).overlayCoords(overlay).uv2(light).normal(normalX, normalY, normalZ).endVertex();
        }

        @Override
        public void endVertex() {
            this.delegate.vertex(this.x, this.y, this.z).color(this.defaultR, this.defaultG, this.defaultB, this.defaultA).uv(this.u, this.v).overlayCoords(OverlayTexture.NO_OVERLAY).uv2(LightTexture.FULL_BRIGHT).normal(0f, 0f, 0f).endVertex();
        }
    }
}