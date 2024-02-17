package com.williambl.haema.client.vampire.ability.powers.dash;

import com.mojang.blaze3d.pipeline.RenderTarget;
import com.mojang.blaze3d.platform.GlConst;
import com.mojang.blaze3d.platform.GlStateManager;
import com.mojang.blaze3d.shaders.BlendMode;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexFormat;
import com.williambl.haema.client.util.rendering.TransformedMultiBufferSource;
import ladysnake.satin.api.event.EntitiesPreRenderCallback;
import ladysnake.satin.api.event.PostWorldRenderCallback;
import ladysnake.satin.api.event.PostWorldRenderCallbackV2;
import ladysnake.satin.api.event.ShaderEffectRenderCallback;
import ladysnake.satin.api.managed.ManagedFramebuffer;
import ladysnake.satin.api.managed.ManagedShaderEffect;
import ladysnake.satin.api.managed.ShaderEffectManager;
import ladysnake.satin.api.util.RenderLayerHelper;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.minecraft.Util;
import net.minecraft.client.Camera;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderStateShard;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.culling.Frustum;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.NotNull;

import java.util.function.Function;

import static com.williambl.haema.Haema.id;
import static org.lwjgl.opengl.GL11.GL_COLOR_BUFFER_BIT;

/**
 * Manages the Glowing rendering effect. Glows are rendered to a separate framebuffer with a custom renderlayer and then
 * composited into the main framebuffer with a post shader. This uses similar techniques to
 * <a href="https://github.com/Ladysnake/Requiem/blob/ad68fa534c6b6d4c64be86162eb35e67140138c6/src/main/java/ladysnake/requiem/client/ShadowPlayerFx.java">Requiem's Shadow Player rendering</a>,
 * and, even moreso, the Aura rendering from Arcanus.
 */
public final class DashShimmerEffectManager implements EntitiesPreRenderCallback, PostWorldRenderCallbackV2, ClientTickEvents.StartTick {
	public static final DashShimmerEffectManager INSTANCE = new DashShimmerEffectManager();
	private final Minecraft client = Minecraft.getInstance();
	private final ManagedShaderEffect auraPostShader = ShaderEffectManager.getInstance().manage(id("shaders/post/dash_charge_shimmer.json"));
	private final ManagedFramebuffer auraFramebuffer = this.auraPostShader.getTarget("shimmers");
	private boolean auraBufferCleared;

	public void init() {
		EntitiesPreRenderCallback.EVENT.register(this);
		PostWorldRenderCallbackV2.EVENT.register(this);
		ClientTickEvents.START_CLIENT_TICK.register(this);
	}

	@Override
	public void beforeEntitiesRender(@NotNull Camera camera, @NotNull Frustum frustum, float tickDelta) {
		this.auraBufferCleared = false;
	}

	@Override
	public void onWorldRendered(PoseStack matrices, Camera camera, float tickDelta, long nanoTime) {
		if(this.auraBufferCleared) {
			this.auraPostShader.render(tickDelta);
			this.client.getMainRenderTarget().bindWrite(true);
			RenderSystem.enableBlend();
			RenderSystem.blendFuncSeparate(GlStateManager.SourceFactor.ONE, GlStateManager.DestFactor.ONE, GlStateManager.SourceFactor.ZERO, GlStateManager.DestFactor.ONE);
			this.auraFramebuffer.draw(client.getWindow().getScreenWidth(), client.getWindow().getScreenHeight(), false);
			RenderSystem.disableBlend();
		}
	}

	/**
	 * Binds aura framebuffer for use and clears it if necessary.
	 */
	public void beginGlowFramebufferUse() {
		RenderTarget auraFramebuffer = this.auraFramebuffer.getFramebuffer();

		if(auraFramebuffer != null) {
			auraFramebuffer.bindWrite(false);

			if(!this.auraBufferCleared) {
				// clear framebuffer colour and copy depth from
				float[] clearColor = auraFramebuffer.clearChannels;
				RenderSystem.clearColor(clearColor[0], clearColor[1], clearColor[2], clearColor[3]);
				RenderSystem.clear(GL_COLOR_BUFFER_BIT, Minecraft.ON_OSX);

				this.auraFramebuffer.copyDepthFrom(this.client.getMainRenderTarget());

				auraFramebuffer.bindWrite(false);
				this.auraBufferCleared = true;
			}
		}
	}


	@Override
	public void onStartTick(Minecraft client) {
		var cam = client.cameraEntity;
		if (cam == null) {
        }
	}

	/**
	 * Unbinds aura framebuffer for use and undoes changes made in {@link #beginGlowFramebufferUse()}.
	 */
	private void endGlowFramebufferUse() {
		this.client.getMainRenderTarget().bindWrite(false);
	}

	/**
	 * Gets the {@link RenderType} for rendering glowies from a given base
	 *
	 * @param base the base render type
	 *
	 * @return the render type
	 */
	public static RenderType getRenderType(RenderType base) {
		return ShimmerRenderTypes.getRenderType(base);
	}

	public static TransformedMultiBufferSource buffers(MultiBufferSource wrapped, int r, int g, int b, int a) {
		return new TransformedMultiBufferSource(DashShimmerEffectManager::getRenderType, DashShimmerEffectManager::getRenderType, wrapped, r, g, b, a);
	}


	/**
	 * Gets the {@link RenderType} for rendering glowies with a default texture
	 *
	 * @return the render type
	 */
	public static RenderType getRenderType() {
		return ShimmerRenderTypes.DEFAULT_GLOW_LAYER;
	}

	/**
	 * Helper for the creating and holding the aura render layers and target
	 */
	private static final class ShimmerRenderTypes extends RenderType {
		// have to extend RenderType to access a few of these things

		private static final OutputStateShard GLOW_TARGET = new OutputStateShard("haema:shimmer_target", DashShimmerEffectManager.INSTANCE::beginGlowFramebufferUse, DashShimmerEffectManager.INSTANCE::endGlowFramebufferUse);
		private static final Function<ResourceLocation, RenderType> GLOW_LAYER_FUNC = Util.memoize(id ->
				RenderType.create(
						"haema:shimmer",
						DefaultVertexFormat.POSITION_COLOR_TEX,
						VertexFormat.Mode.QUADS,
						256,
						CompositeState.builder()
								.setShaderState(RenderStateShard.RENDERTYPE_BEACON_BEAM_SHADER)
								.setWriteMaskState(COLOR_DEPTH_WRITE)
								.setTransparencyState(TRANSLUCENT_TRANSPARENCY)
								.setOutputState(GLOW_TARGET)
								.setTextureState(new TextureStateShard(id, false, false)).createCompositeState(false))
		);
		private static final ResourceLocation WHITE_TEXTURE = new ResourceLocation("misc/white.png");
		private static final RenderType DEFAULT_GLOW_LAYER = GLOW_LAYER_FUNC.apply(WHITE_TEXTURE);


		// no need to create instances of this
		private ShimmerRenderTypes(String string, VertexFormat vertexFormat, VertexFormat.Mode mode, int i, boolean bl, boolean bl2, Runnable runnable, Runnable runnable2) {
			super(string, vertexFormat, mode, i, bl, bl2, runnable, runnable2);
		}

		public static RenderType getRenderType(RenderType base) {
			return RenderLayerHelper.copy(base, "haema:shimmer", builder -> builder.setOutputState(GLOW_TARGET));
		}
	}
}