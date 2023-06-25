package com.williambl.haema.hunters;

import com.williambl.haema.api.vampire.VampireComponent;
import net.minecraft.ChatFormatting;
import net.minecraft.Util;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.util.RandomSource;
import net.minecraft.world.Difficulty;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.*;
import net.minecraft.world.entity.ai.goal.FloatGoal;
import net.minecraft.world.entity.ai.goal.LookAtPlayerGoal;
import net.minecraft.world.entity.ai.goal.RandomStrollGoal;
import net.minecraft.world.entity.ai.goal.target.HurtByTargetGoal;
import net.minecraft.world.entity.ai.goal.target.NearestAttackableTargetGoal;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.PatrollingMonster;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.entity.projectile.ProjectileUtil;
import net.minecraft.world.item.*;
import net.minecraft.world.item.alchemy.PotionUtils;
import net.minecraft.world.item.alchemy.Potions;
import net.minecraft.world.item.enchantment.Enchantment;
import net.minecraft.world.item.enchantment.EnchantmentHelper;
import net.minecraft.world.item.enchantment.Enchantments;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BannerPattern;
import net.minecraft.world.level.block.entity.BannerPatterns;

import java.util.HashMap;
import java.util.Map;

import static com.williambl.haema.Haema.id;

public class VampireHunter extends PatrollingMonster implements CrossbowAttackMob {
    public static final String RIGHTEOUS_BANNER_TRANSLATION_KEY = Util.makeDescriptionId("block", id("righteous_banner"));
    private static final EntityDataAccessor<Boolean> CHARGING = SynchedEntityData.defineId(VampireHunter.class, EntityDataSerializers.BOOLEAN);

    private final SimpleContainer inventory = new SimpleContainer(5);

    protected VampireHunter(EntityType<? extends PatrollingMonster> entityType, Level level) {
        super(entityType, level);
    }

    @Override
    protected void registerGoals() {
        super.registerGoals();
        this.goalSelector.addGoal(0, new FloatGoal(this));
        //this.goalSelector.addGoal(1, );
        this.goalSelector.addGoal(8, new RandomStrollGoal(this, 0.6));
        this.goalSelector.addGoal(9, new LookAtPlayerGoal(this, Player.class, 15, 1));
        this.goalSelector.addGoal(10, new LookAtPlayerGoal(this, Mob.class, 15));

        this.targetSelector.addGoal(1, new HurtByTargetGoal(this, AbstractVillager.class));
        this.targetSelector.addGoal(2, new NearestAttackableTargetGoal<>(this, LivingEntity.class, 10, true, false, l -> VampireComponent.KEY.maybeGet(l).filter(VampireComponent::isVampire).isPresent()));
    }

    @Override
    protected void defineSynchedData() {
        super.defineSynchedData();
        this.entityData.define(CHARGING, false);
    }

    @Override
    protected void populateDefaultEquipmentSlots(RandomSource randomSource, DifficultyInstance difficultyInstance) {
        //todo configure this
        var crossbow = new ItemStack(Items.CROSSBOW);
        this.inventory.addItem(crossbow);
        var sword = new ItemStack(this.random.nextDouble() + 1.0 > difficultyInstance.getEffectiveDifficulty() ? Items.IRON_SWORD : Items.WOODEN_SWORD);
        this.inventory.addItem(sword);
        if (this.isPatrolLeader()) {
            var banner = new ItemStack(Items.WHITE_BANNER);
            var tag = banner.getOrCreateTagElement("BlockEntityTag");
            var patterns = new BannerPattern.Builder()
                    .addPattern(BannerPatterns.RHOMBUS_MIDDLE, DyeColor.RED)
                    .addPattern(BannerPatterns.HALF_HORIZONTAL_MIRROR, DyeColor.LIGHT_BLUE)
                    .addPattern(BannerPatterns.CIRCLE_MIDDLE, DyeColor.RED)
                    .toListTag();
            tag.put("Patterns", patterns);
            banner.hideTooltipPart(ItemStack.TooltipPart.ADDITIONAL);
            banner.setHoverName(Component.translatable(RIGHTEOUS_BANNER_TRANSLATION_KEY).withStyle(ChatFormatting.GOLD));
            this.setItemSlot(EquipmentSlot.HEAD, banner);
        }
    }

    @Override
    protected void enchantSpawnedWeapon(RandomSource randomSource, float f) {
        var crossbow = this.inventory.getItem(0);
        if (crossbow.is(Items.CROSSBOW)) {
            Map<Enchantment, Integer> enchants = new HashMap<>();
            enchants.put(Enchantments.QUICK_CHARGE, 3);
            if (randomSource.nextInt(300) == 0) {
                enchants.put(Enchantments.PIERCING, 1);
            }

            EnchantmentHelper.setEnchantments(enchants, crossbow);
        }

        var sword = this.inventory.getItem(1);
        if (sword.getItem() instanceof SwordItem) {
            sword.enchant(Enchantments.SMITE, 2);
        }
    }

    @Override
    public void addAdditionalSaveData(CompoundTag compoundTag) {
        super.addAdditionalSaveData(compoundTag);
        var inv = this.inventory.createTag();
        compoundTag.put("Inventory", inv);
    }

    @Override
    public void readAdditionalSaveData(CompoundTag compoundTag) {
        super.readAdditionalSaveData(compoundTag);
        this.inventory.fromTag(compoundTag.getList("Inventory", Tag.TAG_COMPOUND));
    }

    @Override
    protected void customServerAiStep() {
        if (this.isHolding(stack -> !(stack.getItem() instanceof TieredItem) && !(stack.getItem() instanceof CrossbowItem) && !(stack.isEmpty()))) {
            this.moveHandStackToInventory();
        }
    }

    @Override
    protected void dropCustomDeathLoot(DamageSource damageSource, int i, boolean bl) {
        super.dropCustomDeathLoot(damageSource, i, bl);
        for (var stack : this.inventory.items) {
            if (this.random.nextFloat() < 0.1) {
                this.spawnAtLocation(stack);
            }
        }

        if (this.random.nextFloat() < 0.1) {
            this.spawnAtLocation(this.getMainHandItem());
        }
    }

    private void moveHandStackToInventory() {
        if (this.inventory.canAddItem(this.getMainHandItem())) {
            this.inventory.addItem(this.getMainHandItem());
            this.setItemSlot(EquipmentSlot.MAINHAND, ItemStack.EMPTY);
        }
    }

    @Override
    public void setChargingCrossbow(boolean bl) {
        this.entityData.set(CHARGING, bl);
    }

    @Override
    public void shootCrossbowProjectile(LivingEntity livingEntity, ItemStack itemStack, Projectile projectile, float multishotSpray) {
        var projectileToUse = this.getLevel().getCurrentDifficultyAt(this.blockPosition()).isHarderThan(Difficulty.EASY.getId()+this.random.nextFloat()*2) ?
                ProjectileUtil.getMobArrow(this, PotionUtils.setPotion(new ItemStack(Items.TIPPED_ARROW), Potions.WEAKNESS), 1) :
                projectile;
        if (this.getTarget() != null) {
            this.shootCrossbowProjectile(this, this.getTarget(), projectileToUse, multishotSpray, 1.6f);
        }
    }

    @Override
    public void onCrossbowAttackPerformed() {}

    @Override
    public void performRangedAttack(LivingEntity livingEntity, float f) {
        this.performCrossbowAttack(this, 1.6f);
    }

    @Override
    public boolean isAlliedTo(Entity other) {
        //TODO tag for friendly entity types ?
        return super.isAlliedTo(other) || ((other instanceof VampireHunter || other instanceof AbstractVillager) && this.getTeam() == null && other.getTeam() == null);
    }
}
