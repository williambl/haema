package com.williambl.haema.hunters;

import com.sun.jna.platform.win32.WinDef;
import com.williambl.haema.api.vampire.VampireApi;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
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
import net.minecraft.world.entity.ai.Brain;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.behavior.LookAtTargetSink;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.PatrollingMonster;
import net.minecraft.world.entity.npc.AbstractVillager;
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
import net.tslat.smartbrainlib.api.SmartBrainOwner;
import net.tslat.smartbrainlib.api.core.BrainActivityGroup;
import net.tslat.smartbrainlib.api.core.SmartBrainProvider;
import net.tslat.smartbrainlib.api.core.behaviour.FirstApplicableBehaviour;
import net.tslat.smartbrainlib.api.core.behaviour.custom.attack.BowAttack;
import net.tslat.smartbrainlib.api.core.behaviour.custom.misc.Idle;
import net.tslat.smartbrainlib.api.core.behaviour.custom.move.MoveToWalkTarget;
import net.tslat.smartbrainlib.api.core.behaviour.custom.target.InvalidateAttackTarget;
import net.tslat.smartbrainlib.api.core.behaviour.custom.target.SetPlayerLookTarget;
import net.tslat.smartbrainlib.api.core.behaviour.custom.target.SetRandomLookTarget;
import net.tslat.smartbrainlib.api.core.behaviour.custom.target.TargetOrRetaliate;
import net.tslat.smartbrainlib.api.core.sensor.ExtendedSensor;
import net.tslat.smartbrainlib.api.core.sensor.vanilla.NearbyLivingEntitySensor;
import net.tslat.smartbrainlib.api.core.sensor.vanilla.NearbyPlayersSensor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import static com.williambl.haema.Haema.id;

public class VampireHunter extends PatrollingMonster implements CrossbowAttackMob, SmartBrainOwner<VampireHunter> {
    public static final String RIGHTEOUS_BANNER_TRANSLATION_KEY = Util.makeDescriptionId("block", id("righteous_banner"));
    private static final EntityDataAccessor<Boolean> CHARGING = SynchedEntityData.defineId(VampireHunter.class, EntityDataSerializers.BOOLEAN);

    private final SimpleContainer inventory = new SimpleContainer(5);

    protected VampireHunter(EntityType<? extends PatrollingMonster> entityType, Level level) {
        super(entityType, level);
    }

    public static AttributeSupplier.Builder createHunterAttributes() {
        return createMobAttributes()
                .add(Attributes.MOVEMENT_SPEED, 0.35)
                .add(Attributes.MAX_HEALTH, 20.0)
                .add(Attributes.ATTACK_DAMAGE, 5.0)
                .add(Attributes.FOLLOW_RANGE, 64.0);
    }

    @Override
    protected void registerGoals() {
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
        this.tickBrain(this);
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

    private boolean moveHandStackToInventory() {
        if (this.getMainHandItem().isEmpty()) {
            return true;
        }
        if (this.inventory.canAddItem(this.getMainHandItem())) {
            this.inventory.addItem(this.getMainHandItem());
            this.setItemSlot(EquipmentSlot.MAINHAND, ItemStack.EMPTY);
            return true;
        }

        return false;
    }

    private boolean startHolding(Predicate<ItemStack> stackToHold) {
        if (stackToHold.test(this.getMainHandItem())) {
            return true;
        } else {
            for (int i = 0; i < this.inventory.getContainerSize(); i++) {
                if (stackToHold.test(this.inventory.getItem(i)))  {
                    var removed = this.inventory.removeItem(i, this.inventory.getMaxStackSize());
                    if (this.moveHandStackToInventory()) {
                        this.setItemSlot(EquipmentSlot.MAINHAND, removed);
                        return true;
                    } else {
                        this.inventory.addItem(removed);
                        return false;
                    }
                }
            }

            return false;
        }
    }

    public boolean isCharging() {
        return this.entityData.get(CHARGING);
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

    @Override
    protected Brain.Provider<?> brainProvider() {
        return new SmartBrainProvider<>(this);
    }

    @Override
    public List<ExtendedSensor<VampireHunter>> getSensors() {
        return ObjectArrayList.of(
                new NearbyPlayersSensor<>(),
                new NearbyLivingEntitySensor<VampireHunter>()
                        .setPredicate((target, entity) -> target instanceof VampireHunter || target instanceof AbstractVillager || VampireApi.isVampire(target)));
    }

    @Override
    public BrainActivityGroup<VampireHunter> getCoreTasks() {
        return BrainActivityGroup.coreTasks(
                new LookAtTargetSink(40, 300),
                new MoveToWalkTarget<>());
    }

    @Override
    public BrainActivityGroup<VampireHunter> getIdleTasks() {
        return BrainActivityGroup.idleTasks(
                new FirstApplicableBehaviour<>(
                    new TargetOrRetaliate<>().attackablePredicate(VampireApi::isVampire),
                    new SetPlayerLookTarget<>(),
                    new SetEntityLookTarget<>().predicate(e -> e instanceof VampireHunter || e instanceof AbstractVillager),
                    new SetRandomLookTarget<>()),
                new Idle<>().runFor(e -> e.getRandom().nextInt(30, 60)));
    }

    @Override
    public BrainActivityGroup<VampireHunter> getFightTasks() {
        return BrainActivityGroup.fightTasks(
                new InvalidateAttackTarget<>());
    }
}
