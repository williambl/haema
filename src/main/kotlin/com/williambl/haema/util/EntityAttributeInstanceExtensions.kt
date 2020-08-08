package com.williambl.haema.util

import net.minecraft.entity.attribute.EntityAttributeInstance
import net.minecraft.entity.attribute.EntityAttributeModifier
import java.util.*

fun EntityAttributeInstance.computeValueWithout(without: UUID): Double {
    var addedValue = baseValue

    for (entityAttributeModifier in getModifiers(EntityAttributeModifier.Operation.ADDITION).filterNot { it.id == without })
        addedValue += entityAttributeModifier.value

    var multipliedValue = addedValue

    for (entityAttributeModifier in getModifiers(EntityAttributeModifier.Operation.MULTIPLY_BASE).filterNot { it.id == without })
        multipliedValue += addedValue * entityAttributeModifier.value

    for (entityAttributeModifier in getModifiers(EntityAttributeModifier.Operation.MULTIPLY_TOTAL).filterNot { it.id == without })
    multipliedValue *= 1.0 + entityAttributeModifier.value

    return attribute.clamp(multipliedValue)
}