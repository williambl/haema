package com.williambl.haema.abilities.bat

import com.williambl.haema.component.BatFormComponent
import com.williambl.haema.component.VampireComponent

interface BatFormable {
    var isBat: Boolean
        get() = BatFormComponent.entityKey.get(this).isBat
        set(value) {
            BatFormComponent.entityKey.get(this).isBat = value
        }
}