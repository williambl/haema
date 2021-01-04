package com.williambl.haema.client.config

enum class HudPlacement(val x: (Int, Int) -> Int, val y: (Int, Int) -> Int) {
    TOP_LEFT({ _, textWidth -> textWidth/2 + 10}, { height, index -> 8 + index*9}),
    TOP_RIGHT({width, textWidth -> width - textWidth/2 - 10}, {height, index -> 8 + index*9}),
    BOTTOM_LEFT({ _, textWidth -> textWidth/2 + 10}, { height, index -> height - 16 - index*9}),
    BOTTOM_RIGHT({width, textWidth -> width - textWidth/2 - 10}, {height, index -> height - 16 - index*9}),
    NONE({ _, _ -> 0}, { _, _ -> 0})
}