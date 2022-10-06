# Haema

Want to download Haema? It's here:

https://www.curseforge.com/minecraft/mc-mods/haema
https://modrinth.com/mod/haema

Please see CONTRIBUTING.md if you'd like to contribute :)

Want to build against Haema? Bring it in as a dependency like this:

```groovy
repositories {
    maven {
        name "Will BL Releases"
        url "https://maven.willbl.dev/releases"
    }
}

dependencies {
    // use modImplementation instead if required
    modApi ("com.williambl.haema:haema:${haema_version}")
}
```