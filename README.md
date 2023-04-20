# Greggin'

[GregTech](https://gregtech.overminddl1.com/) is an (in)famous Minecraft overhaul mod.

One aspect of the mod is meticulous crafting chains that require a batch crafting strategy.

Using just [JEI](https://www.curseforge.com/minecraft/mc-mods/jei), it can be difficult to plan ahead with a reasonable goal or gather all the materials for a batch.

This app will help you build a recipe collection, and then generate a report on what is needed for each step of the crafting process, beginning with raw materials.

## Running

### Set up Storage
This app is currently set up to store recipes in an Azure Storage Blob. The necessary configs are in `appsettings.json`

### Run
Start front end and back end: `dotnet run`

## Technology

This app is built on the F# SAFE stack, because I wanted to try it out and learn it. It actually began as the [Safe Dojo](https://github.com/CompositionalIT/SAFE-Dojo) project.

Simply put, developing this app has been a joy. I _highly_ recommend checking out this stack!