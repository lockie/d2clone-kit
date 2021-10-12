# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- SYSTEM-REF helper re-added for loose coupling reasons.
- Priorities of CastleDB tables.
- Default value for font configuration variable.

### Changed
- Changed syntax of WITH-SYSTEM-SLOTS macro to be more DSL-ish.
- Changed inlined functions to be uninlined in debug mode.
- Load all CastleDB .cdb files instead of the one specific for the game.
- Improved error handling of .cdb file loading.

### Removed

### Fixed
- Fixed compilation warnings, slightly improving performance.

## [0.1.1] - 2021-08-07
### Added
- MacOS support.
- Helper function to do proper error checking on liballegro file loading.
- Proper collision map clearing on map chunks deletion.
- System initialization method.
- Game session control functions (NEW-GAME, GAME-STARTED-P, EXIT).
- READ-FILE-INTO-LIST helper to be used in credits screen.
- GUI system DSL based on Nuklear library.
- Main menu.
- Credits screen.
- Parent-child relationship in ECS entities.
- Actions subsystem.
- DUMP-ENTITIES debug helper.
- Ability to synchronously process issued event.
- NON-INTERRUPTIBLE property for sounds.
- Loading screen.
- Support for sprite per-layer frame properties.
- Support for sound effects on frames different than first animation frame.
- Support for sprite layer properties.
- CastleDB database support and simple data tables API.
- EQUIPPED-WEAPON-CLASS and EQUIPPED-WEAPON helpers.
- Random sound effect variablility.
- Attack impact sounds.
- Mention of engine version in the log.

### Changed
- Removed ubiquitous mana system component.
- Refactored coordinates system. Now orthogonal coordinates are stored and used
  everywhere, and isometric ones are calculated on the fly when needed.
- Removed ubiquitous coordinates component.
- Removed ubiquitous sprite batch system component.
- Refactored new game object initialization to avoid runtime code generation.
- DEFPREFAB macro now also defines necessary empty MAKE-COMPONENT method.
- ECS systems refactored to use DEFSTRUCTs for performance reasons.
- Refactored invalid entity mechanics.
- ECS components refactored to sparse SoA arrays for performance and memory
  consumption reasons.
- Radically simplified events subsystem.
- Delta time between frames is now a global variable.
- Mob health bar and item text drawing refactored out of player system.
- Refactored sprite frame time calculation.
- Comply with ECS system order when initializing component in MAKE-OBJECT.
- Comply with ECS system order when finalizing systems.
- Forced the 80 columns limit on all source code files.
- Prevented unwanted symbol interning.

### Removed
- Obsolete console-system stub.
- QUIT event in favour of plain SYSTEM-FINALIZE method.
- SYSTEM-REF replaced with global-vars for performance reasons.
- Removed cl-static-dispatch library usage.

### Fixed
- Fixed compilation on CCL.
- Assets are now loaded using path relative to executable, not to current
  working directory.
- Minor performance improvements in asset loading.
- Fixed bug causing FLOATING-POINT-INVALID-OPERATION on some systems.
- Greatly improved memory usage by avoiding storing parsed ase files.
- Fixed bug causing log messages with % symbol to be output incorrectly.
- Fixed bug causing log messages longer than approx. 2kbytes to be truncated.
- Fixed bug causing character shadow to be drawn over the weapon.
- Fixed bug causing character to be able to move through other characters.
- Fixed bug causing player character not being able to stop the initiated
  attack.
- Fixed item-related performance issue.
- Greatly improved sprite batch performance.
- Fixed bug causing item sprite batch to be still drawn when item was picked
  up.
- Fixed bug causing sprites to glitch when all objects were recreated.
- Fixed bug causing the game to crash on exit when some fonts were not deleted.
- Fixed bug causing corpse sprites to be incorrectly rendered on top of other
  ones.
- Fixed several memory leaks on exit.
- Fixed off-by-one error in growable vector implementation.
- Fixed crash on creating new game with items present on the ground.
- Fixed bug with attack landing animation.
- Minor performance improvement when creating components.
- Fixed bug with trying to drop fists weapon.
- Fixed the import name of golden-utils library.
- Fixed the bug causing crash on weapon change.
- Fixed compilation under MinGW.

## [0.1.0] - 2020-04-20
### Added
- DSL-like entitites initialization.
- HP and mana subsystems as well as player's HP and mana orbs.
- Mob system.
- Support for Aseprite cel user data.
- Combat system.
- Ability to do target lock by not releasing mouse button, just like in D2.
- Mob health bar.
- Non-interruptible sprite animations.
- Abitily to override config options in call to START-ENGINE.
- Prefab preloading.
- Sound system.
- ECS improvements.
- Simple item system.

### Changed
- Next animation handling.
- Set default parameters for sprites.

### Removed
- Unnecessary camera component.

### Fixed
- Minor deployment-related fixes.
- Fixed character movement speed maths.
- Fixed bug causing memory faults on secondary start attempts after caught
  conditions.
- Fixed bug when the character was registered as colliding with some stuff when
  it really wasn't.
- Fixed bug when the player character would start moving when it was not asked
  to.
- Fixed bug when the player character would keep moving when it was not asked
  to.

## [0.0.1] - 2020-04-10
### Added
- Aseprite format parser.
- Viewport handling routines.
- A* algorithm.
- Config subsystem built on top of
  [liballegro APIs](https://liballeg.org/a5docs/5.2.0/config.html).
- Simple engine demo.
- Means of visual debugging.
- Simple event loop built on top of [deeds](https://github.com/Shinmera/deeds)
  library.
- Support for loading assets from zip files.
- Utilities to easily parse binary files.
- Some data structures, including simple-vector of dynamic size, priority queue
  and sparse-matrix.
- Log subsystem built on top of liballegro APIs.
- Proper (not like the
  [last time](https://awkravchuk.itch.io/darkness-looming))
  isometric <-> orthogonal conversion maths.
- Functional 2D renderer.
- Sprite batching subsystem.
- Stateful movable sprites.
- Simple ECS implementation.
- Tiled format parser.
