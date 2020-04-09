Diablo 2 is arguably one of the best action RPG created ever. This is humble attempt to recreate and, hopefully, deconstruct it using Common Lisp programming language and related techniques.

## Features

* Support for [Tiled](https://www.mapeditor.org) map format.
* Support for [Aseprite](https://aseprite.org) sprite format.
* Entity-Component-System implementation.
* Powerful events subsystem (courtesy of [Shinmera's deeds library](https://github.com/Shinmera/deeds)).
* 2D renderer with functional interface.
* Sprite batching subsystem.
* Naïve implementation of A* pathfinding algorithm.
* Powerful debugging facilities, including visual debugging.
* Loading assets from zip files.
* Simple yet powerful configuration subsystem.
* Logging subsystem.

## Installation
d2clone-kit requires [liballegro](https://liballeg.org) to function. To install liballegro, refer to your distribution's package manager; for instance, on Debian derivatives (including Ubuntu and Mint):

```
sudo apt-get install liballegro-acodec5.2 liballegro-audio5.2 liballegro-image5.2 \
    liballegro-dialog5.2 liballegro-ttf5.2 liballegro-physfs5.2 liballegro-video5.2
```

Currently the following Common Lisp implementations are tested and supported by d2clone-kit:

* [SBCL](http://sbcl.org)
* [CCL](https://ccl.clozure.com)

You can run it on x86_64 Linux, OS X, and Windows, while SBCL on Linux is the recommended platform.

To install d2clone-kit, clone the repository to your Quicklisp's `local-projects` directory (assuming you have [Quicklisp](http://quicklisp.org) installed):

```
$ git clone https://gitlab.com/lockie/d2clone-kit ~/quicklisp/local-projects/d2clone-kit
$ sbcl --quit --eval "(progn (ql:register-local-projects) (ql:quickload :d2clone-kit))"
```

## Glossary
* **map coordinates** - *world coordinates* of tile in staggered map grid.
* **prefab** - system-specific object loaded from file and used as a template to create new instances of system's component.
* **viewport coordinates** - screen pixel coordinates relative to the current camera position.
* **world coordinates** - floating point in-game world coordinates. One unit of length corresponds to one map tile width, typically 64 pixels.

## Legal
d2clone-kit is licensed under the GNU GPL license version 3. See [LICENSE](https://gitlab.com/lockie/d2clone-kit/-/blob/master/LICENSE).

Diablo® II - Copyright © 2000 Blizzard Entertainment, Inc. All rights reserved. Diablo and Blizzard Entertainment are trademarks or registered trademarks of Blizzard Entertainment, Inc. in the U.S. and/or other countries.

d2clone-kit and any of its maintainers are in no way associated with or endorsed by Blizzard Entertainment®.
