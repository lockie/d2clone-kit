Diablo 2 is arguably one of the best action RPG created ever. This is humble
attempt to recreate and, hopefully, deconstruct it using Common Lisp
programming language and related techniques.

## Features

* Support for [Tiled](https://www.mapeditor.org) map format.
* Support for [Aseprite](https://aseprite.org) sprite format.
* Support for [CastleDB](http://castledb.org) database format.
* Performant Entity-Component-System implementation supporting
  parent-child entitiy relationships.
* Prefab subsystem allowing creation of new components using
  predefined file-backed template.
* Simple yet powerful events subsystem.
* Full-blown game character subsystem.
* Simple actions subsystem, providing building blocks for complex
  character behaviour.
* Basic game session utilities.
* 2D renderer with functional interface.
* Sprite batching subsystem.
* Simple DSL for creating GUI windows by
  [Nuklear library](https://gitlab.com/lockie/cl-liballegro-nuklear).
* Naïve implementation of A* pathfinding algorithm.
* Powerful debugging facilities, including entity tree dumping and
  visual debugging.
* Loading assets from zip files.
* Simple yet versatile configuration subsystem.
* Logging subsystem.

## Installation
d2clone-kit requires [liballegro](https://liballeg.org) to function. To install
liballegro, refer to your distribution's package manager; for instance, on
Debian derivatives (including Ubuntu and Mint):

```
sudo apt-get install liballegro-acodec5.2 liballegro-audio5.2 \
    liballegro-image5.2 liballegro-dialog5.2 liballegro-ttf5.2 \
    liballegro-physfs5.2 liballegro-video5.2
```

Currently the following Common Lisp implementations are tested and supported by
d2clone-kit:

* [SBCL](http://sbcl.org)
* [CCL](https://ccl.clozure.com)
* [ECL](https://common-lisp.net/project/ecl)

You can run it on x86_64 Linux, macOS, and Windows, while SBCL on Linux is the
recommended platform.

To install d2clone-kit, clone the repository to your Quicklisp's
`local-projects` directory (assuming you have
[Quicklisp](http://quicklisp.org) installed):

```
$ git clone https://gitlab.com/lockie/d2clone-kit \
    ~/quicklisp/local-projects/d2clone-kit
$ sbcl --quit --eval \
    "(progn (ql:register-local-projects) (ql:quickload :d2clone-kit))"
```

## Glossary
* **action** - abstraction of a multi-frame action performed by an
  entity. Unless stated otherwise, action is represented with its global index.
* **isometric coordinates** - coordinates in isometric projection.
* **orthogonal coordinates** - floating point in-game world coordinates. One
  unit of length corresponds to one map tile width, typically 64 pixels.
* **prefab** - system-specific object loaded from file and used as a template
  to create new instances of system's component.
* **screen coordinates** - integer screen pixel coordinates.
* **viewport coordinates** - screen pixel coordinates relative to the current
  camera position.


## Development Roadmap
See [ROADMAP](https://gitlab.com/lockie/d2clone-kit/-/blob/master/ROADMAP.org).

## Legal
d2clone-kit is licensed under the GNU GPL license version 3.
See [LICENSE](https://gitlab.com/lockie/d2clone-kit/-/blob/master/LICENSE).

Diablo® II - Copyright © 2000 Blizzard Entertainment, Inc. All rights
reserved. Diablo and Blizzard Entertainment are trademarks or registered
trademarks of Blizzard Entertainment, Inc. in the U.S. and/or other countries.

d2clone-kit and any of its maintainers are in no way associated with or
endorsed by Blizzard Entertainment®.
