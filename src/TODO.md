TODO : внутриигровая консоль с прямым доступом к REPL! sdl2-ttf, swank?.. https://github.com/astine/swank-client/blob/master/swank-description.markdown
:boot хук в deploy?..
https://github.com/borodust/bodge-nuklear/blob/master/src/example.lisp#L29 !!!
cl-nuklear в ~/Progs :|
чот у nuklear интеграция с SDL какая-то позорная, чорное окно мне рисует ((
можно попробовать чонибудь плюсовое с https://github.com/Islam0mar/cl-cxx

TODO : префабы / инстансинг для спрайтов и фрагментов карт
https://gamedev.stackexchange.com/questions/163914/how-to-design-prefabs-in-entity-component-systems
https://www.reddit.com/r/gamedev/comments/80490i/efficient_prefabsinstancing_with_ecs/
по идее, чтобы было как можно меньше промахов по кэшу, нужно хранить в одном-единственном объекте всю нужную инфу, а компоненты уже должны на этот префаб как-то ссылаться
!!! для вдохновения на архитектуренг: https://github.com/toptea/roguelike_tutorial
см. тж. https://github.com/benmoran56/esper
наверное, надо начать с ресурсной системы, чтобы префабы могли быть кейвордами/символами, напр. префаб 'zombie -> assets.zip/sprites/zombie.ase
а потом, наверное, хранить в отдельной хэшмапе префабы и копировать их при создании компонента (из соображений быстродействия)
для ресурсной системы, видимо, нужны gray streams


DONE чтение из pak-архивов через https://liballeg.org/a5docs/trunk/physfs.html (см. тж. https://icculus.org/physfs/physfstut.txt )
(заврапать в grey streams, из cl-liballegro экспортится всё, что нужно :3)

TODO : конпеляция в travis-ci, чтобы виндусовые сборки делать: https://docs.travis-ci.com/user/reference/windows/ . Ну или appveyor.com , он free for OpenSource

TODO : сконпелировать под шиндус через nuget?

TODO : готовая event system?
https://github.com/Shinmera/deeds
таск менеджер: https://github.com/Shinmera/simple-tasks


XXX use https://github.com/danlentz/manardb ?

TODO : в будущем, для ускорения CLOS: https://github.com/guicho271828/inlined-generic-function (экспериментальное!) или https://github.com/alex-gutev/static-dispatch


DONE : change event loop to that https://git.io/JeutQ ?
> I would recommend you to not use cl-sdl2's event loop at all. Instead you can integrate cbaggers/live-support into your code with a custom event loop. You can look at one of my projects for such an event loop. Most of one is in this file

TODO : физика!!!!!!!
cl-ode + ode. отд. компонент для 3d позиции, из которой будут высчитываться целочисленные 2d point.
-> получается, нужны зависимости у систем!

у не-ground тайлов на карте просто создавать физ. объектом кубик :3
ещё, кстати, видимо придётся слоям на карте придавать артрибут height, только хз, как с ним правильно рендерить.

компонент "персонаж": target - 3d позиция (или отд. компонент, и хранить его entity?)
в начале движения в ode прилагаем соотв. силу. по направлению к target. незадолго перед концом движения - противоположную силу, тип тормозим. когда с учётом эпсилон попадаем в target, убираем (зануляем).
ну или сделать через servo: http://www.ode.org/ode-latest-userguide.html#sec_7_5_0

ещё, как вариант, использовать что-то 2d-шное (box2d или chipmunk), всё равно tiled толком не поддерживает высоту.
А, не, высоту можно кое-как проэмулировать с vertical layer offset: https://discourse.mapeditor.org/t/tiled-0-14-0-released/769
однако же кодить и поддерживать эту залупу я точно заебусь, поэтому начну пока с 2d.

с сишным интерфейсом только chipmunk ._.

про плеера см. https://chipmunk-physics.net/forum/viewtopic.php?t=316
и https://chipmunk-physics.net/forum/viewtopic.php?f=1&t=3676


TODO : docstring в defcomponent?
+ убрать параметр system у макроса?, всё равно у одной системы не м/б несколько компонентов


TODO Реклама, лол. Видос с бодрой музыкой и дергающимися скринами кода с подписями:
LINGUISTIC ABSTRACTION (тут скрин из camera-system пачки вложенных with. ну или из map-system draw)
MACRO-DEFINING MACRO (тут, понятно, скрин макроса defcomponent и ещё defoptions)
