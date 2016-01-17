Moot   ----- holds all the systems
|
System ----- 1 for each component type

Entity
| ---------- 1 to many
Component

### Basics

- components are just data. They are structs

- all component-types have an index. The moot takes care of which
  indicies are used

- entities hold an array of components (plus caches etc)

- entities can recieve messages from the moot, these are for moot wide
  things like undefining a type and as such dont need to be super
  performant.

- entities announce when a component is added/removed/updated. This is
  to update caches and such

- Systems are associated with one component type.

- Systems can be either be tick-driven or event-driven

- Tick driven Systems perform a pass when the moot's tick function is
  called

- A pass to a system is having its pass function called on every
  entity which that a component of the System's type

- A partial pass is a pass on a subset of the entities of a given
  system. This may cause issues in dependent systems if you get the
  order wrong.

- Event-driven systems only run passes when a certain event is
  triggered

### Particular Features

- release mode: a constant you have to set. If true at compile time
  it strips a lot of checks (and enables more optimizations)

- dependant components: so to add one it checks for the other.
  It is NOT inheritance the components are still seperate things.
  An example is that movable needs position

- dependant systems: Systems can be declared order dependent, thus it
  is an error to start a pass on a system before the systems it is
  dependant on. These checks are turned off in release-mode

- systems to be only able to modify one type of component but
  view many what they can view has to be declared. When not in release
  mode will check a 'key' for permission to make changes


###Inspiration

- http://cowboyprogramming.com/2007/01/05/evolve-your-heirachy/
  The first article that got me into this

- http://t-machine.org/index.php/2007/11/11/entity-systems-are-the-future-of-mmog-development-part-2/
  The best breakdown of the initial concepts I know

- https://github.com/sschmid/Entitas-CSharp
  For the concept of Execute & Reactive Systems
