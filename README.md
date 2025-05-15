# ship-demo

## A simple demo of running Luerl in Erlang using space ships

Each ship is a separate process using basic Erlang for control and
communication while the logic is written in Lua.

Use ``run.sh`` to start the system. You need to check the path where
it is to find Luerl and the esdl2 systems.

The various ship types:

**default_ship** - These are the default ship type for all the initially created ships. They just run a straight line and bounce off the edge of the universe.

**run_ship** - These are green and just move towards the edge of the universe and run along the edge of the universe.

**fly_ship** - These are blue and behave like flies. They fly to edge of the universe, sit there for a while then fly off.

**flock_ship** - These ships tend to fly in flocks.

**timid_ship** - These start of white and when they see a ship in the same sector they turn yellow and go backwards.

**attack_ship** - These ships are orange zap and kill ships which they get close to.

An example run:

``` erlang
(sim@renat)1> start(300,300,1000).
{ok,<0.95.0>}
(sim@renat)2> start_run(75).
ok
(sim@renat)3> set_ships("run_ship", 1, 300).
ok
(sim@renat)4> set_ships("fly_ship", 301, 500).
ok
(sim@renat)5> set_ships("attack_ship", 951, 1000).
ok
(sim@renat)6> stop_run().
ok
```
