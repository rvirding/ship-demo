#! /bin/sh

# Pick the right path.
# DEMO_LIBS="$HOME/luerl/luerl/ebin $HOME/erlang/esdl2-henning/ebin ./ebin"
# DEMO_LIBS="$HOME/luerl/luerl/ebin $HOME/erlang/esdl2/ebin ./ebin"

DEMO_LIBS="$HOME/luerl/luerl/ebin $HOME//esdl2/ebin ./ebin"

# Pick the right configuration options.
# exec lfe -smp enable +stbt true -sname sim -setcookie ship-demo -pa $DEMO_LIBS
# exec lfe -smp enable +sub true -sname sim -setcookie ship-demo -pa $DEMO_LIBS

exec lfe -smp enable +stbt db -sname sim -setcookie ship-demo -pa $DEMO_LIBS
