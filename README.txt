Hong.

Pong clone written in haskell.

The instructions for playing are printed to STDOUT when starting the game,
so open the game in a terminal to see them (> ./Main)

Written in haskell, and the resulting code is well-documented and
clean (I intended that at least :)). Basically I use ideas of incremental
sampling used in the book Haskell School of Expression by Paul Hudak
for sampling states, but I use StateT IO for managing the IO-part and
the environment around the IO-operations (like time-flow).

I use the SOE library for graphics, and I think it was quite good.
I do recommend you to stay away from gtkhs and wx if you're going
to make a game (or anything with actively refreshing graphics).

Arash Rouhani 2010
miffoljud@gmail.com
www.arashrouhani.com

