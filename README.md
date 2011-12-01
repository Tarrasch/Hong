# Hong

This is a pong clone written in haskell.

![Hong](/Hong.png "Hong!")

## Installing and Running

  1. First get the [Haskell Platform][hp].

  1. Clone the project

         git clone git://github.com/Tarrasch/Hong.git

  1. Install the program (run these commands from the `Hong` directory)

         cabal update
         cabal configure
         cabal install

  1. Run the program

         Hong

     Alternatively if you don't have the cabal bin folder in your `$PATH`.

         ~/.cabal/bin/Hong


## Gameplay features

This implementation is as simple as can be, the most cool feature
is the time manipulation functionality. To see the controls,
just start the game from a terminal and it will print out the playing
intsructions.

## Technology

I've used Functional Reactive Programming to write this.
Haskell is the language of choice and I used a **very** old
library called Fal that provides the Reactive part.
Fal is from the book *Haskell School of Expression*. So it is very simple.

Functional and Programming are well known terms, though Reactive
needs some explenation. In Reactive Programming when you write
`z = x + y` you actually
mean `z(t) = x(t) + y(t)`. Everything is implicitly functions of time.
And it turns out to be very expressive in creating games!

Note that Reactive is not neccesary a high-level paradigm!
On the contrary, VHDL is Reactive!
(since circuits are asynchronus they can be seen as functions of time)

### Side-effect free

Haskell is a pure language, which means functions have no side effects.
Why is that good? Well, it's very good for
*seperating logic from IO*! And to test ones program (unit tests)
one must do it in a side-effect free way. Otherwise a test that passes
today might fail tommorow!

Thank god I used a library (Fal) that already have seperated
all IO for me, actually, the **only** function with side-effects
is the main function, which simply only starts the game! That's it!

### Actual testing

I would be a hypocrit praising purity without writing actual
unit tests. I've used `hspec` and *after* you have successfully
installed the game you can easily run the tests

    ghci test/HongTest.hs
    (.. lots of output ..)
    *HongTest> hspec specs

Of course the tests pass.

### Fal

I've manually included the Fal files. To hierarcially seperate code
I written from library code I put Fal-files in the folder `Fal`.

Fal is very small, and lacks features,
so I was forced to add my addendum to it, I called it `Fal.hs`.

### Middleware

Fal is great as you it exposes an interface, with an exposed interface
one can write middlewares that work for all Fal programs. I've
examplified this by a timemanipulating middleware, which allows the
user to increase/decrease game speed and pause the game.
That exact same middleware can be used for anything written
in Fal!

Other middleware one could have written:

  * Allow any game to be restartable
  * Allow any game to have save states.


[hp]: http://hackage.haskell.org/platform/
