Moonies
=======

~~Since I am a huge wanker I decided to rewrite monies in Haskell. When it reaches
even half of the functionality the Python version has we'll all have a party.~~

~~In the likely event that it doesn't, don't use this.~~

A sandbox to monitor a bunch of crap (funds, stocks (maybe)). The idea is to evolve it into
some sort of portfolio monitoring tool with maybe something to do with forecasting etc.

Dependencies are listed in the cabal file. Only one foreign dependency:

* MongoDB (tested with >= 2.4)

It's a Yesod project so you should have the Haskell Platform installed.

Warnings
========

1. The code requires a bunch of secrets to compile and to work, it's mildly unusable
   at the moment unless you have the "private" objects set up.
2. ~~Should you have those secrets, there is no caching, so if you're flooding somebody's
   website with your calls... well, you know.~~ There is some caching implementation now
   so let's skip this.

TODO
====

* Page to manage funds: ~~list~~ + form (almost done, inserts missing).
* ~~Rewrite the whole MongoDB infrastructure using the Persist API.~~ Almost done, I'll
  refactor it when I'm less drunk.
* ~~Caching, seriously, what the fuck.~~
* Separate the foreign gathering function (it's not wise to insert things on refresh).
* Fix flymake.
