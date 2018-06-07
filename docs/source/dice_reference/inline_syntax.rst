.. RPDiscordRewrite documentation master file, created by
   sphinx-quickstart on Mon May 28 13:33:53 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _inline_syntax:

Inline Syntax
============================================

When toggled on, the inline roller can recognize `inline expressions` in any non-prefixed messages in channels where it is allowed to type, and immediately type response message containing the roll results. If a message has more than one `inline expression` within, all `inline expressions` present will be rolled independently.

An inline expression is composed of an optional `repeat term`, a necessary `dice term`, and optionally followed by the addition (`+`) and/or subtraction (`-`) of integers or further `dice terms`.

A `repeat term` is simply a term of a form `X#`, where `X` is a positive integer. If present, the following expression will be rolled `X` times, with the results shown as a comma-delimited list.

A `dice term` has two forms:

*To roll one die, use `dN`, where `N` is a positive integer; this rolls one `N`-side die

*To roll a pool of dice, use `MdN`, where `M` and `N` are positive integers; this rolls a pool of `M` `N`-sided dice.

.. _inline_examples:
Examples:
---------------------------------------------

* `d20`: rolls a twenty-sided die.

* `5d6`: rolls five six-sided dice, adding the results.

* `3d6+5`: rolls three six-sided dice, adding the results to five.

* `5#d6`: rolls five six-sided dice, without adding the results.

* `5#d6-d4`: rolls one six-sided die minus one four-sided die, five times.

* `2#2d10`: rolls two pools of two six-sided dice. Each pool will be added together; the end results will not.

* `2#1d20+5d6+7`: rolls the sum of one twenty-sided die, five six-sided dice, and seven, two times.
