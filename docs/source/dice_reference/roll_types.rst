.. RPDiscordRewrite documentation master file, created by
   sphinx-quickstart on Mon May 28 13:33:53 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _roll_types:

Roller Types
============================================

There are two methods of rolling dice using the bot: the inline roller and the command-based roller.

.. _inine_roller:

Inline Roller
--------------------------------------------

The inline dice roller has the bot roll dice from expressions present in the middle of non-prefixed messages. Since the bot has to work harder to spot dice expressions that occur without the warning a prefix gives, these expressions are much more limited than the command-based roller, but still useful for more standard rolls. See the page on :ref:`inline_syntax` for more information on how inline rolls are structured


.. _command_roller:

Command-Based Roller
--------------------------------------------

The command-based roller allows for evaluation of sophisticated dice expressions. This does mean that expressions that could be computationally expensive can be presented to the dice roller; to avoid the risk of malignant use of this fact, the command-based roller has a 10-second timeout on computations. Rather than explain the full capabilities of the command-based roller here, see the page on :ref:`command_syntax` for further info.
