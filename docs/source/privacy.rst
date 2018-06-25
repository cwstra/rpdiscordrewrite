.. RPDiscordRewrite documentation master file, created by
   sphinx-quickstart on Mon May 28 13:33:53 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _privacy:

Privacy
--------------------------------------------

What personal information does the bot store? Well, there are two types.

* For record keeping, the bot stores a certain type of data unique to Discord called a Snowflake, so named because each Snowflake is unique. Virtually everything has a Snowflake: messages, servers, roles, even users. Snowflakes are quite important for the inner workings of Discord, but they're not particularly delicate information; anyone who's been on a server with you before could have kept your Snowflake for posterity, if they wanted.

  * "But why are you storing them?"
  * To store unique data, say for a server or user, the bot needs a unique identifier for said entity. Obviously, using the entity's name wouldn't work, since two entities can have the same name. That's where the Snowflakes come in. Since each server and each user has their own unique snowflake, they can be used to keep track of how many user the bot has, which server uses which prefix, and who made a particular character.

* The other type of data that the bot stores is the obvious kind: the information you intentionally give it. When you call a command to store a character, the bot has to store that information for future use. Wouldn't really work, otherwise. It also stores anonymized data for things like how many times each command has been used globally.
