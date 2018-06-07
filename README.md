# Discord Bot for Tabletop RPG Systems

## Intro

Hello folks! I've recently started GMing a *very* small system ([located here, for those interested](http://forums.pokemontabletop.com/topic/30120503/1/)), and, for a change of pace, the brunt of the gameplay is taking place in Discord. 

Naturally, I wanted a bot to handle rolling and such. While there were a few existing bots for these purposes, there weren't any that had all of the features I wanted. So I made my own.

## Prerequisites

The bot requires

* Python 3

* numpy

* scipy

* matplotlib

* asteval

* discord.py

* fuzzywuzzy

Python 3 is probably best installed by googling it.

numpy/scipy/matplotlib installation instructions are [here](https://scipy.org/install.html).

asteval installation instructions are [here](https://newville.github.io/asteval/installation.html).

Discord.py installation instructions are [here](https://github.com/Rapptz/discord.py).

fuzzywuzzy installation instructions are [here](https://pypi.python.org/pypi/fuzzywuzzy).

You will also need your own Discord bot set up, with token. Instructions for that are located [here](https://github.com/reactiflux/discord-irc/wiki/Creating-a-discord-bot-&-getting-a-token)

## Installing

1. Download the git file
2. Put your Discord bot's token into the corresponding part of settings.json
3. Run bot.py with Python 3
4. Type /help
5. Use the bot!

## Using the bot

By default, the bot's prefix is a forward slash (/). This can be changed in the settings.json, or by using the prefix command while interacting with the bot.

The bot itself has a "help" command, which *should* provide enough help for basic use. The bot was mostly made for my personal use, and I'm not terribly good at writeup, so please, ask as many questions and give as much feedback as you want. 
