import discord
from discord.ext import commands

import time

#Zelda-based poker
class Fairy:
    def __init__(self):
        self.word = 'Watch Out!'

    def state(self):
        if self.word == 'Hey!':
            self.word = 'Listen!'
        elif self.word == 'Listen!':
            self.word = 'Watch Out!'
        else:
            self.word = 'Hey!'
        return self.word

class General(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.pokeEntities = {}
        self.git = self.bot.settings['git']
        self.discord_server = self.bot.settings['discord']
        self.doc = self.bot.settings['doc']
        self.donate = self.bot.settings['donate']
    
    @commands.command()
    async def server_link(self, ctx):
        """Sends the link to the bot discord server."""
        await ctx.send(self.discord_server)

    #Links the github page
    @commands.command()
    async def docs(self, ctx):
        """Sends the documentation link to the channel"""
        await ctx.send(self.doc)

    @commands.command()
    async def donation(self, ctx):
        """Sends links to donate to the upkeep of the bot."""
        await ctx.send(self.donate)

    #Links the github page
    @commands.command()
    async def github(self, ctx):
        """Sends the git link to the channel"""
        await ctx.send(self.git)

    @commands.command()
    async def invite(self, ctx):
        """Returns a link to invite the bot to a server you have the permissions to invite it to."""
        await ctx.send(discord.utils.oauth_url(self.bot.user.id))

    @commands.command()
    async def ping(self, ctx):
        """Calculates the ping time."""
        # [p]ping
        t_1 = time.perf_counter()
        await ctx.trigger_typing()  # tell Discord that the bot is "typing", which is a very simple request
        t_2 = time.perf_counter()
        time_delta = round((t_2-t_1)*1000)  # calculate the time needed to trigger typing
        await ctx.send("Pong.\nTime: {}ms".format(time_delta))  # send a message telling the user the calculated

    #If poked on a server, responds with the appropriate poker
    @commands.command()
    async def poke(self, ctx):
        """Pokes the bot, mostly for checking if it's online. Use ping to get a response time."""
        if not(ctx.channel.id in self.pokeEntities):
            self.pokeEntities[ctx.channel.id] = Fairy()
        await ctx.send(self.pokeEntities[ctx.channel.id].state())

def setup(bot):
    bot.add_cog(General(bot))
