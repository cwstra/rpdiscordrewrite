import discord
from discord.ext import commands

import json
import time

class Initialize:
    def __init__(self, bot):
        self.bot = bot

    async def is_allowed(self, ctx):
        permObject = None
        allowedroles = await self.bot.serverdata(ctx.guild.id, 'permissionroles')
        test = ctx.author.permissions_in(ctx.channel) or (ctx.author.roles and any(i.id in allowedroles for i in ctx.author.roles))
        return test

    #Links the github page
    @commands.command(aliases = ['init'])
    async def initialize(self, ctx):
        """Lists important commands for setting the bot up for initial use."""
        msg = "To set me up properly, an admin can do the following: \n"
        msg += "\t* Use the `prefix` command to set up a less cumbersome way to use me\n"
        msg += "\t* Use the `codex` command to select a system codex for reference\n"
        msg += "\t* Use the `inline_toggle` command to enable inline rolling, if that's your thing\n"
        msg += "\t* Use the `botmodrole` command to add roles that can edit my settings\n"
        msg += "To learn more, use the `docs` command to get a link to my documentation"
        await ctx.send(msg)

def setup(bot):
    bot.add_cog(Initialize(bot))
