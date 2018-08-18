import discord
from discord.ext import commands

import json
import time

def smartSurround(string):
    if string.startswith('<@') and string.find('>') > -1:
        return string
    return f'`{string}`'

def smartListToString(l):
    if len(l) == 1:
        return smartSurround(str(l[0]))
    if len(l) == 2:
        return ' and '.join(map(smartSurround, l))
    return ' and '.join(', '.join(map(smartSurround, l)).rsplit(', ', 1))

class Initialize:
    def __init__(self, bot):
        self.bot = bot

    async def is_allowed(self, ctx):
        permObject = None
        allowedroles = await self.bot.serverdata(ctx, 'permissionroles')
        test = ctx.author.permissions_in(ctx.channel) or (ctx.author.roles and any(i.id in allowedroles for i in ctx.author.roles))
        return test

    #Links the github page
    @commands.command(aliases = ['init'])
    async def initialize(self, ctx):
        """Lists important commands for setting the bot up for initial use."""
        msg = f"Hello {ctx.author.display_name}! Here's some things you should know to set me up!\n\n"
        currentInfo = await self.bot.serverdata(ctx, 'prefixes')
        if not(currentInfo):
            currentInfo = [f'<@{self.bot.user.id}>']
        msg += f"To issue a command, you have to use a prefix. Currently, this server's prefix{'es are' if len(currentInfo)>1 else ' is'} {smartListToString(currentInfo)}.\n\n"
        msg += "To set me up for further use in your server, an admin can do the following: \n"
        msg += "\t* Use the `channel_prefix` / `server_prefix` commands to set up some alternate prefixes\n"
        msg += "\t* Use the `channel_codex` / `server_codex` commands to select a system codex for reference\n"
        msg += "\t* Use the `channel_inline_toggle` / `server_inline_toggle` commands to enable inline rolling, if that's your thing\n"
        msg += "\t* Use the `botmodrole` command to add roles that can edit my settings\n"
        msg += "To learn more, use the `docs` command to get a link to my documentation"
        await ctx.send(msg)

def setup(bot):
    bot.add_cog(Initialize(bot))
