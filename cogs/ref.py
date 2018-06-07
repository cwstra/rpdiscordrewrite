import discord
from discord.ext import commands

import json
import cogs.specialized.ref as db

def test_int(i):
    try:
        int(i)
        return True
    except:
        return False

class Ref:
    def __init__(self, bot):
        self.bot = bot
        self.bot.loop.create_task(self.server_start())

    async def server_start(self):
        self.bot.refserver = await db.Server.create(self.bot.settings)

    #gets data from the server's codex
    @commands.command()
    async def ref(self, ctx, *, args):
        """Look up info from this server's codex."""
        codex = await self.bot.serverdata(ctx.guild.id, 'codex')
        if codex:
            info = await self.bot.refserver.ref(codex, args)
            if type(info)==str:
                await ctx.send(info.format(ctx.author.display_name))
            else:
                minifo = {i:j for i, j in info.items() if i!='fields'}
                embed = discord.Embed(**minifo)
                for i in info['fields']:
                    embed.add_field(**i)
                await ctx.send(None, embed = embed)
        else:
            await ctx.send('This server has no codex selected.')

    #gets data from the server's codex
    @commands.command()
    async def schema(self, ctx, *, args=''):
        """Look up the data structure for the server's codex, or individual entries in its codex."""
        codex = await self.bot.serverdata(ctx.guild.id, 'codex')
        if codex:
            info = await self.bot.refserver.schema(codex, args)
            if args == '':
                pref = "Schema for the "+codex+" codex:"
            else:
                pref = "Schema for the search "+args+' in the '+codex+" codex:"
            await self.bot.smartSend(ctx, pref, info,'```\n','```')
        else:
            await ctx.send('This server has no codex selected.')

    #gets the top n results from the server's codex
    @commands.command()
    async def top(self, ctx, *, args):
        """Finds best matches for a search term in this server's codex."""
        codex = await self.bot.serverdata(ctx.guild.id, 'codex')
        if codex:
            if ' ' in args:
                n, mess = args.split(' ', 1)
                if test_int(n):
                    n = int(n)
                else:
                    n = 5
                    mess = args
            else:
                n = 5
                mess = args
            info = await self.bot.refserver.top(codex, n, mess)
            if n > 5:
                await self.bot.smartSend(ctx.author,"Top "+str(n)+" results for "+args+":", info,'```')
                await ctx.send("Results sent via PM.")
            else:
                await self.bot.smartSend(ctx,"Top "+str(n)+" results for "+args+":", info,'```')
        else:
            await ctx.send('This server has no codex selected.')

def setup(bot):
    bot.add_cog(Ref(bot))
