import discord
from discord.ext import commands

import shlex
import json
import cogs.specialized.ref as db
from cogs.utils.SimplePaginator import SimplePaginator

def test_int(i):
    try:
        int(i)
        return True
    except:
        return False

class Ref(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.bot.loop.create_task(self.server_start())

    async def server_start(self):
        self.bot.refserver = await db.Server.create(self.bot.settings)

    @commands.command(aliases=['metronome'])
    async def random_entry(self, ctx, *, category):
        """Fetches a random entry in the given category from the codex."""
        codex = await self.bot.serverdata(ctx, 'codex')
        freeze = await self.bot.serverdata(ctx, 'freeze_pages')
        test = category.split()
        forceEmbed = False
        if test[0] == '--paginator' or test[0] == '--page':
            if test[1].lower() == 'false' or test[1].lower() == 'f':
                forceEmbed = True
            category = ' '.join(test[2:])
        if codex:
            info = await self.bot.refserver.random(codex, category)
            if type(info)==str:
                await ctx.send(info.format(ctx.author.display_name))
            else:
                await self.bot.pageOrEmbed(ctx, info, self.bot.logger, freeze, forceEmbed)
        else:
            await ctx.send('This server has no codex selected.')

    #gets data from the server's codex
    @commands.command()
    async def ref(self, ctx, *, args):
        """Look up info from this server's codex."""
        codex = await self.bot.serverdata(ctx, 'codex')
        freeze = await self.bot.serverdata(ctx, 'freeze_pages')
        test = args.split()
        forceEmbed = False
        if test[0] == '--paginator' or test[0] == '--page':
            if test[1].lower() == 'false' or test[1].lower() == 'f':
                forceEmbed = True
            args = ' '.join(test[2:])
        if codex:
            info = await self.bot.refserver.ref(codex, args)
            if type(info)==str:
                await ctx.send(info.format(ctx.author.display_name))
            else:
                await self.bot.pageOrEmbed(ctx, info, self.bot.logger, freeze, forceEmbed)
                """minfo = {i:j for i, j in info.items() if i!='fields'}
                embed = discord.Embed(**minfo)
                for i in info['fields']:
                    embed.add_field(**i)
                await ctx.send(None, embed = embed)"""
        else:
            await ctx.send('This server has no codex selected.')

    #gets data from the server's codex
    @commands.command()
    async def schema(self, ctx, *, args=''):
        """Look up the data structure for the server's codex, or individual entries in its codex."""
        codex = await self.bot.serverdata(ctx, 'codex')
        if codex:
            dis_codex = await self.bot.systemlist(codex)
            info = await self.bot.refserver.schema(codex, args)
            if args == '':
                pref = "Schema for the "+dis_codex+" codex:"
            else:
                pref = "Schema for the search "+args+' in the '+dis_codex+" codex:"
            await self.bot.smartSend(ctx, pref, info,'```\n','```')
        else:
            await ctx.send('This server has no codex selected.')

    #gets the top n results from the server's codex
    @commands.command()
    async def top(self, ctx, *, args):
        """Finds best matches for a search term in this server's codex."""
        def header(n, mess, coll):
            if coll:
                return f"Top {n} results for `{mess}` in `{coll}`:"
            else:
                return f"Top {n} results for `{mess}`:"
        codex = await self.bot.serverdata(ctx, 'codex')
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
            (info, coll) = await self.bot.refserver.top(codex, n, mess, self.bot.logger)
            if n > 5:
                await self.bot.smartSend(ctx.author, header(n, mess, coll), info,'```\n', '```')
                await ctx.send("Results sent via PM.")
            else:
                await self.bot.smartSend(ctx, header(n, mess, coll), info,'```\n', '```')
        else:
            await ctx.send('This server has no codex selected.')

def setup(bot):
    bot.add_cog(Ref(bot))
