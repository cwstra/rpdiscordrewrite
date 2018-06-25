import discord
from discord.ext import commands

import io
import random
import aiohttp
import re
import shlex
import cogs.specialized.dice as rollP
from collections import OrderedDict
from urllib.parse import quote as urlEscape
import ahocorasick
import os
import ujson as json

def specialstart(message, tup, ind):
    if message.startswith(tup, ind):
        for i in tup:
            if message.startswith(i, ind):
                return i
    else:
        return False

def getKeys(message, charsigns, charseps):
    print(charseps)
    signtup = tuple(i for i in sorted(charsigns, key=len, reverse=True))
    septup = tuple(i for i in sorted(charseps, key=len, reverse=True))
    ind = 0
    outdata = OrderedDict()  
    while ind < len(message):
        signtest = specialstart(message, signtup, ind)
        if signtest:
            outbegin = ind
            ind += len(signtest)
            begin = ind
            nextentry = False
            while ind < len(message):
                septest = specialstart(message, septup, ind)
                if septest:
                    nextentry = message[begin:ind]
                    ind += len(septest)
                    begin = ind
                    break
                else:
                    ind += 1
            if not(nextentry):
                break
            while ind < len(message):
                signtest = specialstart(message, signtup, ind)
                if signtest:
                    nextentry = (nextentry, message[begin:ind])
                    ind += len(signtest)
                    break
                else:
                    ind += 1
            if type(nextentry) == str:
                break
            outdata[nextentry] = (outbegin, ind)
        else:
            ind += 1
    return outdata

async def parseChars(ctx, message, charsigns, charseps, getInfo):
    data = getKeys(message, charsigns, charseps)
    res = await getInfo(ctx, data)
    for i,j in reversed(data):
        if (i, j) in res:
            message = message[:data[(i,j)][0]] + res[(i, j)] + message[data[(i,j)][1]:]
    return message 

class Roll:
    def __init__(self, bot):
        self.bot = bot
        self.bot.inline_roller = self.inline_roller
        self.rollParse = rollP.BasicRoll()
        self.rollPort = str(self.bot.settings['haskellport'])
        self.bot.loop.create_task(self.server_start())

    async def server_start(self):
        self.bot.statserver = await rollP.StatServer.create(self.bot.settings)

    #Handle charsign in here, rest in diceParser
    async def inline_roller(self, ctx):
        test = await self.bot.serverdata(ctx.guild.id, 'inline')
        if not(ctx.prefix==None and test):
            return
        charsigns = await self.bot.serverdata(ctx.guild.id, 'charsigns')
        if not(charsigns):
            charsigns = ['$']
        charseps = await self.bot.serverdata(ctx.guild.id, 'charseps')
        if not(charseps):
            charseps = [':']
        message = await parseChars(ctx, ctx.message.content, charsigns, charseps, self.bot.charserver.getBatchInfo)
        await self.rollParse.roll(ctx, message)

    #Command-invoked roller
    @commands.command(aliases = ['r'])
    async def roll(self, ctx, *, args):
        """Command-invoked dice roller."""
        async with ctx.typing():
            print('roll: awaiting charsigns')
            charsigns = await self.bot.serverdata(ctx.guild.id, 'charsigns')
            if not(charsigns):
                charsign = ['$']
            prefix = str(ctx.author.display_name)+':\n'+args
            print('roll: awaiting chardata')
            charseps = await self.bot.serverdata(ctx.guild.id, 'charseps')
            if not(charseps):
                charseps = [':']
            print('roll: awaiting parseChars')
            message = await parseChars(ctx, args, charsigns, charseps, self.bot.charserver.getBatchInfo)
            if message != args:
                prefix += '=' + message
            prefix += '\n'
            seed = random.randint(0,18446744073709551615)
            url = 'http://localhost:'+self.rollPort+'/roll?roll='+urlEscape(message,safe='')+'&seed='+str(seed)
            print('roll: awaiting server')
            async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=20)) as client:
                message = await client.get(url)
                message = await message.text()
            if message.startswith('Roll Statistics:'):
                message = message.split('\n',3)
                if message[1] != '{}':
                    await self.bot.statserver.editStats(ctx, message[1])
                message = message[-1]
            print('roll: awaiting send')
            await self.bot.smartSend(ctx,prefix.replace('*','\*'), message,'```')


    #Displays roll statistics
    @commands.command(aliases = ['stats'])
    async def statistics(self, ctx, *, args=''):
        """Displays roll statistics from this server. Can be called with arguments to only see results for certain dice or users."""
        async with ctx.typing():
            servername = ctx.guild.name
            if args == '':
                res = await self.bot.statserver.viewStats(ctx)
                titlename = "All"
            else:
                parsedargs = shlex.split(args)
                test = ctx.guild.get_member_named(parsedargs[0])
                if test:
                    rejoined = ' '.join(parsedargs[1:])
                    if rejoined.strip():
                        res = await self.bot.statserver.viewStats(ctx, test.id, map(lambda x: x.strip(), rejoined.split(',')))
                    else:
                        res = await self.bot.statserver.viewStats(ctx, test.id)
                    titlename = test.display_name + "'s"
                elif args.startswith('myself'): 
                    args = args[6:].strip()
                    if args == '':
                        res = await self.bot.statserver.viewStats(ctx, ctx.author.id)
                    else:
                        res = await self.bot.statserver.viewStats(ctx, ctx.author.id, map(lambda x: x.strip(), args.split(',')))
                    titlename = ctx.author.display_name + "'s"
                else:
                    res = await self.bot.statserver.viewStats(ctx, dice=map(lambda x: x.strip(), args.split(',')))
                    titlename = 'All'
            if type(res) == str:
                await ctx.send(res)
            else:
                if len(res) <= 1:
                    send = ctx.send
                else:
                    send = ctx.author.send
                for i in res:
                    if type(res[i]) == str:
                        await send(res[i])
                    else:
                        graphname = await self.bot.statserver.plot(self.bot.loop, titlename, servername, i, res[i])
                        await send(file = discord.File(graphname))
                        os.remove(graphname)
                if len(res) > 1:
                    await ctx.send('Information sent via PM.')

    #Displays roll statistics
    @commands.command()
    async def text_stats(self, ctx, *, args=''):
        """DMs a JSON of dice statistics from this server. Can be called with arguments to only see results for certain dice or users."""
        async with ctx.typing():
            servername = ctx.guild.name
            if args == '':
                res = await self.bot.statserver.viewStats(ctx)
            else:
                parsedargs = shlex.split(args)
                test = ctx.guild.get_member_named(parsedargs[0])
                if test:
                    rejoined = ' '.join(parsedargs[1:])
                    if rejoined.strip():
                        res = await self.bot.statserver.viewStats(ctx, test.id, map(lambda x: x.strip(), rejoined.split(',')))
                    else:
                        res = await self.bot.statserver.viewStats(ctx, test.id)
                elif args.startswith('myself'): 
                    args = args[6:].strip()
                    if args == '':
                        res = await self.bot.statserver.viewStats(ctx, ctx.author.id)
                    else:
                        res = await self.bot.statserver.viewStats(ctx, ctx.author.id, map(lambda x: x.strip(), args.split(',')))
                else:
                    res = await self.bot.statserver.viewStats(ctx, dice=map(lambda x: x.strip(), args.split(',')))
            if type(res) == str:
                await ctx.author.send(res)
            else:
                for i in res:
                    if type(res[i]) == str:
                        await ctx.author.send(res)
                    else:
                        await ctx.author.send(json.dumps(res))
                await ctx.send('Information sent via PM.')


def setup(bot):
    bot.add_cog(Roll(bot))
