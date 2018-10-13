import discord
from discord.ext import commands

import io
import random
import aiohttp
import re
import shlex
from collections import OrderedDict
from urllib.parse import quote as urlEscape
import os
import ujson as json
import tempfile
import asyncio

if __name__ == "__main__":
    import specialized.dice as rollP
else:
    import cogs.specialized.dice as rollP

def specialstart(message, tup, ind):
    if message.startswith(tup, ind):
        for i in tup:
            if message.startswith(i, ind):
                return i
    else:
        return False

def getKeys(message, charsigns, charseps):
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

async def parseChars(ctx, message, charsigns, charseps, getInfo, logFun=None):
    data = getKeys(message, charsigns, charseps)
    res = await getInfo(ctx, data)
    for i,j in reversed(data):
        if (i, j) in res:
            result = res[(i,j)]
            nextData = getKeys(result, charsigns, charseps)
            if logFun:
                logFun(result)
                logFun(nextData)
            killSwitch = 0
            while nextData and killSwitch < 100:
                nextRes = await getInfo(ctx, nextData)
                for k, v in reversed(nextData):
                    logFun((k,v))
                    logFun(nextRes.keys())
                    logFun((k,v) in nextRes)
                    if (k,v) in nextRes:
                        logFun(nextRes[(k,v)])
                        result = result[:nextData[(k,v)][0]] + nextRes[(k, v)] + result[nextData[(k,v)][1]:]
                nextData = getKeys(result, charsigns, charseps)
                killSwitch += 1
            message = message[:data[(i,j)][0]] + result + message[data[(i,j)][1]:]
    return message 

urlRegex = re.compile(r'https?:\/\/[^\s]*\.[^\s]*')

def purgeURLs(message):
    test = urlRegex.search(message)
    while test:
        message = message[:test.start()] + message[test.end():]
        test = urlRegex.search(message)
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
        test = await self.bot.serverdata(ctx, 'inline')
        if not(ctx.prefix==None and test):
            return
        charsigns = await self.bot.serverdata(ctx, 'charsigns')
        if not(charsigns):
            charsigns = ['$']
        charseps = await self.bot.serverdata(ctx, 'charseps')
        if not(charseps):
            charseps = [':']
        message = await parseChars(ctx, purgeURLs(ctx.message.content), charsigns, charseps, self.bot.charserver.getBatchInfo)
        await self.rollParse.roll(ctx, message)

    async def resultShortener(self, ctx, prefix, message):
        l = message.split('\n')
        maxlen = max(map(len, l))
        if maxlen > 500:
            with tempfile.TemporaryFile() as t:
                t.write(bytes(message, 'UTF-8'))
                t.seek(0)
                await ctx.send(prefix, file = discord.File(t, filename = 'roll.txt'))
        else:
            await self.bot.smartSend(ctx,prefix.replace('*','\*'), message, '```\n', '```')

    #Command-invoked roller
    @commands.command(aliases = ['r'])
    async def roll(self, ctx, *, args):
        """Command-invoked dice roller."""
        async with ctx.typing():
            charsigns = await self.bot.serverdata(ctx, 'charsigns')
            if not(charsigns):
                charsigns = ['$']
            prefix = str(ctx.author.display_name)+':\n'+args
            charseps = await self.bot.serverdata(ctx, 'charseps')
            if not(charseps):
                charseps = [':']
            message = await parseChars(ctx, args, charsigns, charseps, self.bot.charserver.getBatchInfo, self.bot.logger)
            if message != args:
                prefix += '=' + message
            prefix += '\n'
            seed = random.randint(0,18446744073709551615)
            url = 'http://localhost:'+self.rollPort+'/roll?roll='+urlEscape(message,safe='')+'&seed='+str(seed)
            async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=20)) as client:
                message = await client.get(url)
                message = await message.text()
            if message.startswith('Roll Statistics:'):
                message = message.split('\n',3)
                if message[1] != '{}':
                    await self.bot.statserver.editStats(ctx, message[1])
                message = message[-1]
            await self.resultShortener(ctx, prefix, message)

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
