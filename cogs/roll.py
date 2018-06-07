import discord
from discord.ext import commands

import json
import random
import aiohttp
import re
import shlex
import cogs.specialized.dice as rollP
from collections import OrderedDict
from urllib.parse import quote as urlEscape

def specialstart(message, tup, ind):
    if message.startswith(tup, ind):
        for i in tup:
            if message.startswith(i, ind):
                return i
    else:
        return False

def getKeys(message, charsigns, chardata, authorstr):
    ind = 0
    begin = -1
    outbegin = -1
    data = OrderedDict()
    key = ''
    private = False
    signtup = tuple(i for i in sorted(charsigns, key=len, reverse=True))
    chartup = tuple(i for i in sorted(chardata, key=len, reverse=True))
    while ind < len(message):
        signtest = specialstart(message, signtup, ind)
        if signtest:
            outbegin = ind
            ind += len(signtest)
            chartest = specialstart(message, chartup, ind)
            tempdict = None
            if message.startswith('p',ind) and not(chartest):
                opt = message.find('>', ind)
                if message.startswith('<@', ind+1) and message[ind+1].isdigit() and opt>-1:
                    opt = message[2:opt]
                    lopt = len(opt)
                    try:
                        int(opt)
                        ind += 3+lopt
                    except ValueError:
                        opt = authorstr
                else:
                    opt = authorstr
                signtest = specialstart(message, signtup, ind+1)
                if signtest:
                    lsigntest = len(signtest)
                    signtest = specialstart(message, signtup, ind+1+lsigntest)
                    if signtest:
                        ind += 1+lsigntest+len(signtest)
                elif message.startswith('::', ind+1):
                    ind += 1+2
                tempkeys = ([i for i in chardata if not(u"\uFEFF" in i)], [i.split(u"\uFEFF",1) for i in chardata if (i.find(u"\uFEFF")>-1 and i.split(u"\uFEFF",1)[0]==opt)])
                tempdict = {**{i:i for i in tempkeys[0] if not(i in tempkeys[1])}, **{i[1]:i[0] for i in tempkeys[1]}}
                temptup = tuple(i for i in sorted(tempdict, key=len, reverse=True))
                chartest = specialstart(message, temptup, ind)
            if chartest:
                key = chartest
                ind += len(chartest)
                if message.startswith(':', ind):
                    ind += 1
                else:
                    signtest = specialstart(message, signtup, ind)
                    if signtest:
                        ind += len(signtest)
                    else:
                        continue
                chartest = specialstart(message, tuple(chardata[key]), ind)
                if chartest:
                    value = chartest
                    ind += len(chartest)
                signtest = specialstart(message, signtup, ind)
                if signtest:
                    if tempdict:
                        key = tempdict[key]+u"\uFEFF"+key
                    ind += len(signtest)
                    data[key]=(value, outbegin, ind)
        else:
            ind += 1
    return data

async def parseChars(ctx, message, charsigns, chardata, getInfo):
    data = getKeys(message, charsigns, chardata, str(ctx.author.id))
    res = await getInfo(ctx, data)
    out = message
    for i in reversed(data):
        if i in res:
            out = out[:data[i][1]] + res[i] + out[data[i][2]:]
    return out

class Roll:
    def __init__(self, bot):
        self.bot = bot
        self.bot.inline_roller = self.inline_roller
        self.rollParse = {'basic':rollP.BasicRoll(), 'adv':aiohttp.ClientSession()}
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
        chardata = await self.bot.charserver.getServerInfo(ctx)
        chardata = {i['id'].split(u"\uFEFF",1)[1]:i['attributes'] for i in chardata}
        message = await parseChars(ctx, ctx.message.content, charsigns, chardata, self.bot.charserver.getBatchInfo)
        await self.rollParse['basic'].roll(ctx, message)

    #Command-invoked roller
    @commands.command(aliases = ['r'])
    async def roll(self, ctx, *, args):
        """Command-invoked dice roller."""
        async with ctx.typing():
            charsigns = await self.bot.serverdata(ctx.guild.id, 'charsigns')
            if not(charsigns):
                charsign = ['$']
            prefix = str(ctx.author.display_name)+':\n'+args
            chardata = await self.bot.charserver.getServerInfo(ctx)
            chardata = {i['id'].split(u"\uFEFF",1)[1]:i['attributes'] for i in chardata}
            message = await parseChars(ctx, args, charsigns, chardata, self.bot.charserver.getBatchInfo)
            if message != args:
                prefix += '=' + message
            prefix += '\n'
            seed = random.randint(0,18446744073709551615)
            message = await self.rollParse['adv'].get('http://localhost:'+self.rollPort+'/roll?roll='+urlEscape(message,safe='')+'&seed='+str(seed))
            message = await message.text()
            if message.startswith('Roll Statistics:'):
                message = message.split('\n',3)
                #if message[1] != '{}':
                #    await self.bot.statserver.editStats(message[1])
                message = message[-1]
            await self.bot.smartSend(ctx,prefix.replace('*','\*'), message,'```')


    #Displays roll statistics
    #@commands.command()
    #async def statistics(self, ctx, *, args=''):
        """Displays roll statistics for a given user in this channel. Can be called with arguments to only see results for certain dice."""
        """async with ctx.typing():
            if args == '':
                res = await self.bot.statserver.viewStats(ctx)
            else:
                if args.startswith('myself'):
                    args = args[6:].strip()
                    if args == '':
                        res = await self.bot.statserver.viewStats(ctx, ctx.author)
                    else:
                        res = await self.bot.statserver.viewStats(ctx, ctx.author, map(lambda x: x.strip(), args.split(',')))
                else:
                    parsedargs = shlex.split(args)
                    test = ctx.guild.get_member_named(parsedargs[0])
                    if test:
                        rejoined = ' '.join(parsedargs[1:])
                        res = await self.bot.statserver.viewStats(ctx, test, map(lambda x: x.strip(), rejoined.split(',')))
                    else:
                        res = await self.bot.statserver.viewStats(ctx, dice=map(lambda x: x.strip(), args.split(',')))
            if type(res) == str:
                await ctx.send(res)
            else:
                for i in res:
                    if type(res[i]) == str:
                        await ctx.send(res[i])
                    else:
                        await ctx.send()"""

def setup(bot):
    bot.add_cog(Roll(bot))
