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

<<<<<<< HEAD
def evensplit(l, n):
    for i in range(0, len(l), n):
        yield l[i:i+n]

def splittext(count, text):
    if count > 10:
        return list(map(lambda x: '\n'.join(x), evensplit(text.split('\n'), 10)))
    else:
        return text

def titlemake(tup):
    if type(tup[1]) == str:
        return [{'name':tup[0], 'value':tup[1]}]
    else:
        return [{'name':tup[0], 'value':tup[1][0]}] + [{'name':tup[0]+' (cont.)', 'value':i} for i in tup[1][1:]]

def splitbigfields(l):
    newlist = [(i, splittext(j, k)) for i, j, k in l]
    newlist = [titlemake(i) for i in newlist]
    outlist = []
    for i in newlist:
        outlist += i
    return outlist

def toembed(d, printFun, genFooter=None):
    fields = d.pop('fields', [])
    image = d.pop('image', None)
    footer = d.pop('footer', None)
    if not(footer) and genFooter:
        footer = genFooter
    printFun(image)
    em = discord.Embed(**d)
    for i in fields:
        em.add_field(**i)
    if image:
        em.set_image(url=image)
    if footer:
        em.set_footer(**footer)
    return em

async def pageOrEmbed(ctx, info, printFun, forceEmbed = False):
    def maybeover(key, l, n):
        if n < len(l):
            return {key:l[n]}
        else:
            return {}
    counts = {'description':info['description'].count('\n')+1 if 'description' in info else None, 'fields':[str(i['value']).count('\n')+1 for i in info['fields']], 'image':len(info['image']) if 'image' in info else 0}
    maxlines = max([counts['description'] if counts['description'] else 1]+[i for i in counts['fields']])
    baseembed = {'title':info['title']} ; iterables = {}
    if not(forceEmbed) and (len(info['fields'])>3 or maxlines>10 or ('image' in info and len(info['image'])>1)):
        if counts['description']:
            desc = splittext(counts['description'], info['description'])
            if type(desc) == str:
                baseembed['description'] = desc
            else:
                iterables['description'] = desc
        littlefields = splitbigfields([(i['name'], counts['fields'][ind], str(i['value'])) for ind, i in enumerate(info['fields'])])  
        if len(littlefields)>3:
            iterables['fields'] = list(evensplit(littlefields, 3))
        else:
            baseembed['fields'] = littlefields
        if 'image' in info and len(info['image']) == 1:
            printFun('baseImage')
            baseembed['image'] = info['image'][0]
        elif 'image' in info and len(info['image']) > 1:
            printFun('iterImage')
            iterables['image'] = info['image']
        repfields =  ('description', 'fields', 'image')
        embeds = [baseembed]
        for i in repfields:
            if i in iterables:
                embeds = [{**(embeds[j] if j<len(embeds) else baseembed), **maybeover(i, iterables[i], j)} for j in range(max(len(embeds), len(iterables[i])))]
        if 'footer' in info:
            footer = info['footer']
        else:
            footer = None
        embeds = [toembed(i, printFun, footer) for i in embeds]
        if len(embeds) == 1:
            await ctx.send(None, embed = embeds[0])
        else:
            await SimplePaginator(extras=embeds).paginate(ctx) 
    else:
        if 'image' in info and len(info['image']) == 1:
            info['image'] = info['image'][0]
        await ctx.send(None, embed = toembed(info, printFun))

=======
>>>>>>> 6758b352f18db1407fe2d7aa09d2d23eac061f3e
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
        codex = await self.bot.serverdata(ctx, 'codex')
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
                await self.bot.pageOrEmbed(ctx, info, self.bot.logger, forceEmbed)
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
            info = await self.bot.refserver.top(codex, n, mess)
            if n > 5:
                await self.bot.smartSend(ctx.author,"Top "+str(n)+" results for "+mess+":", info,'```\n', '```')
                await ctx.send("Results sent via PM.")
            else:
                await self.bot.smartSend(ctx,"Top "+str(n)+" results for "+mess+":", info,'```\n', '```')
        else:
            await ctx.send('This server has no codex selected.')

def setup(bot):
    bot.add_cog(Ref(bot))
