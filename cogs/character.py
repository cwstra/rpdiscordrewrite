import discord
from discord.ext import commands

import cogs.specialized.character as db
import shlex
from cogs.utils.SimplePaginator import SimplePaginator

def parseMarkedArgs(ctx, args, namecheck=False):
    out = {}
    while True:
        if args[0] == ('--n') and namecheck:
            name, args = args[1], args[2:]
            out['name'] = name
        else:
            break
    return (out, args)

def parseAttrArgs(ctx, args):
    out = {}
    while len(args) > 0:
        if args[0].endswith('=') and not(args[0].endswith('\=')) and len(args)>1:
            out[args[0][:-1]], args = args[1], args[2:]
        elif args[0].count('=') > args[0].count('\='):
            eqspl, attr, value = args[0].split('='), '', ''
            test = False
            for i in eqspl:
                if test:
                    value += i + '='
                else:
                    if i.endswith('\\'):
                        attr += i+'='
                    else:
                        attr += i
                        test = True
            value = value[:-1]
            out[attr], args = value, args[1:]
        else:
            if len(args) == 1:
                out[args[0]], args = None, args[1:]
            elif args[1] == ('='):
                out[args[0]], args = args[2], args[3:]
            elif args[1].startswith('='):
                out[args[0]], args = args[1][1:], args[2:]
            else:
                out[args[0]], args = None, args[1:]
    return out

def smartPlural(objectlist):
    if len(objectlist) == 1:
        return objectlist[0]
    elif len(objectlist) == 2:
        return objectlist[0] + ' and ' + objectlist[1]
    else:
        return ', '.join(objectlist[:-1]) + ', and '+ objectlist[-1]

def evensplit(l, n):
    for i in range(0, len(l), n):
        yield l[i:i+n]

def splitlist(text):
    if len(text) > 10:
        return map(lambda x: '\n'.join(x), evensplit(text, 10))
    else:
        return ['\n'.join(text)]

async def charList(ctx, title, lines):
    def toembed(d):
        fields = d.pop('fields', []) if 'fields' in d else []
        em = discord.Embed(**d)
        for i in fields:
            em.add_field(**i)
        return em
    embeds = [toembed({'title':title, 'description':i}) for i in splitlist(lines)]
    await SimplePaginator(extras=embeds).paginate(ctx)

class Character:
    def __init__(self, bot):
        self.bot = bot
        self.bot.loop.create_task(self.server_start())

    async def server_start(self):
        self.bot.charserver = await db.Server.create(self.bot.settings)
    
    def authOr(self, ctx):
        async def f(authorId):
            allowedroles = await self.bot.serverdata(ctx, 'permissionroles')
            test = ctx.author.id == authorId or ctx.author.permissions_in(ctx.channel) or (ctx.author.roles and any(i.id in allowedroles for i in ctx.author.roles))
        return f 

    @commands.command()
    async def newchar(self, ctx, *, args):
        """Creates a new stored character for this server.
            Syntax:
                newchar [character name] <list of attr = value>
            Required Fields
                [character name]: The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
            Optional Fields
                <attr = value>: Zero or more Name=Value pairs. If Name or Value contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \."""
        args = shlex.split(args)
        if len(args) == 0:
            await ctx.send("At the very least, I need a name to make a new character.")
            return
        character, args = args[0], args[1:]
        attrdict = parseAttrArgs(ctx, args)
        try:
            await self.bot.charserver.newInfo(ctx, character, attrdict)
            msg = 'Character "'+ character + '" successfully created'
            if attrdict:
                msg += ', with attribute'+(' ' if len(attrdict)==1 else 's ')+smartPlural(list(attrdict.keys()))
            msg += '!'
        except db.CharExistsException:
            msg = 'That character ("'+ character + '") already exists.'
        await ctx.send(msg)

    @commands.command()
    async def viewchar(self, ctx, *, args=''):
        """Gets information about an existing character. If the command has no attribute fields, it will return a list of attributes the character has. Otherwise, it will return the values of the requested attributes.
            Syntax:
                viewchar [character name] <attributes>
            Required Fields
                [character name]: The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
            Optional Fields
                <attributes>: A space-separated list of attribute names. If a name contains spaces, quotation marks are necessary. If a name contains quotation marks, they must be escaped with \."""
        args = shlex.split(args)
        if len(args) == 0:
            res = await self.bot.charserver.listInfo(ctx)
            msg = []
            for i in res:
                charname, authorname = i[0], ctx.guild.get_member(i[1])
                authorname = authorname.display_name if authorname else 'a former member of the server'
                msg.append(f'`{charname}`, created by {authorname}')
            await charList(ctx, 'Character Paginator', msg)
            return
        elif len(args) == 1:
            character, attrs = args[0], []
        else:
            character, attrs = args[0], args[1:]
        try:
            res = await self.bot.charserver.getInfo(ctx, character, attrs)
            if attrs:
                await self.bot.smartSend(ctx, '"'+ character + '"'+"'s requested attributes are:", '\n'.join([i+': '+j for i,j in res.items()]))
            else:
                creator = discord.utils.get(ctx.guild.members, id=res[0]).display_name
                if res[1]:
                    await self.bot.smartSend(ctx, 'The character "'+ character + '", created by '+creator+', has attributes:\n', ', '.join(res[1]))
                else:
                    await ctx.send('The character "'+ character + '", created by '+creator+', has no attributes.')
            return
        except db.CharDoesNotExistException:
            msg = 'The character "'+ character + '" does not exist.'
        except db.AttrDoesNotExistException as e:
            await self.bot.smartSend(ctx, 'The character "'+ character + '" has none of those attributes. The character'+"'s existing attributes are:", '\n'.join(e.args[2])+'\nUse editchar to change this.')
            return
        await ctx.send(msg)

    @commands.command()
    async def editchar(self, ctx, *, args):
        """Edits an existing character's attributes.
            Syntax:
                editchar <--n new_name> [character name] [list of attr = value]
            Required Fields
                [character name]: The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
                [list of attr = value]: One or more Name=Value attribute pairs, or lone attribute Names; if it's a single attribute name, that attribute will be deleted, if it exists. If Name or Value contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
            Options:
                <--n new_name>: Tell the bot to rename your character. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \."""
        args = shlex.split(args)
        argdict, args = parseMarkedArgs(ctx, args, True)
        if not('name' in argdict):
            argdict['name'] = None
        if len(args) == 0:
            await ctx.send("You'll need to give me the name of the character you want to edit.")
            return
        elif len(args) == 1:
            character, args = args[0], []
        else:
            character, args = args[0], args[1:]
        attrdict = parseAttrArgs(ctx, args)
        if not(attrdict) and argdict['name'] == None:
            await ctx.send("You'll need to give me some attributes to change.")
            return
        try: #(ctx, charactername, predicate, newattrdict, newname=None)
            if argdict['name'] == None:
                await self.bot.charserver.editInfo(ctx, character, self.authOr, attrdict)
                msg = 'Character '+character+' edited!'
            else:
                newcharacter = argdict.pop('name')
                await self.bot.charserver.editInfo(ctx, character, self.authOr, attrdict, newname = newcharacter)
                if attrdict:
                    msg = 'Character '+character+' edited and renamed to '+newcharacter+'!'
                else:
                    msg = 'Character '+character+' renamed to '+newcharacter+'!'
        except db.CharDoesNotExistException:
            msg = 'The character "'+ character + '" does not exist.'
        except db.EditNotAllowedException as e:
            msg = 'That character was made by someone else, namely '+ discord.utils.get(ctx.guild.members, id=e.args[1]).display_name +'. Only the original creator, an admin, or a server member with a bot permissions role can edit it.'
        await ctx.send(msg)

    @commands.command()
    async def delchar(self, ctx, *, args):
        """Deletes an existing character.
            Syntax:
                delchar [character name]
                Required Fields
                    [character name]: The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \."""
        args = shlex.split(args)
        if len(args) == 0:
            await ctx.send("You'll need to give me the name of the character you want to delete.")
            return
        character = args[0]
        try:
            test = await self.bot.charserver.getInfo(ctx, character)
        except db.CharDoesNotExistException:
            await ctx.send("That character does not exist.")
            return
        if not(self.authOr(test[0])):
            await ctx.send('That character was made by someone else. Only the original creator, an admin, or a server member with a bot permissions role can delete it.')
        msg = 'Are you sure you want to delete '+character+'"? Type "'+ctx.prefix+'Yes" or "'+ctx.prefix+'No" to confirm.'
        await ctx.send(msg)
        async def f(name):
            if name == "Yes":
                try:
                    await self.bot.charserver.delInfo(ctx, character)
                    if test[1]:
                        if ctx.author.id != test[0]:
                            creator = discord.utils.get(ctx.guild.members, id=test[0]).display_name
                            await ctx.send(creator+"'s"+' character "'+ character + '" has been deleted.')
                        else:
                            await ctx.send('Your character "'+ character + '" has been deleted.')
                    else:
                        await ctx.send('The character "'+ character + '" has been deleted.')
                except db.CharDoesNotExistException as e: #except Exception as e:
                    if e.args:
                        await ctx.send('Something weird happened while deleting:\n' +'\n'.join(str(e.args)))
                    else:
                        await ctx.send('Something weird happened while deleting.')
            else:
                await ctx.send('Character deletion cancelled.')
        d = {'prefix':ctx.prefix, 'function':f, 'options':['Yes', 'No']}
        if not(ctx.channel.id in self.bot.waiting):
            self.bot.waiting[ctx.channel.id] = {}
        self.bot.waiting[ctx.channel.id][ctx.author.id] = d

def setup(bot):
    bot.add_cog(Character(bot))
