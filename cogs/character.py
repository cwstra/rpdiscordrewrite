import discord
from discord.ext import commands

import cogs.specialized.character as db
import shlex

def is_allowed(ctx):
        permObject = None
        for i in permissions:
            if i['guildID']==ctx.channel.id:
                permObject = i
                break
        if not(permObject):
            permObject = {"guildID":ctx.channel.id,"roles":[],"admin":true}
            permissions.append(permObject)
            self.bot.setData("permissions", permissions)
        #self.is_owner(ctx.author) or 
        test = (permObject['admin'] and ctx.author.permissions_in(ctx.channel))
        test = test or (ctx.author.roles and any(i.id in permObject['roles'] for i in ctx.author.roles))
        return test

def parseMarkedArgs(ctx, args, privstr, namecheck=False):
    out = {}
    while True:
        if args[0] == '--a':
            author, args = args[1], args[2:]
            if author.startswith('<'):
                author = author[2:-1]
            try:
                out['author'] = int(author)
            except ValueError:
                out['author'] = None
        elif args[0] == '--p':
            private, args = args[1], args[2:]
            if private.lower() == 'true':
                private = True
            elif private.lower() == 'false':
                private = False
            out[privstr] = private
        elif args[0] == ('--n') and namecheck:
            name, args = args[1], args[2:]
            out['name'] = name
        else:
            break
    return (out, args)

def parseAttrArgs(ctx, args):
    out = {}
    while len(args) > 0:
        print(args)
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
    print(out)
    return out

class Character:
    def __init__(self, bot):
        self.bot = bot
        self.bot.loop.create_task(self.server_start())

    async def server_start(self):
        self.bot.charserver = await db.Server.create(self.bot.settings)

    @commands.command()
    async def newchar(self, ctx, *, args):
        """Creates a new stored character for this server.
            Syntax: 
                newchar <--a author_ping/author_id> <--p True/False> [character name] <list of attr = value>
            Required Fields
                [character name]: The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
            Optional Fields
                <attr = value>: Zero or more Name=Value pairs. If Name or Value contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
            Options:
                <--a author_ping/author_id>: Tell the bot to treat you as author for the sake of private characters. Only users authorized to edit the bot on this server can make use of this option.
                <--p True/False>: Create a private character. This character will be invisible to anyone but you, or users pretending to be you. Defaults to False."""
        args = shlex.split(args)
        argdict, args = parseMarkedArgs(ctx, args, 'private')
        if not('author' in argdict):
            argdict['author'] = None
        if not('private' in argdict):
            argdict['private'] = False
        if len(args) == 0:
            await ctx.send("At the very least, I need a name to make a new character.")
            return
        character, args = args[0], args[1:]
        if u"\uFEFF" in character:
            await ctx.send("Sorry, but unicode FEFF isn't allowed in character names, as it's invisible, and is used in internal storage.")
            return
        attrdict = parseAttrArgs(ctx, args)
        try:
            msg = 'Character "'+ character + '" successfully created'
            if attrdict:
                if len(attrdict) == 1:
                    attrs = next(iter(attrdict))
                    msg += ', with attribute '+attrs+'!'
                elif len(attrdict) == 2:
                    attrs = ' and '.join(attrdict.keys())
                    msg += ', with attributes '+attrs+'!'
                else:
                    attrs = ', and '.join(', '.join(attrdict.keys()).rsplit(', ', 1))
                    msg += ', with attributes '+attrs+'!'
            else:
                msg += '!'
            await self.bot.charserver.newInfo(ctx, character, attrdict, **argdict)
        except db.CharExistsException:
            msg = 'That character "'+ character + '" already exists.'
        await ctx.send(msg)

    @commands.command()
    async def viewchar(self, ctx, *, args):
        """Gets information about an existing character. If the command has no attribute fields, it will return a list of attributes the character has. Otherwise, it will return the values of the requested attributes.
            Syntax: 
                viewchar <--a author_ping> <--p True/False> [character name] <attributes>
            Required Fields
                [character name]: The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
            Optional Fields
                <attributes>: A space-separated list of attribute names. If a name contains spaces, quotation marks are necessary. If a name contains quotation marks, they must be escaped with \.
            Options:
                <--a author_ping/author_id>: Tell the bot to treat you as author for the sake of private characters. Only users authorized to edit the bot on this server can make use of this option.
                <--p True/False>: Attempt to search for a private character belonging to you under character_name. If that fails, will search the public characters for this server. If False, only searches the server's public characters. Defaults to False."""
        args = shlex.split(args)
        argdict, args = parseMarkedArgs(ctx, args, 'private')
        if not('author' in argdict):
            argdict['author'] = None
        if not('private' in argdict):
            argdict['private'] = False
        if len(args) == 0:
            await ctx.send("You'll need to give me the name of the character you want to look up.")
            return
        elif len(args) == 1:
            character, attrs = args[0], []
        else:
            character, attrs = args[0], args[1:]
        try:
            res = await self.bot.charserver.getInfo(ctx, character, attrs, **argdict)
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
        """except Exception as e:
            if len(e.args) == 1:
                await self.bot.smartSend(ctx, "An unexpected occured, with message:\n", e.args[0], '```')
            elif e.args:
                await self.bot.smartSend(ctx, "An unexpected error occured, with messages:\n"+'\n'.join(e.args), '```')
            else:
                await self.bot.smartSend(ctx, "An unexpected error occured, with no message.", '```')
            return"""
        await ctx.send(msg)

    @commands.command()
    async def editchar(self, ctx, *, args):
        """Edits an existing character's attributes.
            Syntax: 
                editchar <--a author_ping> <--n new_name> <--p True/False> [character name] [list of attr = value]
            Required Fields
                [character name]: The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
                [list of attr = value]: One or more Name=Value attribute pairs, or lone attribute Names; if it's a single attribute name, that attribute will be deleted, if it exists. If Name or Value contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
            Options:
                <--a author_ping/author_id>: Tell the bot to treat you as author for the sake of private characters. Only users authorized to edit the bot on this server can make use of this option.
                <--n new_name>: Tell the bot to rename your character. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
                <--p True/False>: Attempt to search for a private character belonging to you under character_name. If that fails, will search the public characters for this server. If False, only searches the server's public characters. Defaults to False."""
        args = shlex.split(args)
        argdict, args = parseMarkedArgs(ctx, args, 'private', True)
        if not('author' in argdict):
            argdict['author'] = None
        if not('private' in argdict):
            argdict['private'] = False
        if not('name' in argdict):
            argdict['name'] = None
        elif u"\uFEFF" in argdict['name']:
            argdict['name'] = None
            await ctx.send('Name Change cancelled; illegal character unicode FEFF found.')
        if len(args) == 0:
            await ctx.send("You'll need to give me the name of the character you want to edit.")
            return
        elif len(args) == 1:
            character, args = args[0], []
        else:
            character, args = args[0], args[1:]
        print(args)
        attrdict = parseAttrArgs(ctx, args)
        print(attrdict)
        if not(attrdict) and argdict['name'] == None:
            await ctx.send("You'll need to give me some attributes to change.")
            return
        print(argdict)
        try:
            if argdict['name'] == None:
                argdict.pop('name')
                await self.bot.charserver.editInfo(ctx, character, attrdict, **argdict)
                msg = 'Character '+character+' edited!'
            else:
                newcharacter = argdict.pop('name')
                if attrdict:
                    await self.bot.charserver.editInfo(ctx, character, attrdict, **argdict)
                    msg = 'Character '+character+' edited and renamed to '+newcharacter+'!'
                else:
                    msg = 'Character '+character+' renamed to '+newcharacter+'!'
                await self.bot.charserver.renameInfo(ctx, character, newcharacter, **argdict)
        except db.CharDoesNotExistException:
            msg = 'The character "'+ character + '" does not exist.'
        await ctx.send(msg)

    @commands.command()
    async def delchar(self, ctx, *, args):
        """Deletes an existing character.
            Syntax: 
                delchar <--a author_ping> <--p True/False> [character name]
                Required Fields
                    [character name]: The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with \.
                Options:
                    <--a author_ping/author_id>: Tell the bot to treat you as author for the sake of private characters. Only users authorized to edit the bot on this server can make use of this option.
                    <--p True/False>: Attempt to search for a private character belonging to you under character_name. If that fails, will search the public characters for this server. If False, only searches the server's public characters. Defaults to False."""
        args = shlex.split(args)
        argdict, args = parseMarkedArgs(ctx, args, 'private')
        if not('author' in argdict):
            argdict['author'] = None
        if not('private' in argdict):
            argdict['private'] = False
        if len(args) == 0:
            await ctx.send("You'll need to give me the name of the character you want to delete.")
            return
        character = args[0]
        test = await self.bot.charserver.checkInfo(ctx, character, **argdict)
        if not(test[0]):
            await ctx.send('That character does not exist.')
            return
        elif test[1]:
            if argdict['author']:
                creator = discord.utils.get(ctx.guild.members, id=argdict['author']).display_name
                msg = 'Are you sure you want to delete '+creator+"'s private character "+character+'"? Type "'+ctx.prefix+'Yes" or "'+ctx.prefix+'No" to confirm.'
            else:
                msg = 'Are you sure you want to delete your private character '+character+'"? Type "'+ctx.prefix+'Yes" or "'+ctx.prefix+'No" to confirm.'
        else:
            msg = 'Are you sure you want to delete '+character+'"? Type "'+ctx.prefix+'Yes" or "'+ctx.prefix+'No" to confirm.'
        await ctx.send(msg)
        async def f(name):
            if name == "Yes":
                try:
                    await self.bot.charserver.delInfo(ctx, character, **argdict)
                    if test[1]:
                        if argdict['author']:
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