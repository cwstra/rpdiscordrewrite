import discord
from discord.ext import commands

import json
import asyncio

ynlist = ['YES', 'YEs', 'YeS', 'yES', 'yeS', 'yEs', 'Yes', 'yes', 'NO', 'No', 'nO', 'no']

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

class Settings(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    async def is_allowed(self, ctx):
        permObject = None
        allowedroles = await self.bot.serverdata(ctx, 'permissionroles')
        test = ctx.author.permissions_in(ctx.channel).administrator or (allowedroles and any(i.id in allowedroles for i in ctx.author.roles))
        return test

    async def general_dialogue(self, ctx, initial, currentInfo, optionsList, generalFun):
        d = {'author': ctx.author.display_name}
        if currentInfo:
            d['current'] = currentInfo
        await ctx.send(initial.format(**d))
        d = {'prefix':ctx.prefix, 'function':generalFun, 'options':optionsList}
        if not(ctx.channel.id in self.bot.waiting):
            self.bot.waiting[ctx.channel.id] = {}
        self.bot.waiting[ctx.channel.id][ctx.author.id] = d

    async def symbol_list_change(self, ctx, arg, singtype, plurtype, default, usechannel):
        serv_or_chan = 'channel' if usechannel else 'server'
        currentInfo = await self.bot.serverdata(ctx, plurtype)
        if currentInfo == None:
            currentInfo = []
        initial = "{author}: "
        emptyTest = False
        options = []
        datalist = currentInfo if currentInfo else default
        initial += f"This {serv_or_chan}'s current {f'{singtype} is' if len(datalist)==1 else f'{plurtype} are'} {smartListToString(datalist)}"
        posslist = [i for i in currentInfo]
        if len(arg)==0:
            pre = (f'@{self.bot.user.display_name}' if ctx.prefix[2:-2]==str(self.bot.user.id) else ctx.prefix)
            initial += f"If you would like to add or remove a {singtype}, type {smartSurround(f'{pre}{serv_or_chan}_{singtype} <new {singtype}>')}."
            await ctx.send(initial.format(author=ctx.author.display_name))
            return
        elif arg==default and (len(currentInfo)==0 or (len(currentInfo)==1 and currentInfo[0]==default)):
            initial += f"You have to have _a_ {singtype}. To remove {smartSurround(default)} from your {singtype} list, you'll have to add another {singtype} first."
            await ctx.send(initial.format(author=ctx.author.display_name))
            return
        elif arg in currentInfo:
            initial += f"Would you like to remove {smartSurround(arg)} from your {singtype} list? Type {smartSurround(f'{ctx.prefix}Yes')} or {smartSurround(f'ctx.prefixNo')} to confirm."
            for i, v in enumerate(currentInfo):
                if v == arg:
                    posslist.pop(i)
                    break
        elif len(currentInfo)>10:
            initial += f"A given {serv_or_chan} can only have 10 {plurtype}. You'll have to remove some {plurtype} before you can add a new one."
            await ctx.send(initial.format(author=ctx.author.display_name))
            return
        else:
            initial += f"Would you like to add {smartSurround(arg)} to your {singtype} list? Type {smartSurround(f'{ctx.prefix}Yes')} or {smartSurround(f'{ctx.prefix}No')} to confirm."
            posslist.append(arg)
            if len(currentInfo) == 0:
                posslist += default
        posslist.sort(key = lambda x:len(x))
        async def change(name):
            if type(name) == str and name.strip().lower() == 'yes':
                await self.bot.upsert_entry(ctx, {plurtype:posslist}, usechannel)
                await ctx.send(f'{singtype.title()} list changed!')
            else:
                await ctx.send(f'{singtype.title()} list change cancelled.')
        await self.general_dialogue(ctx, initial, currentInfo, ynlist, change)

    async def remove_entry(self, ctx, entry_name, entry):
        if await self.is_allowed(ctx):
            channelcheck = await self.bot.serverfetcher.checkchannel(ctx, entry)
            if channelcheck != None:
                async def delete(test):
                    if test.lower() == "yes":
                        await self.bot.serverfetcher.remove_channel_entry(ctx, entry)
                        await ctx.send(f'Channel {entry_name} deleted!')
                    else:
                        await ctx.send(f'Channel {entry_name} deletion cancelled.')
                initial = "{author}: "
                initial += f"Currently, this channel has its own {entry_name}. If you would like to delete it, setting the channel to use the server's {entry_name}, type {smartSurround(f'{ctx.prefix}Yes')}. Otherwise, type {smartSurround(f'{ctx.prefix}No')}."
                await self.general_dialogue(ctx, initial, channelcheck, ynlist, delete)
            else:
                await ctx.send(f"This channel already uses the server's {entry_name}.")
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    async def prefix_root(self, usechannel, ctx, arg):
        if await self.is_allowed(ctx):
            await self.symbol_list_change(ctx, arg, 'prefix', 'prefixes', [f'<@{self.bot.user.id}>', f'<@{self.bot.user.id}>'], usechannel)
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def prefix(self,ctx, *,arg=''):
        """Forking command for bot prefixes"""
        if ctx.prefix.startswith('<@') and ctx.prefix.find('>')>-1:
            await ctx.send(f"Please use {ctx.prefix}`channel_prefix` or {ctx.prefix}`server_prefix` to specify the context of your prefix.")
        else:
            await ctx.send(f"Please use `{ctx.prefix}channel_prefix` or `{ctx.prefix}server_prefix` to specify the context of your prefix.")

    @commands.command()
    async def server_prefix(self,ctx, *,arg=''):
        """Changes the server prefix. Only available to admins or allowed roles. Will prompt for confirmation before changing the prefix."""
        await self.prefix_root(False, ctx, arg)

    @commands.command()
    async def channel_prefix(self,ctx,*,arg=''):
        """Changes the prefix for the channel the command is used in. Only available to admins or allowed roles. Will prompt for confirmation before changing the prefix."""
        await self.prefix_root(True, ctx, arg)

    @commands.command()
    async def remove_channel_prefix(self, ctx):
        """Deletes a channel's independent prefix list, if it exists. Only available to admins or allowed roles. Will prompt for confirmation before deleting the prefix list."""
        await self.remove_entry(ctx, 'prefix list', 'prefixes')

    async def charsign_root(self, usechannel, ctx, arg):
        if await self.is_allowed(ctx):
            if '@' in arg:
                await ctx.send(f"Sorry, {ctx.author.display_name}, but charsigns can't contain @.")
                return
            await self.symbol_list_change(ctx, arg, 'charsign', 'charsigns', ['$'], usechannel)
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def charsign(self,ctx, *,arg=''):
        """Forking command for bot charsign(s)"""
        if ctx.prefix.startswith('<@') and ctx.prefix.find('>')>-1:
            await ctx.send(f"Please use {ctx.prefix}`channel_charsign` or {ctx.prefix}`server_charsign` to specify the context of your charsign.")
        else:
            await ctx.send(f"Please use `{ctx.prefix}channel_charsign` or `{ctx.prefix}server_charsign` to specify the context of your charsign.")

    @commands.command()
    async def server_charsign(self,ctx,*,arg=''):
        """Changes the server charsign. Only available to admins or allowed roles. Will prompt for confirmation before changing the charsign."""
        await self.charsign_root(False, ctx, arg)

    @commands.command()
    async def channel_charsign(self,ctx,*,arg=''):
        """Changes the charsign for the channel the command is used in. Only available to admins or allowed roles. Will prompt for confirmation before changing the charsign."""
        await self.charsign_root(True, ctx, arg)

    @commands.command()
    async def remove_channel_charsign(self, ctx):
        """Deletes a channel's independent charsign list, if it exists. Only available to admins or allowed roles. Will prompt for confirmation before deleting the prefix list."""
        await self.remove_entry(ctx, 'charsign list', 'charsigns')

    async def charsep_root(self, usechannel, ctx, arg):
        if await self.is_allowed(ctx):
            if '@' in arg:
                await ctx.send(f"Sorry, {ctx.author.display_name}, but charseps can't contain @.")
                return
            await self.symbol_list_change(ctx, arg, 'charsep', 'charseps', [':'], usechannel)
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def charsep(self,ctx, *,arg=''):
        """Forking command for bot charsep(s)"""
        if ctx.prefix.startswith('<@') and ctx.prefix.find('>')>-1:
            await ctx.send(f"Please use {ctx.prefix}`channel_charsep` or {ctx.prefix}`server_charsep` to specify the context of your charsep.")
        else:
            await ctx.send(f"Please use `{ctx.prefix}channel_charsep` or `{ctx.prefix}server_charsep` to specify the context of your charsep.")

    @commands.command()
    async def server_charsep(self,ctx,*,arg=''):
        """Changes the server charsep. Only available to admins or allowed roles. Will prompt for confirmation before changing the charsign."""
        await self.charsep_root(False, ctx, arg)

    @commands.command()
    async def channel_charsep(self,ctx,*,arg=''):
        """Changes the charsep for the channel the command is used in. Only available to admins or allowed roles. Will prompt for confirmation before changing the charsign."""
        await self.charsep_root(True, ctx, arg)

    @commands.command()
    async def remove_channel_charsep(self, ctx):
        """Deletes a channel's independent charsep list, if it exists. Only available to admins or allowed roles. Will prompt for confirmation before deleting the prefix list."""
        await self.remove_entry(ctx, 'charsep list', 'charseps')

    async def codex_root(self, usechannel, ctx):
        serv_or_chan = 'channel' if usechannel else 'server'
        if await self.is_allowed(ctx):
            currentInfo = await self.bot.serverdata(ctx, 'codex')
            initial = "{author}: "
            initial += "The current codex for this "+serv_or_chan+" is {current}.\nYou can change it to any of the following:" if currentInfo else f"This {serv_or_chan} does not currently have a codex. \nIf you would like to change that, you have the following options:"
            systems = await self.bot.systemlist()
            systemsdict = {}
            for i in systems:
                if currentInfo:
                    if i['id'] == currentInfo:
                        currentInfo = i['display_name']
                systemsdict[i['display_name']] = i['id']
            async def setcodex(name):
                if name:
                    await self.bot.upsert_entry(ctx, {'codex':systemsdict[name]}, usechannel)
                    await ctx.send('Codex changed to '+name+'!')
                else:
                    await ctx.send('Codex change cancelled.')
            initial += '\n```'+'\n'.join(systemsdict)
            initial += f"```\nTo do so, type {ctx.prefix}<codex name>"
            await self.general_dialogue(ctx, initial, currentInfo, list(systemsdict.keys()), setcodex)
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def codex(self,ctx, *,arg=''):
        """Forking command for codex selection"""
        if ctx.prefix.startswith('<@') and ctx.prefix.find('>')>-1:
            await ctx.send(f"Please use {ctx.prefix}`channel_codex` or {ctx.prefix}`server_codex` to specify the context of your codex.")
        else:
            await ctx.send(f"Please use `{ctx.prefix}channel_codex` or `{ctx.prefix}server_codex` to specify the context of your codex.")

    @commands.command()
    async def server_codex(self,ctx):
        """Changes the server codex. Takes no arguments, instead giving a selection. Only available to admins or allowed roles."""
        await self.codex_root(False, ctx)

    @commands.command()
    async def channel_codex(self,ctx):
        """Changes the codex for the channel the command is used in. Takes no arguments, instead giving a selection. Only available to admins or allowed roles."""
        await self.codex_root(True, ctx)

    @commands.command()
    async def remove_channel_codex(self, ctx):
        """Deletes a channel's independent codex, if it exists. Only available to admins or allowed roles. Will prompt for confirmation before deleting the prefix list."""
        await self.remove_entry(ctx, 'codex', 'codex')

    async def freeze_root(self, usechannel, ctx):
        serv_or_chan = 'in this channel' if usechannel else 'on this server'
        if await self.is_allowed(ctx):
            currentInfo = await self.bot.serverdata(ctx, 'freeze_pages')
            async def toggle(test):
                if test.lower() == "yes":
                    await self.bot.upsert_entry(ctx, {'freeze_pages':not(currentInfo)}, usechannel)
                    await ctx.send(f'Paginator freezing set to {not(currentInfo)}!')
                else:
                    await ctx.send('Paginator freezing toggle cancelled.')
            initial = "{author}: "
            initial += f"Currently, Paginators will {'freeze' if currentInfo else 'delete'} themselves after timeout {serv_or_chan}.\n"
            initial += f"If you would like to toggle this, type {smartSurround(f'{ctx.prefix}Yes')}. Otherwise, type {smartSurround(f'{ctx.prefix}No')}."
            await self.general_dialogue(ctx, initial, bool(currentInfo), ynlist, toggle)
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def freeze_pages(self,ctx, *,arg=''):
        """Forking command for paginator freezing"""
        if ctx.prefix.startswith('<@') and ctx.prefix.find('>')>-1:
            await ctx.send(f"Please use {ctx.prefix}`channel_freeze_pages` or {ctx.prefix}`server_freeze_pages` to specify the context of your toggling.")
        else:
            await ctx.send(f"Please use `{ctx.prefix}channel_freeze_pages` or `{ctx.prefix}server_freeze_pages` to specify the context of your toggling.")

    @commands.command()
    async def server_freeze_pages(self,ctx):
        """Toggles pageinator freezing on the current server. Only available to admins or allowed roles."""
        await self.freeze_root(False, ctx)

    @commands.command()
    async def channel_freeze_pages(self,ctx):
        """Toggles pageinator freezing for the channel the command is used in. Only available to admins or allowed roles."""
        await self.freeze_root(True, ctx)

    @commands.command()
    async def remove_channel_freeze_pages(self, ctx):
        """Deletes a channel's independent pageinator freezing setting, if it exists. Only available to admins or allowed roles. Will prompt for confirmation before deleting the prefix list."""
        await self.remove_entry(ctx, 'paginator freezing', 'freeze_pages')

    async def inline_toggle_root(self, usechannel, ctx):
        serv_or_chan = 'in this channel' if usechannel else 'on this server'
        if await self.is_allowed(ctx):
            currentInfo = await self.bot.serverdata(ctx, 'inline')
            async def toggle(test):
                if test.lower() == "yes":
                    await self.bot.upsert_entry(ctx, {'inline':not(currentInfo)}, usechannel)
                    await ctx.send(f'Inline roll set to {not(currentInfo)}!')
                else:
                    await ctx.send('Inline roll toggle cancelled.')
            initial = "{author}: "
            initial += f"Currently, inline rolls are {'enabled' if currentInfo else 'disabled'} {serv_or_chan}.\n"
            initial += f"If you would like to toggle it, type {smartSurround(f'{ctx.prefix}Yes')}. Otherwise, type {smartSurround(f'{ctx.prefix}No')}."
            await self.general_dialogue(ctx, initial, bool(currentInfo), ynlist, toggle)
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def inline_toggle(self,ctx, *,arg=''):
        """Forking command for inline rolling"""
        if ctx.prefix.startswith('<@') and ctx.prefix.find('>')>-1:
            await ctx.send(f"Please use {ctx.prefix}`channel_inline_toggle` or {ctx.prefix}`server_inline_toggle` to specify the context of your toggling.")
        else:
            await ctx.send(f"Please use `{ctx.prefix}channel_inline_toggle` or `{ctx.prefix}server_inline_toggle` to specify the context of your toggling.")

    @commands.command()
    async def server_inline_toggle(self,ctx):
        """Toggles inline rolling on the current server. Only available to admins or allowed roles."""
        await self.inline_toggle_root(False, ctx)

    @commands.command()
    async def channel_inline_toggle(self,ctx):
        """Toggles inline rolling for the channel the command is used in. Only available to admins or allowed roles."""
        await self.inline_toggle_root(True, ctx)

    @commands.command()
    async def remove_channel_inline_toggle(self, ctx):
        """Deletes a channel's independent inline toggle, if it exists. Only available to admins or allowed roles. Will prompt for confirmation before deleting the prefix list."""
        await self.remove_entry(ctx, 'inline roll setting', 'inline')

    @commands.command()
    async def botmodroles(self, ctx, role=None):
        """Allows or disables a given role from altering this bot's settings on this server. Users with admin privileges will always be able to change the settings."""
        if await self.is_allowed(ctx):
            roles = await self.bot.serverdata(ctx, 'permissionroles')
            if not(roles):
                roles = []
            initial = "{author}: "
            if role:
                role = int(role[3:-1])
                rolename = discord.utils.get(ctx.guild.roles, id=role).name
                if role in roles:
                    roles.remove(role)
                    async def removerole(test):
                        if test.lower() == 'yes':
                            await self.bot.upsert_entry(ctx, {'permissionroles': roles}, False)
                            await ctx.send('The role "' + rolename + "\" was removed from the list of roles allowed to change the bot's settings.")
                        else:
                            await ctx.send('Permissions change cancelled.')
                    initial += "Would you like to remove bot-changing permissions from the role \"{current}\"?\nType "+ctx.prefix+"Yes or "+ctx.prefix+"No to confirm."
                    await self.general_dialogue(ctx, initial, rolename, ynlist, removerole)
                else:
                    roles.append(role)
                    async def setrole(test):
                        if test.lower() == 'yes':
                            await self.bot.upsert_entry(ctx, {'permissionroles': roles}, False)
                            await ctx.send('The role "' + rolename + "\" was added to the list of roles allowed to change this bot's settings.")
                        else:
                            await ctx.send('Permissions change cancelled.')
                    initial += "Would you like to add bot-changing permissions to the role \"{current}\"?\nType "+ctx.prefix+"Yes or "+ctx.prefix+"No to confirm."
                    await self.general_dialogue(ctx, initial, rolename, ynlist, setrole)
            else:
                l = [discord.utils.get(ctx.guild.roles, id=i).name for i in roles]
                if len(l) == 0:
                    initial += "Currently, only admins are allowed to alter this bot's settings.\n"
                    initial += "If you want to give a particular role on your server permission to do so, issue this command followed by a ping of that role."
                else:
                    initial += "The current roles with permission to alter this bot's settings are:\n"
                    if len(l) == 1:
                        r = l[0]
                    elif len(l) == 2:
                        r = l[0] + ' and ' + l[1]
                    else:
                        r = ', '.join(l[:-1]) + ', and ' +l[-1]
                    r = '```'+r+'```'
                    initial += r+'\nIf you want to add another role, or remove one of the above roles, issue this command followed by a ping of that role.'
                await ctx.send(initial.format(author=ctx.author.display_name))
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')


def setup(bot):
    bot.add_cog(Settings(bot))
