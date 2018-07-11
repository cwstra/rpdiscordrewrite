import discord
from discord.ext import commands

import json
import asyncio

class Settings:
    def __init__(self, bot):
        self.bot = bot

    async def is_allowed(self, ctx):
        permObject = None
        allowedroles = await self.bot.serverdata(ctx.guild.id, 'permissionroles')
        test = ctx.author.permissions_in(ctx.channel) or (ctx.author.roles and any(i.id in allowedroles for i in ctx.author.roles))
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

    async def symbol_list_change(self, ctx, arg, singtype, plurtype, default):
        currentInfo = await self.bot.serverdata(ctx.guild.id, plurtype)
        if currentInfo == None:
            currentInfo = []
        initial = "{author}: "
        emptyTest = False
        options = []
        if not(currentInfo):
            initial += "This server's current "
            if len(default) == 1:
                initial += singtype +" is `"+ default[0] +"`. "
            else:
                multi = ',` and `'.join(', '.join(default).rsplit(', ',1)) if len(default)>2 else '` and `'.join(default)
                initial += plurtype +" are `"+ multi +"`. "
            emptyTest = True
        elif len(currentInfo) == 1:
            initial += "This server's current "+ singtype +" is `" + currentInfo[0] + "`. "
        else:
            multi = ',` and `'.join(', '.join(currentInfo).rsplit(', ',1)) if len(currentInfo)>2 else '` and `'.join(currentInfo)
            initial += "This server's current "+ plurtype +" are `" + multi + "`. "
        posslist = [i for i in currentInfo]
        if len(arg)==0:
            pre = ('@'+self.bot.user.display_name if ctx.prefix[2:-1]==str(self.bot.user.id) else ctx.prefix)
            pre = ('@'+self.bot.user.display_name+' ' if ctx.prefix[2:-2]==str(self.bot.user.id) else ctx.prefix)
            initial += "If you would like to add or remove a "+ singtype +", type `"+ pre + singtype +" <new "+ singtype +">`."
            await ctx.send(initial.format(author=ctx.author.display_name))
            return
        elif arg==default and (len(currentInfo)==0 or (len(currentInfo)==1 and currentInfo[0]==default)):
            initial += "You have to have _a_ "+ singtype +". To remove `"+ default +"` from your "+ singtype +" list, you'll have to add another "+ singtype +" first."
            await ctx.send(initial.format(author=ctx.author.display_name))
            return
        elif arg in currentInfo:
            initial += "Would you like to remove `"+ arg +"` from your "+ singtype +" list? Type `"+ ctx.prefix +"Yes` or `"+ ctx.prefix +"No` to confirm."
            for i, v in enumerate(currentInfo):
                if v == arg:
                    posslist.pop(i)
                    break
        elif len(currentInfo)>10:
            initial += "A given server can only have 10 "+plurtype+". You'll have to remove some "+plurtype+" before you can add a new one."
            await ctx.send(initial.format(author=ctx.author.display_name))
            return
        else:
            initial += "Would you like to add `"+ arg +"` to your "+ singtype +" list? Type `"+ ctx.prefix +"Yes` or `"+ ctx.prefix +"No` to confirm."
            posslist.append(arg)
            if len(currentInfo) == 0:
                posslist += default
        posslist.sort(key = lambda x:len(x))
        async def change(name):
            if type(name) == str and name.lower() == 'yes':
                await self.bot.upsert_entry(ctx.guild.id, {plurtype:posslist})
                await ctx.send(singtype.title()+' list changed!')
            else:
                await ctx.send(singtype.title()+' list change cancelled.')
        await self.general_dialogue(ctx, initial, currentInfo, ['YES', 'YEs', 'YeS', 'yES', 'yeS', 'yEs', 'Yes', 'yes', 'NO', 'No', 'nO', 'no'], change)

    @commands.command()
    async def prefix(self,ctx,*,arg=''):
        """Changes the server prefix. Only available to admins or allowed roles. Will prompt for confirmation before changing the prefix."""
        if await self.is_allowed(ctx):
            await self.symbol_list_change(ctx, arg, 'prefix', 'prefixes', ['<@'+str(self.bot.user.id)+'>', '<@'+str(self.bot.user.id)+'>'])
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def charsign(self,ctx,*,arg=''):
        """Changes the server charsign. Only available to admins or allowed roles. Will prompt for confirmation before changing the charsign."""
        if await self.is_allowed(ctx):
            if '@' in arg:
                await ctx.send('Sorry, '+ctx.author.display_name+", but charsigns can't contain @.")
                return
            await self.symbol_list_change(ctx, arg, 'charsign', 'charsigns', ['$'])
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')
    
    @commands.command()
    async def charsep(self,ctx,*,arg=''):
        """Changes the server charsep. Only available to admins or allowed roles. Will prompt for confirmation before changing the charsign."""
        if await self.is_allowed(ctx):
            if '@' in arg:
                await ctx.send('Sorry, '+ctx.author.display_name+", but charseps can't contain @.")
                return
            await self.symbol_list_change(ctx, arg, 'charsep', 'charseps', [':'])
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def codex(self,ctx):
        """Changes the server codex. Takes no arguments, instead giving a selection. Only available to admins or allowed roles."""
        if await self.is_allowed(ctx):
            currentInfo = await self.bot.serverdata(ctx.guild.id, 'codex')
            initial = "{author}:"
            initial += "The current codex for this server is {current}.\nYou can change it to any of the following:" if currentInfo else "This server does not currently have a codex. \nIf you would like to change that, you have the following options:"
            systems = await self.bot.systemlist()
            systemsdict = {}
            for i in systems:
                if currentInfo:
                    if i['id'] == currentInfo:
                        currentInfo = i['display_name']
                systemsdict[i['display_name']] = i['id']
            async def setcodex(name):
                if name:
                    await self.bot.upsert_entry(ctx.guild.id, {'codex':systemsdict[name]})
                    await ctx.send('Codex changed to '+name+'!')
                else:
                    await ctx.send('Codex change cancelled.')
            initial += '\n```'+'\n'.join(systemsdict)
            initial += "```\nTo do so, type "+ctx.prefix+'<codex name>'
            await self.general_dialogue(ctx, initial, currentInfo, list(systemsdict.keys()), setcodex)
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def inline_toggle(self,ctx):
        """Toggles inline rolling. Only available to admins or allowed roles."""
        if await self.is_allowed(ctx):
            currentInfo = await self.bot.serverdata(ctx.guild.id, 'inline')
            async def toggle(test):
                if test == "Yes":
                    await self.bot.upsert_entry(ctx.guild.id, {'inline':not(currentInfo)})
                    await ctx.send('Inline roll set to '+str(not(currentInfo))+'!')
                else:
                    await ctx.send('Inline roll toggle cancelled.')
            initial = "{author}:"
            initial += "Currently, inline rolls are "+("enabled" if currentInfo else "disabled")+" on this server.\n"
            initial += "If you would like to toggle it, type `"+ctx.prefix+"Yes`. Otherwise, type `"+ctx.prefix+"No`."
            await self.general_dialogue(ctx, initial, bool(currentInfo), ["Yes", "No"], toggle)
        else:
            await ctx.send('To use this command, you have to either have admin permissions on this server, or have a role permitted to modify this bot.')

    @commands.command()
    async def botmodroles(self, ctx, role=None):
        """Allows or disables a given role from altering this bot's settings on this server. Users with admin privileges will always be able to change the settings."""
        if await self.is_allowed(ctx):
            roles = await self.bot.serverdata(ctx.guild.id, 'permissionroles')
            if not(roles):
                roles = []
            initial = "{author}: "
            if role:
                role = int(role[3:-1])
                rolename = discord.utils.get(ctx.guild.roles, id=role).name
                if role in roles:
                    roles.remove(role)
                    async def removerole(test):
                        if test == 'Yes':
                            await self.bot.upsert_entry(ctx.guild.id, {'permissionroles': roles})
                            await ctx.send('The role "' + rolename + "\" was removed from the list of roles allowed to change the bot's settings.")
                        else:
                            await ctx.send('Permissions change cancelled.')
                    initial += "Would you like to remove bot-changing permissions from the role \"{current}\"?\nType "+ctx.prefix+"Yes or "+ctx.prefix+"No to confirm."
                    await self.general_dialogue(ctx, initial, rolename, ['Yes', 'No'], removerole)
                else:
                    roles.append(role)
                    async def setrole(test):
                        if test == 'Yes':
                            await self.bot.upsert_entry(ctx.guild.id, {'permissionroles': roles})
                            await ctx.send('The role "' + rolename + "\" was added to the list of roles allowed to change this bot's settings.")
                        else:
                            await ctx.send('Permissions change cancelled.')
                    initial += "Would you like to add bot-changing permissions to the role \"{current}\"?\nType "+ctx.prefix+"Yes or "+ctx.prefix+"No to confirm."
                    await self.general_dialogue(ctx, initial, rolename, ['Yes', 'No'], setrole)
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
