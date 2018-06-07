import json
import asyncio

import discord
from discord.ext import commands

import serverfetcher
# Get the prefixes for the bot
async def command_prefix(bot, message):
    if message.guild:
        extras = await serverfetcher.prefixes_for(message.guild.id, bot.user.id)
    else:
        extras = ['', '<@'+str(bot.user.id)+'> ', '<@'+str(bot.user.id)+'>']
    return commands.when_mentioned_or(*extras)(bot, message)

# All cogs that will be loaded on bots startup
startup_extensions = [
    'cogs.character',
    'cogs.general',
    'cogs.ref',
    'cogs.roll',
    'cogs.server_settings'
]

class RPBot(commands.Bot):
    def __init__(self):
        super().__init__(command_prefix=command_prefix, case_insensitive=True, pm_help=None)
        self.serverdata = serverfetcher.serverdata
        self.systemlist = serverfetcher.systemlist
        self.upsert_entry = serverfetcher.upsert_entry
        with open("botsettings.json") as data_file:
            self.settings = json.load(data_file)
        self.waiting = {}

        for extension in startup_extensions:
            try:
                self.load_extension(extension)
            except Exception as e:
                exc = '{}: {}'.format(type(e).__name__, e)
                print('Failed to load extension {}\n{}'.format(extension, exc))

    async def redefine(self):
        self.server_settings = self.serverdata()

    # Print bot information, update status and set uptime when bot is ready
    async def on_ready(self):
        print('Username: ' + str(self.user.name))
        print('Client ID: ' + str(self.user.id))
        await self.redefine()

    # Prevent bot from replying to other bots
    async def on_message(self, message):
        if not message.author.bot:    
            #Response commands. Format is (prefix, funlist yes, funlist no, funlist invalid)
            #funlist is list of two-tuples, ((function, type), arguments), where type is either 'inline' or 'async', and arguments are themselves a tuple
            if message.channel.id in self.waiting and message.author.id in self.waiting[message.channel.id] and message.content.startswith(self.waiting[message.channel.id][message.author.id]['prefix']):
                entry = self.waiting[message.channel.id].pop(message.author.id)
                if message.content.strip()[len(entry['prefix']):] in entry['options']:
                    option = message.content.strip()[len(entry['prefix']):]
                else:
                    option = None
                await entry['function'](option)
                if not(len(self.waiting[message.channel.id])):
                    del self.waiting[message.channel.id]
            else:
                ctx = await self.get_context(message)
                await self.invoke(ctx)
                #await self.inline_roller(ctx)

    @staticmethod
    async def smartSend(ctx,prefix,message,begin='', end=''):
        #print('here',prefix,message)
        await ctx.send(prefix)
        if begin and not(end):
            end = begin
        if not(message.startswith(begin)):
            message = begin + message
        while len(message)>1994:
            m = 2000
            beginStr = endStr = ''
            if not(message[:m].endswith(endStr)):
                m -= len(endStr)
                endStr = end
            print(m)
            pos = message[:m].rfind('\n')
            print(pos)
            if pos==-1:
                await ctx.send(message[:m]+endStr)
                message = message[m:]
            else:
                await ctx.send(beginStr+message[:pos]+endStr)
                message = message[pos+1:]
            print(message)
            if not(message.startswith(begin)):
                message = begin + message
            print(message)
            message = message.lstrip()
        await ctx.send(message+end)

    #Overwriting the default ger_prefix coroutine to allow for blank prefixes in DM Channels
    @asyncio.coroutine
    def get_prefix(self, message):
        """|coro|
        Retrieves the prefix the bot is listening to
        with the message as a context.
        Parameters
        -----------
        message: :class:`discord.Message`
            The message context to get the prefix of.
        Raises
        --------
        :exc:`.ClientException`
            The prefix was invalid. This could be if the prefix
            function returned None, the prefix list returned no
            elements that aren't None, or the prefix string is
            empty.
        Returns
        --------
        Union[List[str], str]
            A list of prefixes or a single prefix that the bot is
            listening for.
        """
        prefix = ret = self.command_prefix
        if callable(prefix):
            ret = prefix(self, message)
            if asyncio.iscoroutine(ret):
                ret = yield from ret

        if isinstance(ret, (list, tuple)):
            if not(isinstance(message.channel, discord.DMChannel)):
                ret = [p for p in ret if p]

        if not ret:
            raise discord.ClientException('invalid prefix (could be an empty string, empty list, or None)')

        return ret

async def run():
    bot = RPBot()
    try:
        await serverfetcher.init_pool(bot.settings)
        await bot.start(bot.settings['token'])
    except KeyboardInterrupt:
        await bot.refserver.close()
        await serverfetcher.serverDB.close()
        await bot.logout()

loop = asyncio.get_event_loop()
loop.run_until_complete(run())