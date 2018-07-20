import datetime
import sys
args = sys.argv
if len(args) > 1:
    if 'print' in args:
        def logexe(e):
            raise e
        logfun = print
    else:
        import logging
        logging.basicConfig(filename='logs/bot/bot '+str(datetime.datetime.now())+'.log', format='%(asctime)s %(message)s')
        logger = logging.getLogger('Logger')
        print(args, type(args), args[1])
        logger.setLevel(args[1])
        logfun = logger.info
        logexe = logger.exception
else:
    import logging
    logging.basicConfig(filename='logs/bot/bot '+str(datetime.datetime.now())+'.log', format='%(asctime)s %(message)s')
    logger = logging.getLogger('Logger')
    logger.setLevel('INFO')
    logexe = logger.exception
    logfun = logger.info
logfun('------------------------------------------------------------------------------------')
logfun('Boot Start')
logfun('------------------------------------------------------------------------------------')
import json 
logfun('json imported')
import asyncio
logfun('asyncio imported')
import asyncpg
logfun('asyncpg imported')

import discord
logfun('discord imported')
from discord.ext import commands
logfun('commands imported')

import serverfetcher
# Get the prefixes for the bot
async def command_prefix(bot, message):
    if message.guild:
        extras = await bot.serverfetcher.prefixes_for(message, bot.user.id)
    else:
        extras = ['', '<@'+str(bot.user.id)+'> ', '<@'+str(bot.user.id)+'>']
    return commands.when_mentioned_or(*extras)(bot, message)

# All cogs that will be loaded on bots startup
startup_extensions = [
    'cogs.character',
    'cogs.general',
    'cogs.ref',
    'cogs.roll',
    'cogs.server_settings',
    'cogs.init'
]

class RPBot(commands.Bot):
    def __init__(self, sf):
        super().__init__(command_prefix=command_prefix, case_insensitive=True, pm_help=None)
        with open("botsettings.json") as data_file:
            self.settings = json.load(data_file)
        self.waiting = {}
        self.serverfetcher = sf

        self.logger = logfun 

        self.botdataserver = {}
        self.botdataserver['credentials'] = {"user": self.settings['sql'][0], "password": self.settings['sql'][2], "database": self.settings['botDataServer'], "host": self.settings['sql'][1]}
        self.botdataserver['commands'] = {}
        self.botdataserver['commands']['increment_command'] = 'INSERT INTO command_usage (name, uses) VALUES ($1, 1) ON CONFLICT (name) DO UPDATE SET uses = command_usage.uses + 1;'
        self.botdataserver['commands']['upsert'] = lambda x: 'INSERT INTO '+x+ '(id) VALUES ($1) ON CONFLICT DO NOTHING;'  #unique_guilds, unique_users

        for extension in startup_extensions:
            try:
                self.load_extension(extension)
            except Exception as e:
                exc = '{}: {}'.format(type(e).__name__, e)
                self.logger('Failed to load extension {}\n{}'.format(extension, exc))

    # Print bot information, update status and set uptime when bot is ready
    async def on_ready(self):
        self.logger('Username: ' + str(self.user.name))
        self.logger('Client ID: ' + str(self.user.id))
        self.check_for_server = self.serverfetcher.check_for_server        
        self.serverdata = self.serverfetcher.serverdata
        self.systemlist = self.serverfetcher.systemlist
        self.upsert_entry = self.serverfetcher.upsert_entry
        self.botdataserver['pool'] = await asyncpg.create_pool(**self.botdataserver['credentials'])
        activity = discord.Activity(name='"<@bot_ping> init" to help newcomers.', type = discord.ActivityType.listening)
        await self.change_presence(activity=activity)

    async def on_command_error(self, ctx, e):
        logexe(e)
        return

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
                if ctx.me.permissions_in(ctx.channel).send_messages: 
                    await self.inline_roller(ctx)

    async def on_command_completion(self, ctx):
        async with self.botdataserver['pool'].acquire() as conn:
            if ctx.guild:
                await conn.execute(self.botdataserver['commands']['upsert']('unique_guilds'), ctx.guild.id)
            await conn.execute(self.botdataserver['commands']['upsert']('unique_users') , ctx.author.id)
            await conn.execute(self.botdataserver['commands']['increment_command']      , ctx.command.name)

    @staticmethod
    async def smartSend(ctx,prefix,message,begin='', end=''):
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
            pos = message[:m].rfind('\n')
            if pos==-1:
                await ctx.send(message[:m]+endStr)
                message = message[m:]
            else:
                await ctx.send(beginStr+message[:pos]+endStr)
                message = message[pos+1:]
            if not(message.startswith(begin)):
                message = begin + message
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
        --------s
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
    logfun('starting fetcher')
    sf = serverfetcher.ServerFetcher()
    logfun('making bot')
    bot = RPBot(sf)
    try:
        logfun('connecting fetcher')
        await sf.init_pool(bot.settings)
        logfun('starting bot')
        await bot.start(bot.settings['token'])
    except KeyboardInterrupt:
        await bot.refserver.close()
        await sf.pool.close()
        await bot.logout()

loop = asyncio.get_event_loop()
loop.run_until_complete(run())
