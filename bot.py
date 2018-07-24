#Set up file logging
import datetime
import sys
args = sys.argv
#If we're to print the output
if 'print' in args:
    #Just raise exceptions
    def logexe(e):
        raise e
    #Print out all logging
    logfun = print
#Otherwise, we're logging the output:
else:
    import logging
    #Log to logs/bot <time>.log
    logging.basicConfig(filename='logs/bot/bot '+str(datetime.datetime.now())+'.log', format='%(asctime)s %(message)s')
    #Get a logger
    logger = logging.getLogger('Logger')
    #Get the level from the first argument, if it exists. (args[0] is always the filename)
    if len(args) > 1:
        logger.setLevel(args[1])
    else:
        logger.setLevel('INFO')
    #Set the general logging functions
    logexe = logger.exception
    logfun = logger.info
#Start the imports
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
from cogs.utils.SimplePaginator import SimplePaginator

import serverfetcher
# Get the prefixes for the bot
async def command_prefix(bot, message):
    #If there's a guild:
    if message.guild:
        #Get its prefixes
        extras = await bot.serverfetcher.prefixes_for(message, bot.user.id)
    #Otherwise, we're in a PM
    else:
        #Ping the bot, or have no prefix.
        extras = ['', '<@'+str(bot.user.id)+'> ', '<@'+str(bot.user.id)+'>']
    #Pass it along
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

def toembed(d, printFun):
    fields = d.pop('fields', [])
    image = d.pop('image', None)
    printFun(image)
    em = discord.Embed(**d)
    for i in fields:
        em.add_field(**i)
    if image:
        em.set_image(url=image)
    return em

class RPBot(commands.Bot):
    def __init__(self, sf):
        super().__init__(command_prefix=command_prefix, case_insensitive=True, pm_help=None)
        #Load the settings json
        with open("botsettings.json") as data_file:
            self.settings = json.load(data_file)
        #Set up the waiting dictionary
        self.waiting = {}
        #Provide serverfetcher to the cogs
        self.serverfetcher = sf

        #Provide logfun to the cogs
        self.logger = logfun 

        #Set up the bot stat logging server
        self.botdataserver = {}
        self.botdataserver['credentials'] = {"user": self.settings['sql'][0], "password": self.settings['sql'][2], "database": self.settings['botDataServer'], "host": self.settings['sql'][1]}
        self.botdataserver['commands'] = {}
        self.botdataserver['commands']['increment_command'] = 'INSERT INTO command_usage (name, uses) VALUES ($1, 1) ON CONFLICT (name) DO UPDATE SET uses = command_usage.uses + 1;'
        self.botdataserver['commands']['upsert'] = lambda x: 'INSERT INTO '+x+ '(id) VALUES ($1) ON CONFLICT DO NOTHING;'  #unique_guilds, unique_users

        #Initializing some objects to prevent errors should their cogs fail to load:
        self.inline_roller = lambda x: None
        self.refserver = None

        #Load extensions
        for extension in startup_extensions:
            try:
                self.load_extension(extension)
            except Exception as e:
                exc = '{}: {}'.format(type(e).__name__, e)
                self.logger('Failed to load extension {}\n{}'.format(extension, exc))

    # Print bot information, update status and set uptime when bot is ready
    async def on_ready(self):
        #Log the login
        self.logger('Username: ' + str(self.user.name))
        self.logger('Client ID: ' + str(self.user.id))
        #Post-serverfetcher set up functions
        self.serverdata = self.serverfetcher.serverdata
        self.systemlist = self.serverfetcher.systemlist
        self.upsert_entry = self.serverfetcher.upsert_entry
        #Get pool for the bot stat server
        self.botdataserver['pool'] = await asyncpg.create_pool(**self.botdataserver['credentials'])
        #Set the current activity
        activity = discord.Activity(name='"<@bot_ping> init" to help newcomers.', type = discord.ActivityType.listening)
        await self.change_presence(activity=activity)

    async def on_command_error(self, ctx, e):
        logexe(e)
        return

    # Prevent bot from replying to other bots
    async def on_message(self, message):
        if not message.author.bot:
            #Response commands. Format is (prefix, funlist yes, funlist no, funlist invalid)
            #Each entry is a dictionary of the form: {'prefix':ctx.prefix, 'function':generalFun, 'options':optionsList}
            #'prefix' is, well, the prefix
            #'function' is the function carried out by the entry
            #'options' is a list of valid arguments to the function

            #If the message's channel has a waiting entry, and the author is in that entry, and the message starts with the entries prefix:
            if message.channel.id in self.waiting and message.author.id in self.waiting[message.channel.id] and message.content.startswith(self.waiting[message.channel.id][message.author.id]['prefix']):
                #Pop out the author's entry
                entry = self.waiting[message.channel.id].pop(message.author.id)
                #If the contents of the message match an option
                if message.content.strip()[len(entry['prefix']):] in entry['options']:
                    #Set option to it
                    option = message.content.strip()[len(entry['prefix']):]
                #Otherwise
                else:
                    #Set option to None
                    option = None
                #Feed option to function
                await entry['function'](option)
                #If the channel's entry is empty
                if not(len(self.waiting[message.channel.id])):
                    #Delete it
                    del self.waiting[message.channel.id]
            #Otherwise
            else:
                #Get the context of the message
                ctx = await self.get_context(message)
                #Invoke the context
                await self.invoke(ctx)
                #If the bot can send messages in the channel
                if ctx.me.permissions_in(ctx.channel).send_messages:
                    #Invoke the inline roller
                    await self.inline_roller(ctx)

    #After a command successfully executes
    async def on_command_completion(self, ctx):
        #Get a connection from the stat server
        async with self.botdataserver['pool'].acquire() as conn:
            #If the message isn't a PM
            if ctx.guild:
                #Log the guild
                await conn.execute(self.botdataserver['commands']['upsert']('unique_guilds'), ctx.guild.id)
            #Log the user
            await conn.execute(self.botdataserver['commands']['upsert']('unique_users') , ctx.author.id)
            #Log the command
            await conn.execute(self.botdataserver['commands']['increment_command']      , ctx.command.name)

    @staticmethod
    async def smartSend(ctx,initial,message,begin='', end=''):
        """Static method to send messages that might be longer than the limit.
        Arguments:
            ctx: context to send to
            initial: An initial message to send to context
            message: The message that might go over the maximum
            begin: A beginning prefix to each individual text block message is split into.
            end: An ending prefix to the same. If non-existent while begin exists, it's set to begin"""
        #Send the initial message
        await ctx.send(initial)
        #If end is empty and begin is not
        if begin and not(end):
            #Set end to begin
            end = begin
        #The longest we can allow a message to be is 2000 - the combined length of the bookends
        maxlength = 2000-(len(begin)+len(end))
        #While message is longer than allowed
        while len(message)>maxlength:
            #Find the last newline in the allowed length of message
            pos = message[:maxlength].rfind('\n')
            #If we couldn't find one:
            if pos==-1:
                #Cut it off at the maximumlength
                await ctx.send(begin + message[:maxlength] + end)
                message = message[maxlength:]
            #Otherwise
            else:
                #Cut it off at the newline
                await ctx.send(begin+message[:pos]+end)
                #And continue after the newline
                message = message[pos+1:]
        #Send what's left
        await ctx.send(begin+message+end)

    @staticmethod
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
            embeds = [toembed(i, printFun) for i in embeds]
            if len(embeds) == 1:
                await ctx.send(None, embed = embeds[0])
            else:
                await SimplePaginator(extras=embeds).paginate(ctx) 
        else:
            if 'image' in info and len(info['image']) == 1:
                info['image'] = info['image'][0]
            await ctx.send(None, embed = toembed(info, printFun))

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

#Initializing function
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
        if bot.refserver:
            await bot.refserver.close()
        await sf.pool.close()
        await bot.logout()

#Run everything
loop = asyncio.get_event_loop()
loop.run_until_complete(run())
