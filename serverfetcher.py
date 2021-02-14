import ujson as json
import asyncpg

class ServerFetcher:
    def __init__(self):
        #Initialize _all_ our commands
        self.commands = {}
        self.commands['channels'] = {}
        self.commands['channels']['get_specific_entry'] = lambda x: "SELECT "+x+" FROM channels WHERE server_id = $1 AND channel_id = $2;"
        self.commands['channels']['get_specific_row'] = "SELECT * FROM channels WHERE server_id = $1 AND channel_id = $2;"
        self.commands['channels']['get_all_of_entry_and_id'] = lambda x: 'SELECT (server_id, channel_id, '+x+') FROM channels;'
        self.commands['channels']['get_all'] = 'SELECT * FROM channels;'
        self.commands['channels']['delete_one_row'] = 'DELETE FROM channels WHERE server_id = $1 AND channel_id = $2;'
        self.commands['channels']['upsert'] = lambda x: 'INSERT INTO channels (server_id, channel_id, '+x+') VALUES ($1, $2, $3) ON CONFLICT (server_id, channel_id) DO UPDATE SET '+x+'= $3;'
        self.commands['channels']['clear_empty_rows'] = "DELETE FROM channels WHERE ((codex IS NULL or codex = '') and (charsigns IS NULL OR charsigns = '{}') and (prefixes IS NULL or prefixes = '{}') and (inline IS NULL or NOT inline));"
        self.commands['servers'] = {}
        self.commands['servers']['get_specific_entry'] = lambda x: "SELECT "+x+" FROM servers WHERE id = $1;"
        self.commands['servers']['get_specific_row'] = "SELECT * FROM servers WHERE id = $1;"
        self.commands['servers']['get_all_of_entry_and_id'] = lambda x: 'SELECT (id,'+x+') FROM servers;'
        self.commands['servers']['get_all'] = 'SELECT * FROM servers;'
        self.commands['servers']['upsert'] = lambda x: 'INSERT INTO servers (id, '+x+') VALUES ($1, $2) ON CONFLICT (id) DO UPDATE SET '+x+'=$2;'
        self.commands['servers']['clear_empty_rows'] = "DELETE FROM servers WHERE ((codex IS NULL or codex = '') and (permissionroles IS NULL or permissionroles = '{}') and (charsigns IS NULL or charsigns = '{}') and (prefixes IS NULL or prefixes = '{}') and (inline IS NULL or NOT inline));"
        self.commands['codex_list'] = {}
        self.commands['codex_list']['get_name'] = 'SELECT display_name FROM codex_list WHERE id = $1;'
        self.commands['codex_list']['get_all'] = 'SELECT * FROM codex_list;'

    async def init_pool(self, settings):
        """Pool initialization function"""
        self.credentials = {"user": settings['sql'][0], "password": settings['sql'][2], "database": settings['serverSettingsDB'], "host": settings['sql'][1]}
        self.pool = await asyncpg.create_pool(**self.credentials)

    async def checkchannel(self, ctx, entry):
        """Checks if the channel has its own entry, separate from the server"""
        guild_id = ctx.guild.id
        channel_id = ctx.channel.id
        async with self.pool.acquire() as conn:
            if guild_id:
                res = await conn.fetchrow(self.commands['channels']['get_specific_entry'](entry), guild_id, channel_id)
                if res and res[entry]!=None:
                    return res[entry]
            return False

    async def serverdata(self, ctx=None, entry=None, force_server=False, return_location=False):
        """Get data from the serverSettings server"""
        guild_id = None
        #If we have a context and that context has a guild
        if ctx and ctx.guild:
            #Set guild_id and channel_id
            guild_id = ctx.guild.id
            channel_id = ctx.channel.id
        #Acquire a connection
        async with self.pool.acquire() as conn:
            #Set res to None initially
            res = None
            location = None
            if guild_id and entry:
                if channel_id and not force_server and entry != 'permissionroles':
                    #Get data from the channel's entry, if it exists
                    res = await conn.fetchrow(self.commands['channels']['get_specific_entry'](entry), guild_id, channel_id)
                    location = "channel"

                #If there isn't a channel or the channel doesn't have an entry
                if res == None or res[entry] == None:
                    #Get data from the server's entry, if it exists
                    res = await conn.fetchrow(self.commands['servers']['get_specific_entry'](entry), guild_id)
                    location = "server"

                #If we found something
                if res:
                    #Unwrap it
                    res = res[entry]
                if not(res):
                    location = None
            #Return what we found
            return [res, location] if return_location else res

    async def systemlist(self, name = None):
        """Gets a list of all codexes currently available"""
        #Acquire a connection
        async with self.pool.acquire() as conn:
            #If there's a specific entry we're getting the name of:
            if name:
                #Get it
                res = await conn.fetchval(self.commands['codex_list']['get_name'], name)
                return res
            #Otherwise, just get a list of them all
            res = await conn.fetch(self.commands['codex_list']['get_all'])
            return res

    async def upsert_entry(self, ctx, entryDict=None, usechannel = True):
        """Inserts new entries, or updates old entries, as relevant"""
        #Acquire a connection
        async with self.pool.acquire() as conn:
            #If we have entries to add
            if entryDict:
                #iterate over the entry dictionary
                for i, j in entryDict.items():
                    #If we are updating a server, or the key is permissionroles
                    if not(usechannel) or i == 'permissionroles':
                        #Upsert to the server's entry
                        await conn.execute(self.commands['servers']['upsert'](i), ctx.guild.id, j)
                    #Otherwise
                    else:
                        #Upsert to the channel's entry
                        await conn.execute(self.commands['channels']['upsert'](i), ctx.guild.id, ctx.channel.id, j)
                #Clear out any empty rows
                await conn.execute(self.commands['servers']['clear_empty_rows'])
                await conn.execute(self.commands['channels']['clear_empty_rows'])

    async def remove_channel_entry(self, ctx, entry):
        """Drops a channel from the list, to force it to follow the server's data again"""
        #Acquire a connection
        async with self.pool.acquire() as conn:
            #Delete the channel's entry
            await conn.execute(self.commands['channels']['delete_one_row'], ctx.guild.id, ctx.channel.id)

    async def prefixes_for(self, ctx, bot_id):
        """Returns the server's prefixes"""
        #Get the prefix entry for the server
        res = await self.serverdata(ctx, 'prefixes')
        #If there's no entry
        if res == None:
            #Return the default
            return ['<@'+str(bot_id)+'> ', '<@'+str(bot_id)+'>']
        #Otherwise return the entry
        return res