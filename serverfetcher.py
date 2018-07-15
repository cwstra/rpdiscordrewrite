import ujson as json
import asyncpg

class ServerFetcher:
    def __init__(self):
        self.commands = {}
        self.commands['channels'] = {}
        self.commands['channels']['get_specific_entry'] = lambda x: "SELECT "+x+" FROM channels WHERE server_id = $1 AND channel_id = $2;"
        self.commands['channels']['get_specific_row'] = "SELECT * FROM channels WHERE server_id = $1 AND channel_id = $2;"
        self.commands['channels']['get_all_of_entry_and_id'] = lambda x: 'SELECT (server_id, channel_id, '+x+') FROM channels;'
        self.commands['channels']['get_all'] = 'SELECT * FROM channels;'
        self.commands['channels']['nullify'] = lambda x: 'UPDATE channels SET '+x+'=NULL WHERE server_id = $1 AND channel_id = $2;'
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
        self.credentials = {"user": settings['sql'][0], "password": settings['sql'][2], "database": settings['serverSettingsDB'], "host": settings['sql'][1]}
        self.pool = await asyncpg.create_pool(**self.credentials)

    async def checkchannel(self, ctx, entry):
        guild_id = ctx.guild.id
        channel_id = ctx.channel.id
        async with self.pool.acquire() as conn:
            if guild_id:
                res = await conn.fetchrow(self.commands['channels']['get_specific_entry'](entry), guild_id, channel_id)
                if res and res[entry]!=None:
                    return res[entry] 
            return False

    async def serverdata(self, ctx=None, entry=None):
        guild_id = None
        if ctx and ctx.guild:
            guild_id = ctx.guild.id
            channel_id = ctx.channel.id
        async with self.pool.acquire() as conn:
            res = None
            if guild_id and entry:
                if channel_id and entry != 'permissionroles':
                    res = await conn.fetchrow(self.commands['channels']['get_specific_entry'](entry), guild_id, channel_id)
                if res == None or res[entry] == None:
                    res = await conn.fetchrow(self.commands['servers']['get_specific_entry'](entry), guild_id)
                if res:
                    res = res[entry]
            return res

    async def systemlist(self, name = None):
        async with self.pool.acquire() as conn:
            if name:
                res = await conn.fetchval(self.commands['codex_list']['get_name'], name)
                return res
            res = await conn.fetch(self.commands['codex_list']['get_all'])
            return res

    async def upsert_entry(self, ctx, entryDict=None, usechannel = True):
        async with self.pool.acquire() as conn:
            if entryDict:
                for i, j in entryDict.items():
                    if not(usechannel) or i == 'permissionroles':
                        await conn.execute(self.commands['servers']['upsert'](i), ctx.guild.id, j)
                    else:
                        await conn.execute(self.commands['channels']['upsert'](i), ctx.guild.id, ctx.channel.id, j) 
                await conn.execute(self.commands['servers']['clear_empty_rows'])
                await conn.execute(self.commands['channels']['clear_empty_rows'])

    async def remove_channel_entry(self, ctx, entry):
        async with self.pool.acquire() as conn:
            await conn.execute(self.commands['channels']['nullify'](entry), ctx.guild.id, ctx.channel.id)
            await conn.execute(self.commands['channels']['clear_empty_rows'])

    async def check_for_server(self, ctx):
        async with self.pool.acquire() as conn:
            return bool(await conn.execute(self.commands['get_specific_entry']('TRUE'), ctx.guild.id))

    # Get all the prefixes the guild can use
    async def prefixes_for(self, ctx, bot_id):
        res = await self.serverdata(ctx, 'prefixes')
        if res == None:
            return ['<@'+str(bot_id)+'> ', '<@'+str(bot_id)+'>']
        else:
            return res
