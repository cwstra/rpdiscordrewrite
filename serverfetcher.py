import json
import asyncpg

class ServerFetcher:
    def __init__(self):
        self.commands = {}
        self.commands['servers'] = {}
        self.commands['servers']['get_specific_entry'] = "SELECT {} FROM servers WHERE {} = {};"
        self.commands['servers']['get_specific_row'] = "SELECT * FROM servers WHERE {} = {};"
        self.commands['servers']['get_all_of_entry_and_id'] = 'SELECT (id,{}) FROM servers;'
        self.commands['servers']['get_all_of_entry'] = 'SELECT {} FROM servers;'
        self.commands['servers']['get_all'] = 'SELECT * FROM servers;'
        self.commands['servers']['upsert'] = 'INSERT INTO servers (id, {0}) VALUES ({1}, {2}) ON CONFLICT (id) DO UPDATE SET {0}={2};'
        self.commands['servers']['clear_empty_rows'] = "DELETE FROM servers WHERE ((codex IS NULL or codex = '') and (permissionroles IS NULL or permissionroles = '{}') and (charsigns IS NULL or charsigns = '{}') and (prefixes IS NULL or prefixes = '{}') and (inline IS NULL or NOT inline));"
        self.commands['codex_list'] = {}
        self.commands['codex_list']['get_name'] = 'SELECT display_name FROM codex_list WHERE id = $1;'
        self.commands['codex_list']['get_all'] = 'SELECT * FROM codex_list;'

    async def init_pool(self, settings):
        self.credentials = {"user": settings['sql'][0], "password": settings['sql'][2], "database": settings['serverSettingsDB'], "host": settings['sql'][1]}
        self.pool = await asyncpg.create_pool(**self.credentials)

    async def serverdata(self, guild_id=None, entry=None):
        async with self.pool.acquire() as conn:
            if guild_id and entry:
                res = await conn.fetchrow(self.commands['servers']['get_specific_entry'].format(entry, 'id', guild_id))
                if res:
                    res = res[entry]
            elif guild_id:
                res = await conn.fetchrow(self.commands['servers']['get_specific_row'].format('id', guild_id))
            elif entry:
                res = await conn.fetch(self.commands['servers']['get_all_of_entry_and_id'].format(entry))
                res = {i['id']:i[entry] for i in res}
            else:
                res = await conn.fetch(self.commands['servers']['get_all'])
                res = {i['id']:{j:k for j,k in i.items() if j!='id'} for i in res}
            return res

    async def systemlist(self, name = None):
        async with self.pool.acquire() as conn:
            if name:
                res = await conn.fetchval(self.commands['codex_list']['get_name'], name)
                return res
            res = await conn.fetch(self.commands['codex_list']['get_all'])
            return res

    async def upsert_entry(self, guild_id, entryDict=None):
        async with self.pool.acquire() as conn:
            if entryDict:
                for i, j in entryDict.items():
                    await conn.execute(self.commands['servers']['upsert'].format(i, '$1', '$2'), guild_id, j)
                await conn.execute(self.commands['servers']['clear_empty_rows'])

    async def check_for_server(self, ctx):
        async with self.pool.acquire() as conn:
            return bool(await conn.execute(self.commands['get_specific_entry'].format('TRUE', 'id', ctx.guild.id)))

    # Get all the prefixes the guild can use
    async def prefixes_for(self, guild_id, bot_id):
        res = await self.serverdata(guild_id, 'prefixes')
        if res == None:
            return ['<@'+str(bot_id)+'> ', '<@'+str(bot_id)+'>']
        else:
            return res
