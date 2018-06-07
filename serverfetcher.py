import json
import asyncpg

commands = {}
commands['servers'] = {}
commands['servers']['get_specific_entry'] = "SELECT {} FROM servers WHERE {} = {};"
commands['servers']['get_specific_row'] = "SELECT * FROM servers WHERE {} = {};"
commands['servers']['get_all_of_entry_and_id'] = 'SELECT (id,{}) FROM servers;'
commands['servers']['get_all_of_entry'] = 'SELECT {} FROM servers;'
commands['servers']['get_all'] = 'SELECT * FROM servers;'
commands['servers']['upsert'] = 'INSERT INTO servers (id, {0}) VALUES ({1}, {2}) ON CONFLICT (id) DO UPDATE SET {0}={2};'
commands['servers']['clear_empty_rows'] = "DELETE FROM servers WHERE ((codex IS NULL or codex = '') and (permissionroles IS NULL or permissionroles = '{}') and (charsigns IS NULL or charsigns = '{}') and (prefixes IS NULL or prefixes = '{}') and (inline IS NULL or NOT inline));"
commands['codex_list'] = {}
commands['codex_list']['get_all'] = 'SELECT * FROM codex_list;'

async def init_pool(settings):
    global pool
    global credentials
    credentials = {"user": settings['sql'][0], "password": settings['sql'][2], "database": settings['serverSettingsDB'], "host": settings['sql'][1]}
    pool = await asyncpg.create_pool(**credentials)

async def serverdata(guild_id=None, entry=None):
    conn = await pool.acquire()
    if guild_id and entry:
        res = await conn.fetchrow(commands['servers']['get_specific_entry'].format(entry, 'id', guild_id))
        if res:
            res = res[entry]
    elif guild_id:
        res = await conn.fetchrow(commands['servers']['get_specific_row'].format('id', guild_id))
    elif entry:
        res = await conn.fetch(commands['servers']['get_all_of_entry_and_id'].format(entry))
        res = {i['id']:i[entry] for i in res}
    else:
        res = await conn.fetch(commands['servers']['get_all'])
        res = {i['id']:{j:k for j,k in i.items() if j!='id'} for i in res}
    await pool.release(conn)
    return res

async def systemlist():
    conn = await pool.acquire()
    res = await conn.fetch(commands['codex_list']['get_all'])
    print('Here')
    print(res)
    res = [i['id'] for i in res]
    await pool.release(conn)
    return res

async def upsert_entry(guild_id, entryDict=None):
    conn = await pool.acquire()
    if entryDict:
        for i, j in entryDict.items():
            print(guild_id,i,j)
            print(commands['servers']['upsert'].format(i, '$1', '$2'), guild_id, j)
            await conn.execute(commands['servers']['upsert'].format(i, '$1', '$2'), guild_id, j)
        await conn.execute(commands['servers']['clear_empty_rows'])
    await pool.release(conn)

# Get all the prefixes the guild can use
async def prefixes_for(guild_id, bot_id):
    res = await serverdata(guild_id, 'prefixes')
    if res == None:
        return ['<@'+str(bot_id)+'> ', '<@'+str(bot_id)+'>']
    else:
        return res