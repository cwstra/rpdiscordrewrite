import asyncio
import asyncpg
import ujson as json

request = lambda c, t: 'SELECT * FROM ptu_'+c+'_'+t+';'
codexList = ['al', 'pt', '05']
categList = ['ability','blessing','capability','d_b','edge','feature','item',
             'keyword','list','maneuver','mon_forms','mon_nature','move',
             'pokemon','status']

easydata = {}

async def run():
    credentials = {"user": 'rpbot', 'password': 'genericroleplayingpassword', 'database':'server_data', 'host':'localhost'}
    out = {}
    async with asyncpg.create_pool(**credentials, min_size=1, max_size=1) as pool:
        async with pool.acquire() as conn:
            for codex in codexList:
                easydata[codex] = {} 
                for categ in categList:
                    print(categ)
                    if categ == 'pokemon':
                        entries = {i['id']:[i['alias_assoc'], i['forms_id_list']] for i in await conn.fetch(request(codex, categ))}
                    else:
                        entries = {i['id']:json.loads(i['embed']) for i in await conn.fetch(request(codex, categ))}
                    easydata[codex][categ] = entries
                    """for name in pages[codex][categ]:
                        print(codex+';'+name)
                        embed = json.loads(await conn.fetchval(request(codex, categ), name))
                        if 'init' in embed:
                            embed['init']['footer'] = {'text':pages[codex][categ][name]} 
                        else:
                            embed['footer'] = {'text':pages[codex][categ][name]}
                        await conn.execute(order(codex, categ), name, json.dumps(embed))"""

loop = asyncio.get_event_loop()
loop.run_until_complete(run())

with open('export.json','w') as f:
    json.dump(easydata, f, indent=4)
