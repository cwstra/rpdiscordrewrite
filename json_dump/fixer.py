import asyncio
import asyncpg
import re
import ujson as json

request = lambda v, c: 'SELECT * FROM ptu_'+v+'_'+c+';'

verList = ['05', 'pt', 'al', 'ga']
catList = ['ability', 'blessing','capability','d_b','edge','feature','item',
           'keyword', 'list', 'maneuver', 'mon_forms', 'mon_nature', 'move',
           'pokemon', 'status']

relCats = [i for i in catList if i not in ['mon_forms', 'pokemon']]

outputs = {v: {c:[] for c in relCats} for v in verList}

async def run():
    credentials = {"user": 'rpbot', 'password': 'genericroleplayingpassword', 'database':'server_data', 'host':'localhost'}
    async with asyncpg.create_pool(**credentials, min_size=1, max_size=1) as pool:
        async with pool.acquire() as conn:
            for version in verList:
                for cat in relCats:
                    data = await conn.fetch(request(version, cat))
                    for entry in data:
                        print(entry['embed'])
                        fields = json.loads(entry['embed'])['fields']
                        if any('name' not in list(f.keys()) or
                               'value' not in list(f.keys()) for f in fields):
                            outputs[version][cat].append(data['id'])

loop = asyncio.get_event_loop()
loop.run_until_complete(run())
