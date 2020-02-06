import asyncio
import asyncpg
import json

request = 'UPDATE ptu_ga_mon_forms SET embed = $2 WHERE id = $1;'

with open('galar_data.json') as f:
    d = json.load(f)

async def run():
    credentials = {"user": 'rpbot', 'password': 'genericroleplayingpassword', 'database':'server_data', 'host':'localhost'}
    out = {}
    async with asyncpg.create_pool(**credentials, min_size=1, max_size=1) as pool:
        async with pool.acquire() as conn:
            for num, emb in d.items():
                await conn.execute(request, num, emb)

loop = asyncio.get_event_loop()
loop.run_until_complete(run())
