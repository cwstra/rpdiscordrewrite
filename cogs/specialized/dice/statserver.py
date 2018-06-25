import asyncpg
import ujson as json
from .chartmaker import dualChart

async def hstoreSetup(conn):
    await conn.set_builtin_type_codec('hstore', codec_name='pg_contrib.hstore')

class StatServer:
    @classmethod
    async def create(cls, settings):
        self = StatServer()
        credentials = {"user": settings['sql'][0], "password": settings['sql'][2], "database": settings['rollStatsDB'], "host": settings['sql'][1], "init":hstoreSetup}
        self.commands = {}
        self.commands['server'] = {}
        self.commands['server']['newserver']        = "INSERT INTO server_roll_stats (server_id, members, stats) VALUES ($1, ARRAY_APPEND('{}'::bigint[], $2), $3)"
        self.commands['server']['updateserver']     = """UPDATE server_roll_stats as srs SET 
                                                            members = CASE
                                                                        WHEN $2=ANY(srs.members) THEN srs.members
                                                                        ELSE ARRAY_APPEND(srs.members,$2)
                                                                      END, 
                                                            stats = $3 
                                                         WHERE server_id = $1;"""
        self.commands['server']['newrollpools']     = "INSERT INTO server_pool_roll_stats (server_id, pool, stats) VALUES ($1,$2,$3);"
        self.commands['server']['updaterollpools']  = "UPDATE server_pool_roll_stats SET stats = $3 WHERE server_id = $1 AND pool = $2;"
        self.commands['server']['checkbig']         = lambda x: "SELECT "+x+" FROM server_roll_stats WHERE server_id = $1;" #*, stats, members 
        self.commands['server']['checkrolls']       = "SELECT stats FROM server_pool_roll_stats WHERE server_id = $1 AND pool = $2;" 

        self.commands['member'] = {}
        self.commands['member']['newmember']        = "INSERT INTO member_roll_stats (server_id, member_id, stats) VALUES ($1, $2, $3)" 
        self.commands['member']['updatemember']     = "UPDATE member_roll_stats SET stats = $3 WHERE server_id = $1 AND member_id = $2;"
        self.commands['member']['newrollpools']     = "INSERT INTO member_pool_roll_stats (server_id, member_id, pool, stats) VALUES ($1,$2,$3,$4);"
        self.commands['member']['updaterollpools']  = "UPDATE member_pool_roll_stats SET stats = $3 WHERE server_id = $1 AND pool = $2;"
        self.commands['member']['checkpools']       = "SELECT stats FROM member_roll_stats WHERE server_id = $1 AND member_id = $2;"
        self.commands['member']['checkrolls']       = "SELECT stats FROM member_pool_roll_stats WHERE server_id = $1 AND member_id = $2 AND pool = $3;"
        self.pool = await asyncpg.create_pool(**credentials)
        return self

    async def editStats(self, ctx, jstr):
        def mergeDice(oldDice, newDice):
            return {**oldDice, **{i:(str(int(oldDice[i])+newDice[i]) if (i in oldDice) else str(newDice[i])) for i in newDice}}
        newStats = json.loads(jstr)
        guild = ctx.guild.id
        author = ctx.author.id
        async with self.pool.acquire() as conn:
            oldServerStats = await conn.fetchrow(self.commands['server']['checkbig']('*'), guild)
            if oldServerStats:
                oldServerStats = oldServerStats['stats']
                await conn.execute(self.commands['server']['updateserver'], guild, author, list(set(oldServerStats).union(set(newStats))))
            else:
                oldServerStats = []
                await conn.execute(self.commands['server']['newserver'],    guild, author, list(newStats)) 
            oldMemberStats = await conn.fetchrow(self.commands['member']['checkpools'], guild, author)
            if oldMemberStats:
                oldMemberStats = oldMemberStats['stats']
                await conn.execute(self.commands['member']['updatemember'], guild, author, list(set(oldMemberStats).union(set(newStats))))
            else:
                oldMemberStats = []
                await conn.execute(self.commands['member']['newmember'],    guild, author, list(newStats))
            for i in newStats:
                if i in oldServerStats:
                    oldDice = await conn.fetchrow(self.commands['server']['checkrolls'], guild, i)
                    oldDice = oldDice['stats'] if oldDice else {} 
                    newDice = mergeDice(oldDice, newStats[i])
                    print(type(guild), type(i), newDice)
                    await conn.execute(self.commands['server']['updaterollpools'], guild, i, newDice)
                    if i in oldMemberStats:
                        oldDice = await conn.fetchrow(self.commands['member']['checkrolls'], guild, author, i)
                        oldDice = oldDice['stats'] if oldDice else {} 
                        newDice = mergeDice(oldDice, newStats[i])
                        await conn.execute(self.commands['member']['updaterollpools'], guild, i, newDice)
                    else:
                        await conn.execute(self.commands['member']['newrollpools'], guild, author, i, {i:str(j) for i,j in newStats[i].items()})
                else:
                    newDice = {i:str(j) for i,j in newStats[i].items()}
                    await conn.execute(self.commands['server']['newrollpools'], guild, i, newDice)
                    await conn.execute(self.commands['member']['newrollpools'], guild, author, i, newDice)

    async def viewStats(self, ctx, author=None, dice=None):
        guild = ctx.guild.id
        async with self.pool.acquire() as conn:
            if author:
                if type(author) == str and author.lower() == 'myself':
                    author = ctx.author.id
                memberData = await conn.fetchrow(self.commands['member']['checkpools'], guild, author)
                if not(memberData):
                    return "That server member has never rolled anything."
                if dice:
                    diceData = {}
                    for die in dice:
                        if die in memberData['stats']:
                            statData = await conn.fetchrow(self.commands['member']['checkrolls'], guild, author, die)
                            diceData[die] = statData['stats']
                        else:
                            test = die.split('d')
                            if len(test) == 2 and all(map(lambda x:x.isdigit(), test)):
                                diceData[die] = author.display_name+" has never rolled "+die+'.'
                            else:
                                diceData[die] = die+" is not a valid, trackable roll (yet!)"
                    return diceData
                else:
                    statList = {}
                    for die in memberData['stats']:
                        statData = await conn.fetchrow(self.commands['member']['checkrolls'], guild, author, die)
                        statList[die] = statData['stats']
                    return statList
            else:
                serverData = await conn.fetchrow(self.commands['server']['checkbig']('stats'), guild)
                if not(serverData):
                    return "This server has never rolled anything."
                if dice:
                    diceData = {}
                    for die in dice:
                        if die in serverData['stats']:
                            statData = await conn.fetchrow(self.commands['server']['checkrolls'], guild, die)
                            diceData[die] = statData['stats']
                        else:
                            test = die.split('d')
                            if len(test) == 2 and all(map(lambda x:x.isdigit(), test)):
                                diceData[die] = author.display_name+" has never rolled "+die+'.'
                            else:
                                diceData[die] = die+" is not a valid, trackable roll (yet!)"
                    return diceData
                else:
                    statList = {}
                    for die in serverData['stats']:
                        statData = await conn.fetchrow(self.commands['server']['checkrolls'], guild, die)
                        statList[die] = statData['stats']
                    return statList

    async def plot(self, loop, prefix, servername, pool, results):
        return await loop.run_in_executor(None, dualChart, prefix, servername, pool, results)
