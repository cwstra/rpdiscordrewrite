import asyncpg
import json

class StatServer:
    @classmethod
    async def create(cls, settings):
        self = StatServer()
        credentials = {"user": settings['sql'][0], "password": settings['sql'][2], "database": settings['rollStatsDB'], "host": settings['sql'][1]}
        self.commands = {}
        self.commands['server'] = {}
        self.commands['server']['serverupdate']        = """INSERT INTO server_roll_stats AS srs (id, members, stats) VALUES ($1, ARRAY_APPEND('{}'::text[], $2), $3) 
                                                            ON CONFLICT (id) DO 
                                                                UPDATE SET
                                                                    members = CASE
                                                                        WHEN $2 = ANY(srs.members)
                                                                            THEN srs.members
                                                                        ELSE ARRAY_APPEND(srs.members, $2) END,
                                                                    stats = array_union(srs.stats, $3);"""

        self.commands['server']['editrollpools']        = """INSERT INTO server_pool_roll_stats AS sprs (id, totals) VALUES ($1,$2);"""

        self.commands['server']['editrolltotal']        = """INSERT INTO server_ind_roll_stats AS sirs (id, amount) VALUES ($1,$2) 
                                                             ON CONFLICT (id) DO
                                                                UPDATE SET
                                                                    totals = sirs.totals + $2;"""

        self.commands['server']['checkrolls']          = "SELECT stats FROM server_roll_stats WHERE id = $1;"
        self.commands['server']['checkrollamount']     = "SELECT totals FROM server_pool_roll_stats WHERE id = $1;"
        self.commands['server']['checkrollamount']     = "SELECT amount FROM server_ind_roll_stats WHERE id = $1;"

        self.commands['member'] = {}
        self.commands['member']['memberupdate']        = """INSERT INTO member_roll_stats AS mrs (id, stats) VALUES ($1, $2) 
                                                            ON CONFLICT (id) DO 
                                                                UPDATE SET 
                                                                    stats = array_union(mrs.stats, $3);"""

        self.commands['member']['newrollpool']        = """INSERT INTO member_pool_roll_stats AS mprs (id, totals) VALUES ($1,$2) 
                                                             ON CONFLICT (id) DO
                                                                UPDATE SET
                                                                    totals = array_union(mprs.totals, $2);"""

        self.commands['member']['editrollamount']        = """INSERT INTO member_ind_roll_stats AS mirs (id, amount) VALUES ($1,$2) 
                                                             ON CONFLICT (id) DO
                                                                UPDATE SET
                                                                    totals = mirs.totals + $2;"""

        self.commands['member']['checkrolls']          = "SELECT stats FROM member_roll_stats WHERE id = $1;"
        self.commands['member']['checkrolls']          = "SELECT totals FROM member_pool_roll_stats WHERE id = $1;"
        self.commands['member']['checkrollamount']     = "SELECT amount FROM member_ind_roll_stats WHERE id = $1;"
        self.pool = await asyncpg.create_pool(**credentials)
        return self

    async def editStats(self, ctx, jstr):
        newStats = json.loads(jstr)
        guild = str(ctx.guild.id)
        author = str(ctx.author.id)
        conn = await self.pool.acquire()
        await conn.execute(self.commands['server']['serverupdate'], guild, author, list(newStats))
        author = guild + u'\uFEFF' + author
        await conn.execute(self.commands['member']['memberupdate'], author, list(newStats))
        for i in newStats:
            await conn.execute(self.commands['server']['editrolltotal'], guild +u'\uFEFF'+i, newStats[i])
            await conn.execute(self.commands['server']['editrolltotal'], author+u'\uFEFF'+i, newStats[i])
        await self.pool.release(conn)

    async def viewStats(self, ctx, author=None, dice=None):
        guild = str(ctx.guild.id)
        if author:
            authorstr = guild + u'\uFEFF' + str(author.id)
            memberData = await conn.fetchrow(self.commands['member']['checkrolls'], guild)
            if not(memberData):
                await self.pool.release(conn)
                return "That server member has never rolled anything."
            if dice:
                diceData = {}
                for die in dice:
                    if die in memberData['stats']:
                        statData = await conn.fetchrow(self.commands['member']['checkrollamount'], authorstr+u'\uFEFF'+i)
                        diceData[die] = statData['totals']
                    else:
                        test = die.split('d')
                        if len(test) == 2 and all(map(lambda x:x.isdigit(), test)):
                            diceData[die] = author.display_name+" has never rolled "+die+'.'
                        else:
                            diceData[die] = die+" is not a valid, trackable roll (yet!)"
                await self.pool.release(conn)
                return diceData
            else:
                statList = {}
                for i in memberData['stats']:
                    statData = await conn.fetchrow(self.commands['member']['checkrollamount'], guild+u'\uFEFF'+i)
                    statList[i] = statData['totals']
                await self.pool.release(conn)
                return statList
        else:
            serverData = await conn.fetchrow(self.commands['server']['checkrolls'], guild)
            if not(serverData):
                await self.pool.release(conn)
                return "This server has never rolled anything."
            if dice:
                diceData = {}
                for die in dice:
                    if die in serverData['stats']:
                        statData = await conn.fetchrow(self.commands['server']['checkrollamount'], guild+u'\uFEFF'+i)
                        diceData[die] = statData['totals']
                    else:
                        test = die.split('d')
                        if len(test) == 2 and all(map(lambda x:x.isdigit(), test)):
                            diceData[die] = author.display_name+" has never rolled "+die+'.'
                        else:
                            diceData[die] = die+" is not a valid, trackable roll (yet!)"
                await self.pool.release(conn)
                return diceData
            else:
                statList = {}
                for i in serverData['stats']:
                    statData = await conn.fetchrow(self.commands['server']['checkrollamount'], guild+u'\uFEFF'+i)
                    statList[i] = statData['totals']
                await self.pool.release(conn)
                return statList