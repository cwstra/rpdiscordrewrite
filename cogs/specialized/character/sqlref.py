import asyncpg

class CharExistsException(Exception):
    pass

class CharDoesNotExistException(Exception):
    pass

class AttrDoesNotExistException(Exception):
    pass

class Server:
    @classmethod
    async def create(cls, settings):
        self = Server()
        credentials = {"user": settings['sql'][0], "password": settings['sql'][2], "database":  settings['characterDB'], "host": settings['sql'][1]}
        self.commands = {}
        self.commands['characters'] = {}
        self.commands['characters']['newchar'] = "INSERT INTO characters (id, author, attributes) VALUES ($1, $2, $3);"
        self.commands['characters']['getchar'] = "SELECT * FROM characters WHERE id = $1"
        self.commands['characters']['getid'] = "SELECT id FROM characters WHERE id = $1"
        self.commands['characters']['getallidsfromserver'] = "SELECT * FROM characters WHERE POSITION($1 in id) = 1"
        self.commands['characters']['update'] = "UPDATE characters SET attributes = $2 WHERE ID = $1;"
        self.commands['characters']['rename'] = "UPDATE characters SET ID = $2 WHERE ID = $1;"
        self.commands['characters']['delete'] = "DELETE FROM characters WHERE ID = $1;"
        self.commands['attributes'] = {}
        self.commands['attributes']['newattr'] = "INSERT INTO attributes (id, value) VALUES ($1, $2);"
        self.commands['attributes']['upsert'] = 'INSERT INTO attributes (id, value) VALUES ($1, $2) ON CONFLICT (id) DO UPDATE SET value=$2;'
        self.commands['attributes']['getentry'] = 'SELECT value FROM attributes WHERE id = $1;'
        self.commands['attributes']['rename'] = "UPDATE attributes SET ID = $2 WHERE ID = $1;"
        self.commands['attributes']['remove'] = 'DELETE FROM attributes WHERE id = $1'
        self.pool = await asyncpg.create_pool(**credentials)
        return self

    async def newInfo(self, ctx, charactername, attrdict = None, private = False, author = None):
        conn = await self.pool.acquire()
        if not(author):
            author = ctx.author.id
        if private:
            charkey = str(ctx.guild.id) + u"\uFEFF" + str(author) + u"\uFEFF" + charactername
        else:
            charkey = str(ctx.guild.id) + u"\uFEFF" + charactername
        charentry = await conn.fetchrow(self.commands['characters']['getid'], charkey)
        if charentry:
            await self.pool.release(conn)
            raise CharExistsException(charactername)
        if attrdict:
            attrkeys = list(attrdict.keys())
        else:
            attrkeys = []
        await conn.execute(self.commands['characters']['newchar'], charkey, author, attrkeys)
        for i in attrdict:
            await conn.execute(self.commands['attributes']['newattr'], charkey + u"\uFEFF" + i, attrdict[i])
        await self.pool.release(conn)

    async def renameInfo(self, ctx, charactername, newname, private = False, author = None):
        conn = await self.pool.acquire()
        if not(author):
            author = ctx.author.id
        if not(private):
            newcharkey = str(ctx.guild.id) + u"\uFEFF"
            oldcharkey = newcharkey + charactername
            newcharkey += newname
            oldcharentry = await conn.fetchrow(self.commands['characters']['getchar'], oldcharkey)
        else:
            newcharkey = str(ctx.guild.id) + u"\uFEFF"
            oldcharkey = newcharkey + str(author) + u"\uFEFF" + charactername
            oldcharentry = await conn.fetchrow(self.commands['characters']['getchar'], oldcharkey)
            if oldcharentry:
                newcharkey += str(author) + u"\uFEFF" + newname
            else:
                oldcharkey = newcharkey + charactername
                newcharkey += newname
                oldcharentry = await conn.fetchrow(self.commands['characters']['getchar'], oldcharkey)
        if not(oldcharentry):
            await self.pool.release(conn)
            raise CharDoesNotExistException(charactername)
        for i in oldcharentry['attributes']:
            await conn.execute(self.commands['attributes']['rename'], oldcharkey + u"\uFEFF" + i, newcharkey + u"\uFEFF" + i)
        await conn.execute(self.commands['characters']['rename'], oldcharkey, newcharkey)
        await self.pool.release(conn)

    async def editInfo(self, ctx, charactername, attrdict, private = False, author = None):
        conn = await self.pool.acquire()
        if not(author):
            author = ctx.author.id
        if not(private):
            charkey = str(ctx.guild.id) + u"\uFEFF" + charactername
            charentry = await conn.fetchrow(self.commands['characters']['getchar'], charkey)
        else:
            charkey = str(ctx.guild.id) + u"\uFEFF" + str(author) + u"\uFEFF" + charactername
            charentry = await conn.fetchrow(self.commands['characters']['getchar'], charkey)
            if not(charentry):
                charkey = str(ctx.guild.id) + u"\uFEFF" + charactername
                charentry = await conn.fetchrow(self.commands['characters']['getchar'], charkey)
        if not(charentry):
            await self.pool.release(conn)
            raise CharDoesNotExistException(charactername)
        attrkeys = set(charentry['attributes'])
        for i in attrdict:
            if attrdict[i] == None:
                if i in attrkeys:
                    attrkeys.remove(i)
                    await conn.execute(self.commands['attributes']['remove'], charkey + u"\uFEFF" + i)
            else:
                attrkeys.add(i)
                await conn.execute(self.commands['attributes']['upsert'], charkey + u"\uFEFF" + i, attrdict[i])
        await conn.execute(self.commands['characters']['update'], charkey, list(attrkeys))
        await self.pool.release(conn)

    async def getInfo(self, ctx, charactername, attrs=None, private = False, author = None):
        conn = await self.pool.acquire()
        if not(author):
            author = ctx.author.id
        if not(private):
            charkey = str(ctx.guild.id) + u"\uFEFF" + charactername
            charentry = await conn.fetchrow(self.commands['characters']['getchar'], charkey)
        else:
            charkey = str(ctx.guild.id) + u"\uFEFF" + str(author) + u"\uFEFF" + charactername
            charentry = await conn.fetchrow(self.commands['characters']['getchar'], charkey)
            if not(charentry):
                charkey = str(ctx.guild.id)+u"\uFEFF"+charactername
                charentry = await conn.fetchrow(self.commands['characters']['getchar'], charkey)
        if not(charentry):
            await self.pool.release(conn)
            raise CharDoesNotExistException(charactername)
        if attrs:
            l = {}
            for i in attrs:
                if i in charentry['attributes']:
                    attrentry = await conn.fetchrow(self.commands['attributes']['getentry'], charkey + u"\uFEFF" + i)
                    l[i] = attrentry['value']
            if len(l) == 0:
                raise AttrDoesNotExistException(charactername, attrs, charentry['attributes'])
            await self.pool.release(conn)
            return l
        else:
            await self.pool.release(conn)
            return (charentry['author'], charentry['attributes'])

    async def getBatchInfo(self, ctx, dataDict):
        conn = await self.pool.acquire()
        out = {}
        for i, val in dataDict.items():
            key = str(ctx.guild.id) + u"\uFEFF" + i + u"\uFEFF" + val[0]
            entry = await conn.fetchrow(self.commands['attributes']['getentry'], key)
            if entry != None:
                out[i] = entry['value']
        return out

    async def getServerInfo(self, ctx):
        conn = await self.pool.acquire()
        key = str(ctx.guild.id)+u"\uFEFF"
        out = await conn.fetch(self.commands['characters']['getallidsfromserver'], str(ctx.guild.id)+u"\uFEFF")
        return out

    async def delInfo(self, ctx, charactername, private = False, author = None):
        conn = await self.pool.acquire()
        if not(author):
            author = ctx.author.id
        if not(private):
            charkey = str(ctx.guild.id) + u"\uFEFF" + charactername
            charentry = await conn.fetchrow(self.commands['characters']['getchar'], charkey)
        else:
            charkey = str(ctx.guild.id) + u"\uFEFF" + str(author) + u"\uFEFF" + charactername
            charentry = await conn.fetchrow(self.commands['characters']['getchar'], charkey)
            if not(charentry):
                charkey = str(ctx.guild.id)+u"\uFEFF"+charactername
                charentry = await conn.fetchrow(self.commands['characters']['getchar'], charkey)
        if not(charentry):
            await self.pool.release(conn)
            raise CharDoesNotExistException(charactername)
        for i in charentry['attributes']:
            await conn.execute(self.commands['attributes']['remove'], charkey + u"\uFEFF" + i)
        await conn.execute(self.commands['characters']['delete'], charkey)
        await self.pool.release(conn)

    async def checkInfo(self, ctx, charactername, private = False, author = None):
        conn = await self.pool.acquire()
        if not(author):
            author = ctx.author.id
        if not(private):
            private = False
            charkey = str(ctx.guild.id) + u"\uFEFF" + charactername
            charentry = await conn.fetchrow(self.commands['characters']['getid'], charkey)
        else:
            private = True
            charkey = str(ctx.guild.id) + u"\uFEFF" + str(author) + u"\uFEFF" + charactername
            charentry = await conn.fetchrow(self.commands['characters']['getid'], charkey)
            if not(charentry):
                private = False
                charkey = str(ctx.guild.id)+u"\uFEFF"+charactername
                charentry = await conn.fetchrow(self.commands['characters']['getid'], charkey)
        await self.pool.release(conn)
        return (charentry, private)
