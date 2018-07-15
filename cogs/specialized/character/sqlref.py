import asyncpg

class CharExistsException(Exception):
    pass

class CharDoesNotExistException(Exception):
    pass

class AttrDoesNotExistException(Exception):
    pass

class EditNotAllowedException(Exception):
    pass

async def hstoreSetup(conn):
    await conn.set_builtin_type_codec('hstore', codec_name='pg_contrib.hstore')

def mergeAttrs(olddict, newdict):
    return {i:j for i, j in {**olddict, **newdict}.items() if j != None}

class Server:
    @classmethod
    async def create(cls, settings):
        self = Server()
        credentials = {"user": settings['sql'][0], "password": settings['sql'][2], "database":  settings['characterDB'], "host": settings['sql'][1], "init":hstoreSetup}
        self.commands = {}
        self.commands['characters'] = {}
        self.commands['characters']['newchar'] = "INSERT INTO characters (server_id, member_id, character_name, attributes) VALUES ($1, $2, $3, $4);"
        self.commands['characters']['getchar'] = "SELECT * FROM characters WHERE server_id = $1 AND character_name  = $2;"
        self.commands['characters']['checkchar'] = "SELECT TRUE FROM characters WHERE server_id = $1 AND character_name = $2;"
        self.commands['characters']['listchar'] = "SELECT character_name, member_id FROM characters WHERE server_id = $1;"
        self.commands['characters']['update'] = "UPDATE characters SET attributes = $3 WHERE server_id = $1 AND character_name = $2;"
        self.commands['characters']['updatewithname'] = "UPDATE characters SET character_name = $3, attributes = $4 WHERE server_id = $1 AND character_name = $2;"
        self.commands['characters']['delete'] = "DELETE FROM characters WHERE server_id = $1 AND character_name = $2;"
        self.pool = await asyncpg.create_pool(**credentials)
        return self

    async def newInfo(self, ctx, charactername, attrdict = None):
        async with self.pool.acquire() as conn:
            author = ctx.author.id
            charentry = await conn.fetchrow(self.commands['characters']['checkchar'], ctx.guild.id, charactername)
            if charentry:
                raise CharExistsException(charactername)
            if not(attrdict):
                attrdict = {}
            await conn.execute(self.commands['characters']['newchar'], ctx.guild.id, author, charactername, attrdict)

    async def getInfo(self, ctx, charactername, attrs=None):
        async with self.pool.acquire() as conn:
            author = ctx.author.id
            charentry = await conn.fetchrow(self.commands['characters']['getchar'], ctx.guild.id, charactername)
            if not(charentry):
                raise CharDoesNotExistException(charactername)
            if attrs:
                l = {i:j for i,j in charentry['attributes'].items() if i in attrs} 
                if len(l) == 0:
                    raise AttrDoesNotExistException(charactername, attrs, charentry['attributes'])
                return l
            else:
                return (charentry['member_id'], charentry['attributes'])

    async def listInfo(self, ctx):
        async with self.pool.acquire() as conn:
            entries = await conn.fetch(self.commands['characters']['listchar'], ctx.guild.id)
            out = [(i['character_name'], i['member_id']) for i in entries]
            return out 

    async def editInfo(self, ctx, charactername, predicate, newattrdict, newname=None):
        async with self.pool.acquire() as conn:
            author = ctx.author.id
            charentry = await conn.fetchrow(self.commands['characters']['getchar'], ctx.guild.id, charactername)
            if not(charentry):
                raise CharDoesNotExistException(charactername)
            elif not(predicate(charentry['member_id'])):
                raise EditNotAllowedException(charactername, charentry['member_id'])
            mergedattrdict = mergeAttrs(charentry['attributes'], newattrdict)
            if newname:
                await conn.execute(self.commands['characters']['updatewithname'], ctx.guild.id, charactername, newname, mergedattrdict)
            else:
                await conn.execute(self.commands['characters']['update'], ctx.guild.id, charactername, mergedattrdict)

    async def delInfo(self, ctx, charactername):
        async with self.pool.acquire() as conn:
            charentry = await conn.fetchrow(self.commands['characters']['checkchar'], ctx.guild.id, charactername)
            if not(charentry):
                raise CharDoesNotExistException(charactername)
            await conn.execute(self.commands['characters']['delete'], ctx.guild.id, charactername)

    async def getBatchInfo(self, ctx, dataIter):
        async with self.pool.acquire() as conn:
            out = {}
            for char, attr in dataIter:
                entry = await conn.fetchrow(self.commands['characters']['getchar'], ctx.guild.id, char)
                if entry != None and attr in entry['attributes']:
                    out[(char, attr)] = entry['attributes'][attr]
            return out

    async def checkInfo(self, ctx, charactername):
        async with self.pool.acquire() as conn:
            author = ctx.author.id
            charentry = await conn.fetchrow(self.commands['characters']['checkchar'], ctx.guild.id, charactername)
            return (charentry, private)
