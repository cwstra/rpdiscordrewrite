import ujson as json
import asyncpg
from fuzzywuzzy import process
from collections import deque, OrderedDict
import random

class Server:
    @classmethod
    async def create(cls, settings):
        self = Server()
        credentials = {"user": settings['sql'][0], "password": settings['sql'][2], "database": settings['codexDB'], "host": settings['sql'][1]}
        self.pool = await asyncpg.create_pool(**credentials)
        self.commands = {}
        self.commands['get_specific_row'] = lambda x,y:  "SELECT * FROM "+x+" WHERE "+y+" = $1;"
        self.commands['get_all_of_entry'] = 'SELECT {} FROM {};'
        self.commands['get_all'] = 'SELECT * FROM {};'
        return self

    async def close(self):
        await self.pool.close()

    @staticmethod
    def limited_words(limit, message, keys, processFun, maxFun):
        test = []
        messlength = len(message.split(' '))
        #For all possible word lengths
        for i in range(0, min(limit, messlength)):
            #Make a list of all keys with that word length.
            minikeys = [j for j in keys if j.count(' ')==i]
            #Peel off that number of words from the message
            testkey = ' '.join(message.split(' ', i+1)[:i+1])
            #Put the results into test
            processFun(test, testkey, minikeys, i)
        #Parse the final results
        return maxFun(test)

    @staticmethod
    def indTest(keys, schema, name, message):
        if schema[name] == 'flat':
            return process.extractOne(message,keys)
        else:
            return Server.limited_words(int(json.loads(schema[name])[0]), message, keys,
                lambda test, singlekey, minikeys, wordLength: test.append(process.extractOne(singlekey, minikeys)),
                lambda test: max(test, key=lambda y:y[1]))

    @staticmethod
    def array_to_dict(arr):
        d = {}
        for i in arr:
            d[i[0]] = i[1]
        return d

    async def dive(self, conn, oMessage, codex, table, keys, schema):
        message = oMessage
        for i in schema:
            if all(j in '1234567890' for j in i):
                test = self.limited_words(int(i), message, keys,
                    lambda test, singlekey, keylist, wordLength: test.append((process.extractOne(singlekey, keylist), wordLength)),
                    lambda x: max(x, key=lambda y:y[0][1]))
                if test[0][1] > 80:
                    table = await conn.fetchval(self.commands['get_specific_row'](table, 'id'), test[0][0], column=1)
                    message = message.split(' ', test[1])[test[1]]
                else:
                    return "Sorry {}, I couldn't find a good match."
            elif i.startswith('<') and i.endswith('>'):
                newTable, default = i[1:-1].split('-', 1)
                mapping = self.array_to_dict(table)
                if len(message) == 0:
                    data = await conn.fetchval(self.commands['get_specific_row'](codex+'_'+newTable,'id'), mapping[default], column=1)
                else:
                    data = await conn.fetch(self.commands['get_all_of_entry'].format('id', codex+'_'+newTable))
                    data = [i['id'] for i in data]
                    if message.find(' ') >-1:
                        k, rest = message.split(' ', 1)
                    else:
                        k, rest = message, ''
                    test = process.extractOne(k, mapping.keys())
                    if test[1] > 80:
                        data = await conn.fetchval(self.commands['get_specific_row'](codex+'_'+newTable,'id'), mapping[test[0]], column=1)
                        message = rest
                    else:
                        data = await conn.fetchval(self.commands['get_specific_row'](codex+'_'+newTable,'id'), mapping[default], column=1)
                data = json.loads(data)
            elif i.startswith('{') and i.endswith('}'):
                j = json.loads(i)
                extra_fields = data['extra_fields']
                if isinstance(extra_fields, list):
                    extra_fields = {k:v for item in extra_fields for k,v in item.items()}
                keys = list(extra_fields.keys())+list(j.keys())
                searches = [k.strip() for m in message.split(',') for k in m.split()]
                test = {k for k in [process.extractOne(k, keys) for k in searches] if k[1]>80}
                extra = []
                for k in test:
                    if k[0] in extra_fields:
                        extra.append(extra_fields[k[0]])
                    elif type(j[k[0]]) != str:
                        for l in j[k[0]]:
                            extra.append(extra_fields[l])
                d = data['init']
                if len(extra) > 0:
                    d['fields'] = list(extra)
                else:
                    d['fields'] = []
                    for k in extra_fields:
                        d['fields'].append(extra_fields[k])
                return d
            else:
                raise Exception('Not sure how I got here.')

    @staticmethod
    def initial_data(codex, message, collections, oSchema):
        schema = {i['id']:i['schema'] for i in oSchema}
        names = {}
        for i in collections:
            if i != 'schema':
                names[i] = codex + '_' + i
        if message.find(' ')>-1:
            coll, rest = message.split(' ', 1)
            coll = coll.lower()
        else:
            coll, rest = '', message
        return (schema, names, coll, rest)

    async def random(self, codex, coll):
        async with self.pool.acquire() as conn:
            #Get schema for codex
            schema = await conn.fetch(self.commands['get_all'].format(codex+'_schema'))
            try:
                schema = next(i['schema'] for i in schema if i['id'] == coll)
            except StopIteration:
                return "Sorry {}, that category doesn't exist in the current codex."
            print(schema)
            entries = await conn.fetch(self.commands['get_all_of_entry'].format('id', codex+'_'+coll))
            entry = random.choice(entries)['id']
            print(entry)
            if schema != 'flat':
                return "Sorry {}, random selections don't work for non-flat categories (yet!)"
            print(codex+'_'+coll, 'embed')
            result = await conn.fetchrow(self.commands['get_specific_row'](codex+'_'+coll, 'id'), entry)
            return json.loads(result['embed'])



    async def lookup(self, codex, message):
        async with self.pool.acquire() as conn:
            #Get list of tables for codex.
            collections = await conn.fetchval(self.commands['get_specific_row']('system_data', 'Name'), codex, column=1)
            #Get schema for codex
            schema = await conn.fetch(self.commands['get_all'].format(codex+'_schema'))
            #Parse initial data
            schema, names, coll, rest = self.initial_data(codex, message, collections, schema)
            #If coll matches a collection name
            if coll in collections:
                #Get the names of the entries in that collection
                keys = await conn.fetch(self.commands['get_all_of_entry'].format('id',names[coll]))
                #Put them into strings
                keys = [i['id'] for i in keys]
                #If that collection's schema is flat
                if schema[coll] == 'flat':
                    #Get one key
                    result = process.extractOne(rest, keys)
                    if result[1] > 80:
                        result = await conn.fetchrow(self.commands['get_specific_row'](names[coll], 'id'), result[0])
                        return json.loads(result['embed'])
                    return "Sorry {}, I couldn't find a good match."
                #Otherwise, start the dive.
                result = await self.dive(conn, rest, codex, names[coll], keys, json.loads(schema[coll]))
                return result
            else:
                test = []
                for i in collections:
                    if i != 'schema':
                        keys = await conn.fetch(self.commands['get_all_of_entry'].format('id',names[i]))
                        keys = [i['id'] for i in keys]
                        val = self.indTest(keys, schema, i, message)
                        if val != None:
                            test += [(val,i)]
                result = max(test, key=lambda x:x[0][1])
                if result[0][1] <= 80:
                    return "Sorry {}, I couldn't find a good match."
                if schema[result[1]] == 'flat':
                    result = await conn.fetchrow(self.commands['get_specific_row'](names[result[1]], 'id'), result[0][0])
                    return json.loads(result['embed'])
                message = message.split(' ', result[0][0].count(' ')+1)
                if len(message) > result[0][0].count(' ')+1:
                    message = message[-1]
                else:
                    message = ''
                table = await conn.fetchrow(self.commands['get_specific_row'](names[result[1]], 'id'), result[0][0])
                result = await self.dive(conn, message, codex, table['alias_assoc'], None, json.loads(schema[result[1]])[1:])
                return result

    async def ref(self, codex, message):
        res = await self.lookup(codex,message)
        return res

    async def schema(self, codex, oMessage):
        def reverse_alias(alias_assoc):
            reversed_assoc = {}
            for i in alias_assoc:
                if not(i[1] in reversed_assoc):
                    reversed_assoc[i[1]] = []
                reversed_assoc[i[1]].append(i[0])
            return reversed_assoc
        async with self.pool.acquire() as conn:
            message = oMessage.strip()
            schema = await conn.fetch(self.commands['get_all'].format(codex+'_schema'))
            if message != '':
                collections = await conn.fetchval(self.commands['get_specific_row']('system_data', 'Name'), codex, column=1)
                schema, names, category, rest = self.initial_data(codex, message, collections, schema)
                if rest == '':
                    res = category + '\n'
                    for i in schema[category]:
                        if i.startswith('<') and i.endswith('>'):
                            s = i[1:-1].split('-')
                            res += '\tSubentry: '+s[0]+'\n'
                            res += '\t\tDefault Key: '+s[1]+'\n'
                        elif i.startswith('{') and i.endswith('}'):
                            res += '\tPartial Fields\n'
                            res += '\t\tCheck individual entries for a list of relevant subfields.'
                    return res
                if category in collections:
                    keys = await conn.fetch(self.commands['get_all_of_entry'].format('id',names[category]))
                    entryKey = self.indTest(keys, schema, i, message)
                else:
                    testRes = []
                    for i in collections:
                        if i!='schema':
                            keys = map(lambda x: x['id'], await conn.fetch(self.commands['get_all_of_entry'].format('id',names[i])))
                            testRes += [(self.indTest(keys, schema, i, message),i)]
                    entryKey, category = max(testRes, key=lambda x:x[0][1])
                if schema[category] == 'flat':
                    return 'Elements of the requested category are flat.'
                schema[category] = deque(json.loads(schema[category]))
                if entryKey[1] > 80:
                    entryKey = entryKey[0]
                    table = await conn.fetchval(self.commands['get_specific_row'](codex+'_'+category, 'id'), entryKey, column=1)
                    message = message.split(' ', entryKey.count(' ')+1)
                    if len(message)<=entryKey.count(' ')+1:
                        message = ''
                    else:
                        message = message[entryKey.count(' ')+1].lstrip()
                    nextTest = ''
                    if message != '':
                        schema[category].popleft()
                        while message != '' and len(schema[category])>0 and schema[category][0].startswith('<') and schema[category][0].endswith('>'):
                            s = schema[category][0][1:-1].split('-')
                            if message.find(' ') > -1:
                                k, r = message.split(' ', 1)
                                r = message.strip()
                            else:
                                k, r = message, ''
                            mapping = self.array_to_dict(table)
                            minitest = process.extractOne(k, mapping.keys())
                            if minitest[1] > 80:
                                nextTest += ' -> '+minitest[0]
                                table = await conn.fetchval(self.commands['get_specific_row'](codex+'_'+s[0], 'id'), mapping[minitest[0]], column=1)
                                schema[category].popleft()
                            else:
                                break
                    res = 'Best Result for ' + oMessage +':\n'
                    res += entryKey + nextTest + '\n'
                    for i in schema[category]:
                        if i.startswith('<') and i.endswith('>'):
                            s = i[1:-1].split('-')
                            res += '\tSubentries: '+s[0]+'\n'
                            ar = self.array_to_dict(table)
                            ra = reverse_alias(table)
                            for j in ra:
                                res += '\t\t'+j+' with aliases '+', '.join([k for k in ra[j] if k!=j])+'\n'
                            res += '\tDetails for default entry '+s[1]+':\n'
                            table = await (conn.fetchval(self.commands['get_specific_row'](codex+'_'+s[0], 'id'), ar[s[1]], column=1))
                        elif i.startswith('{') and i.endswith('}'):
                            s = json.loads(i)
                            res += '\tSubfields:\n'
                            res += '\t\tSimple Subfields:\n'
                            entry = json.loads(table)
                            for j in entry['extra_fields']:
                                res += '\t\t\t'+j+'\n'
                            res += '\t\tGroup Subfields:\n'
                            for j in s:
                                res += '\t\t\t'+j
                                if s[j] == 'default':
                                    res += ': returns all Simple Subfields\n'
                                else:
                                    res += ': returns the following Simple Subfields:\n'
                                    for k in s[j]:
                                        res += '\t\t\t\t'+k+'\n'
                            return res
                else:
                    return "Sorry {}, I couldn't find a good match for that query."
            res = ''
            schema = sorted(i for i in schema)
            for i in schema:
                res += i['id'] + '\n'
                if i['schema'] == 'flat':
                    res += "\tFlat\n"
                else:
                    entry = json.loads(i['schema'])[1:]
                    for j in entry:
                        if j.startswith('<') and j.endswith('>'):
                            s = j[1:-1].split('-')
                            res += '\tSubentry: '+s[0]+'\n'
                            res += '\t\tDefault Key: '+s[1]+'\n'
                        elif j.startswith('{') and j.endswith('}'):
                            res += '\tPartial Fields:\n'
                            res += '\t\tCheck individual entries for a list of relevant subfields.\n'
            return res

    async def top(self, codex, number, message, logger):
        def test(keys, schema_entry, mess, num):
            if schema_entry == 'flat':
                return process.extract(mess, keys, limit=num)
            else:
                parsed = json.loads(schema_entry)
                return self.limited_words(int(json.loads(schema_entry)[0]),message, keys,
                    lambda test, singlekey, minikeys, wordLength: test.extend(process.extract(singlekey, minikeys, limit=num)),
                    lambda test: sorted(test, key=lambda x: x[1], reverse=True)[:num])
        async with self.pool.acquire() as conn:
            collections = await conn.fetchval(self.commands['get_specific_row']('system_data', 'Name'), codex, column=1)
            schema = await conn.fetch(self.commands['get_all'].format(codex+'_schema'))
            schema, names, coll, rest = self.initial_data(codex, message, collections, schema)
            if coll in collections:
                keys = await conn.fetch(self.commands['get_all_of_entry'].format('id',names[coll]))
                keys = [i['id'] for i in keys]
                results = test(keys, schema[coll], rest, number)
                res = ''
                mode = 0
                for i in results:
                    if mode == 0 and i[1] > 80:
                        res += "Strong Matches:\n"
                        mode = 1
                    elif mode < 2 and i[1] <= 80:
                        res += "Weak Matches:\n"
                        mode = 2
                    res += f'\t"{i[0]}" ({i[1]})\n'
                return (res, coll)
            else:
                testRes = []
                for i in collections:
                    if i != 'schema':
                        keys = await conn.fetch(self.commands['get_all_of_entry'].format('id',names[i]))
                        keys = [i['id'] for i in keys]
                        testRes += list(map(lambda x:(x,i), test(keys, schema[i], message, number)))
                results = sorted(testRes, key=lambda x:x[0][1], reverse=True)[:number]
                res = ''
                mode = 0
                for i in results:
                    logger(i[0][1])
                    if mode == 0 and i[0][1] > 80:
                        res += "Strong Matches:\n"
                        mode = 1
                    elif mode < 2 and i[0][1] <= 80:
                        res += "Weak Matches:\n"
                        mode = 2
                    res += f'\t"{i[0][0]}" in {i[1]} ({i[0][1]})\n'
                return (res, None)

if __name__ == "__main__":
    import asyncio
    import sys
    async def run():
        s = await Server.create()
        print(await s.lookup(sys.argv[1],sys.argv[2]))
    asyncio.get_event_loop().run_until_complete(run())
