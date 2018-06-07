
def simplifyNumber(n):
    if type(n) == complex and n.imag==0:
        n = n.real
    if (type(n) == float and n.is_integer()) or (type(n)==frac and n.denominator==1):
        n = int(n)
    return n

def parseSimpleNumber(n):
    return simplifyNumber(complex(n))

def forceInt(n):
    if isinstance(n, complex):
        n = n.real
    return int(n)

class Vector:
    def __init__(self, data=None):
        if data:
            self.data = data
        else:
            self.data = []

    def __len__(self, l=None):
        return len(self.data)

    def __add__(self, other):
        if type(other)==Vector and len(self) == len(other):
            result = Vector()
            endTest = False
            for ind, data in enumerate(self.data):
                try:
                    entry = simplifyNumber(parseSimpleNumber(data)+parseSimpleNumber(other.data[ind]))
                except ValueError:
                    entry = data+other[ind]
                    endTest = True
                result.addElement(entry)
            return result
        elif isinstance(other, numbers.Number):
            return Vector([simplifyNumber(parseSimpleNumber(i)+other) for i in self.data])
        else:
            return False

    def __sub__(self, other):
        if type(other)==Vector and len(self) == len(other):
            result = Vector()
            endTest = False
            for ind, data in enumerate(self.data):
                try:
                    entry = simplifyNumber(parseSimpleNumber(data)-parseSimpleNumber(other.data[ind]))
                except ValueError:
                    endTest = True
                result.addElement(entry)
            return result
        elif isinstance(other, numbers.Number):
            return Vector([simplifyNumber(parseSimpleNumber(i)-other) for i in self.data])
        else:
            return False

    def __rsub__(self, other):
        if type(other)==Vector and len(self) == len(other):
            result = Vector()
            endTest = False
            for ind, data in enumerate(self.data):
                try:
                    entry = simplifyNumber(parseSimpleNumber(other.data[ind]-parseSimpleNumber(data)))
                except ValueError:
                    endTest = True
                result.addElement(entry)
            return result
        elif isinstance(other, numbers.Number):
            return Vector([simplifyNumber(other-parseSimpleNumber(i)) for i in self.data])
        else:
            return False

    def __mul__(self,other):
        try:
            complex(other)
        except:
            return False
        result = Vector()
        endTest = False
        for i in self.data:
            try:
                e1 = parseSimpleNumber(self.data)
                entry = e1*e2
                try:
                    entry = parseSimpleNumber(entry)
                except ValueError:
                    pass
            except ValueError:
                try:
                    e2 = parseSimpleNumber(other)
                    entry = e1*e2
                except ValueError:
                    entry = str(e1)+'*'+str(e2)
                    endTest = True
            result.addElement(entry)
        return (result, endTest)

    def __rmul__(self, other):
        return Vector([other*i for i in self.data])

    def addElement(self,newEl):
        self.data.append(newEl)
        return self

    def numTest(self):
        return all([isinstance(i,numbers.Number) for i in self.data])

    def dice_resolve(self):
        numstring = prostring = '('
        for ind, val in enumerate(self.data):
            if type(val) == AdvancedRoll.Die:
                s, result = val.roll()
                prostring += s + ','
                numstring += str(result) + ','
                self.data[ind] = result
            elif type(val) == Vector:
                p,n = val.dice_resolve()
                prostring += p+','
                numstring += n+','
            else:
                prostring += str(val)+','
                numstring += str(val)+','
        prostring = prostring[:-1]+')'
        numstring = numstring[:-1]+')'
        return (prostring,numstring)

    def __str__(self):
        return '('+','.join([str(i) for i in self.data])+')'

def duplicate(x, n):
    def minidup(x):
        if type(x)==AdvancedRoll.OpNode:
            return x.duplicate()
        else:
            return x
    return [minidup(x) for i in range(forceInt(n)-1)] + [x]

class AdvancedRoll:
    operators={
        'd':     {  'type':('in',),   'assoc':'Left',     'diceOp':(True,False), 'pre':12,  'function':lambda x,y: AdvancedRoll.Die(x,y) if isinstance(x,numbers.Number) else x.keep_drop('d',y)},
        '#':     {  'type':('in',),   'assoc':'Right',    'diceOp':(False,None), 'pre':11,  'function':lambda n,x: AdvancedRoll.OpNode('vec', 'vec', duplicate(x,n))                            },
        'dF':    {  'type':('post',), 'assoc':'None',     'diceOp':False,        'pre':10,  'function':lambda x:  AdvancedRoll.Die(x,'F')                                                       },
        'k':     {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':10,  'function':lambda x,y: x.keep_drop('k',y)                                                           },
        'dh':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':11,  'function':lambda x,y: x.keep_drop('d',y,'h')                                                       },
        'kh':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':11,  'function':lambda x,y: x.keep_drop('k',y,'h')                                                       },
        'dl':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':10,  'function':lambda x,y: x.keep_drop('k',y,'l')                                                       },
        'kl':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':10,  'function':lambda x,y: x.keep_drop('d',y,'l')                                                       },
        'r':     {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':10,  'function':lambda x,y: x.reroll(y)                                                                  },
        'r<':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':10,  'function':lambda x,y: x.reroll(y, '<')                                                             },
        'r<=':   {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':10,  'function':lambda x,y: x.reroll(y, '<=')                                                            },
        'r>':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':10,  'function':lambda x,y: x.reroll(y, '>')                                                             },
        'r>=':   {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':10,  'function':lambda x,y: x.reroll(y, '>=')                                                            },
        '!<':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':9,   'function':lambda x,y: x.exploding(y,'<')                                                           },
        '!>':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':9,   'function':lambda x,y: x.exploding(y,'>')                                                           },
        '!<=':   {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':9,   'function':lambda x,y: x.exploding(y,'<=')                                                          },
        '!>=':   {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':9,   'function':lambda x,y: x.exploding(y,'>=')                                                          },
        '~':     {  'type':('pre',),  'assoc':'None',     'diceOp': False,'pre':6,          'function':lambda x:   not(x)                                                                       },
        '!':     {  'type':('special',)                                                                                                                                                         }, 
        '^':     {  'type':('in',),   'assoc':'Right',    'diceOp':(False,False),'pre':4,   'function':lambda x,y: x**y                                                                         },
        '**':    {  'type':('in',),   'assoc':'Right',    'diceOp':(False,False),'pre':4,   'function':lambda x,y: x**y                                                                         },
        '*':     {  'type':('in',),   'assoc':'Left',     'diceOp':(False,False),'pre':3,   'function':lambda x,y: x*y                                                                          },
        '/':     {  'type':('in',),   'assoc':'Left',     'diceOp':(False,False),'pre':3,   'function':lambda x,y: frac(x,y) if type(x) in [int,frac] and type(y) in [int,frac] else x/y        },
        '%':     {  'type':('in',),   'assoc':'Left',     'diceOp':(False,False),'pre':3,   'function':lambda x,y: x%y                                                                          },
        '+':     {  'type':('in',),   'assoc':'Left',     'diceOp':(False,False),'pre':2,   'function':lambda x,y: x+y                                                                          },
        '-':     {  'type':('special',)                                                                                                                                                         },
        '>':     {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':1,   'function':lambda x,y: x.success(y, operator.gt, '>') if type(x) == AdvancedRoll.Die else   x>y     },
        '<':     {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':1,   'function':lambda x,y: x.success(y, operator.lt, '<') if type(x) == AdvancedRoll.Die else   x<y     },
        '==':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':1,   'function':lambda x,y: x.success(y, operator.eq, '==') if type(x) == AdvancedRoll.Die else   x==y   },
        '!=':    {  'type':('in',),   'assoc':'None',     'diceOp':(False,False),'pre':1,   'function':lambda x,y: x.success(y, operator.ne, '!=') if type(x) == AdvancedRoll.Die else   x!=y   },
        '>=':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':1,   'function':lambda x,y: x.success(y, operator.ge, '>=') if type(x) == AdvancedRoll.Die else   x>=y   },
        '<=':    {  'type':('in',),   'assoc':'None',     'diceOp':(True,False), 'pre':1,   'function':lambda x,y: x.success(y, operator.le, '<=') if type(x) == AdvancedRoll.Die else   x<=y   },
        'if':    {  'type':('func',3),                    'diceOp':(False,False,False),     'function': lambda x,y,z: y if x else z                                                             },
        'ceil':  {  'type':('func',1),                    'diceOp':False,                   'function': math.ceil                                                                               },
        'res':   {  'type':('func',1),                    'diceOp':False,                   'function': lambda x: x                                                                             },
        'round': {  'type':('func',1),                    'diceOp':False,                   'function': round                                                                                   },
        'floor': {  'type':('func',1),                    'diceOp':False,                   'function': math.floor                                                                              },
        'max':   {  'type':('func',1),                    'diceOp':False,                   'function':lambda *x: max(x)                                                                        },
        'min':   {  'type':('func',1),                    'diceOp':False,                   'function':lambda *x: min(x)                                                                        },
        'sum':   {  'type':('func',1),                    'diceOp':False,                   'function':lambda *x: sum(x)                                                                        },
        'prod':  {  'type':('func',1),                    'diceOp':False,                   'function':lambda *x: functools.reduce(operator.mul, x, 1)                                          }}

    weirdops = {
        '-':{
            'in':{ 'type':('in',),     'assoc':'Left',     'diceOp':(False,False),'pre':2,  'function': lambda x,y: x-y                                                                          },
            'pre':{  'type':('pre',),  'assoc':'None',     'diceOp': False       ,'pre':6,  'function': lambda x: -x                                                                             }
        },
        '!':{
            'post':{'type':('post',), 'assoc':'None',     'diceOp':True, 'pre':8,  'function': lambda x: x.exploding() if type(x)==AdvancedRoll.Die else math.factorial(x)              },
            'in':{'type':('in',),     'assoc':'None',     'diceOp':(True,False), 'pre':8,  'function': lambda x,y: x.exploding(y)                                                               }
        }
        }

    operatorkeys = tuple(sorted(operators.keys(), key=len, reverse=True))

    class ignoredNumber:
        def __init__(self,n):
            self.num = n
            self.value = 0

        def __str__(self):
            return "//"+str(self.num)+"//"

        def __int__(self):
            return 0

        def __float__(self):
            return float(0)

        def __complex__(self):
            return complex(0)

    class Die:
        def __init__(self, number_of_dice, face):
            self.number_of_dice = number_of_dice
            if type(face) == int:
                self.face = Vector([i for i in range(1, face+1)])
                self.secondTerm = str(face)
            elif type(face) == Vector:
                self.face = face
                self.secondTerm = str(self.face)
            elif face == 'F':
                self.face = Vector([-1,0,1])
                self.secondTerm = 'F'
            else:
                self.face = Vector(face)
                self.secondTerm = str(self.face)
            self.explodingList = None
            self.rerollList = None
            self.keep_dropList = None
            self.successList = None
            self.successSuffix = None
            self.suffix = ''

        def exploding(self, number=None, op=None):
            if not(self.explodingList):
                self.explodingList = set()
            if number:
                if op:
                    self.explodingList.update([i for i in self.face.data if AdvancedRoll.operators[op]['function'](i, number)])
                    self.suffix += '!'+ op + str(number)
                else:
                    self.explodingList.update([number])
                    self.suffix += '!'+ str(number)
            else:
                self.explodingList.update([max(self.face.data)])
                self.suffix += '!'
            return self

        def reroll(self,number,op=None):
            if not(self.rerollList):
                self.rerollList = set()
            if op:
                self.rerollList.update([i for i in self.face.data if AdvancedRoll.operators[op]['function'](i, number)])
                self.suffix = 'r'+op+str(number)
            else:
                self.rerollList.update([number])
                self.suffix = 'r'+str(number)
            return self

        def keep_drop(self,keep_drop, number, typ=None):
            if keep_drop == 'k':
                kd = True
            else:
                kd = False
            self.suffix += keep_drop
            reverse = False
            if typ:
                self.suffix += typ
                if (typ == 'l' and keep_drop =='k') or (typ=='h' and keep_drop =='d'):
                    reverse = True
            self.suffix += str(number)
            self.keep_dropList = (kd, reverse, number)
            return self

        def success(self,number,op,s):
            if not(self.successList):
                self.successList = set()
            if not(self.successSuffix):
                self.successSuffix = ''
            self.successList.update([i for i in self.face.data if op(i, number)])
            self.successSuffix += s + str(number)
            self.suffix += s + str(number)
            return self

        def roll(self):
            results = [random.choice(self.face.data) for i in range(self.number_of_dice)]
            if self.explodingList:
                n = len([i for i in results if i in self.explodingList])
                while n > 0:
                    miniresults = [random.choice(self.face.data) for i in range(n)]
                    n = len([i for i in miniresults if i in self.explodingList])
                    results += miniresults
            if self.rerollList:
                n = 0
                for ind, num in enumerate(results):
                    if num in self.rerollList:
                        results[ind] = AdvancedRoll.ignoredNumber(num)
                        n += 1
                while n>0:
                    miniresults = [random.choice(self.face.data) for i in range(n)]
                    n = 0
                    for ind, num in enumerate(miniresults):
                        if num in self.rerollList:
                            miniresults[ind] = AdvancedRoll.ignoredNumber(num)
                            n += 1
                    results += miniresults
            if self.keep_dropList:
                testResults = [(ind, num) for ind, num in enumerate(results) if type(num)!=AdvancedRoll.ignoredNumber]
                testResults.sort(reverse=self.keep_dropList[1], key = operator.itemgetter(1))
                if self.keep_dropList[0]:
                    if self.keep_dropList[2] < len(testResults):
                        testResults = testResults[-self.keep_dropList[2]:]
                else:
                    if self.keep_dropList[2] >= len(testResults):
                        testResults = []
                    else:
                        testResults = testResults[len(testResults)-self.keep_dropList[2]:]
                testResults = [i[0] for i in testResults]
                for ind, num in enumerate(results):
                    if not(ind in testResults) and type(num) != AdvancedRoll.ignoredNumber:
                        results[ind] = AdvancedRoll.ignoredNumber(num)
            if self.successList:
                n = len([i for i in results if i in self.successList])
                return ('('+'+'.join([str(i) for i in results])+')'+self.successSuffix, n)
            else:
                return ('('+'+'.join([str(i) for i in results])+')', sum([simplifyNumber(complex(i)) for i in results]))

        def __str__(self):
            return str(self.number_of_dice) + 'd' + self.secondTerm + self.suffix

    class OpNode:
        printF = {
            'in': lambda array, sym: sym.join(array),
            'pre': lambda array, sym: sym+array[0],
            'post': lambda array, sym: array[0]+sym,
            'func': lambda array, sym: sym + ','.join(array),
            'vec': lambda array, sym: '(' + sym.join(array) + ')',
            'res': lambda array, sym: '[' + sym.join(array) + ']'
        }

        def __init__(self, op, t, children):
            # op <- i['val'], t <- i['type'][0], children <- args
            self.children = children
            # print('t',t)
            if t in ['vec', 'res']:
                self.function = self.type = self.op = t
                self.function = lambda *x: Vector(list(x))
                self.printType = (t, ',')
            else:
                self.type = t
                self.op = op
                self.function = AdvancedRoll.operators[op]
                if self.function['type'][0] == 'special':
                    self.function = AdvancedRoll.weirdops[op][t]
                self.printType = (self.function['type'][0], op)
                self.function = self.function['function']
            self.__validator_init()

        def __validator_init(self):
            def miniTest(boolean):
                if boolean:
                    return lambda i: (isinstance(i, numbers.Number)) or type(i)==AdvancedRoll.Die or type(i)==Vector
                elif boolean == None:
                    return lambda i: True
                else:
                    return lambda i: (isinstance(i, numbers.Number)) or (type(i)==Vector and i.numTest())
            if self.op == 'res':
                self.validator = lambda x: all(miniTest(False)(i) for i in x)
            elif self.op == 'vec':
                self.validator = lambda x: all(miniTest(True)(i) for i in x)
            else:
                entry = AdvancedRoll.operators[self.op]
                if entry['type'][0] == 'special':
                    entry = AdvancedRoll.weirdops[self.op][self.type]
                if entry['type'][0] in ['pre','post'] or (entry['type'][0] == 'func' and entry['type'][1]==1):
                    self.validator = lambda x: miniTest(entry['diceOp'])(x[0])
                elif entry['type'][0] == 'in':
                    self.validator = lambda array: miniTest(entry['diceOp'][0])(array[0]) and miniTest(entry['diceOp'][1])(array[1])
                else:
                    self.validator = lambda vec: type(vec) == Vector and all([miniTest(val)(vec.data[ind]) for ind,val in enumerate(entry['diceOp'])])

        def __str__(self):
            return self.__stringify(0)

        def __stringify(self,indent):
            string = '\t'*indent + str(self.op)+'\n'
            for i in self.children:
                if type(i) == AdvancedRoll.OpNode:
                    string += i.__stringify(indent+1)
                else:
                    string += '\t'*(indent+1) +str(i)+'\n'
            return string

        def math_resolve(self):
            """
                if self.function == 'vec' and all((isinstance(i, numbers.Number) or type(i)==Vector or type(i)==AdvancedRoll.Die) for i in self.children):
                return (Vector(self.children), None)
                elif self.function == 'res' and all((isinstance(i, numbers.Number) or (type(i)==Vector and i.numTest())) for i in self.children):
                if len(self.children) == 1:
                    return (self.children[0], None)
                else:
                    return (Vector(self.children), None)"""
            if self.validator(self.children):
                if self.op in ['res', 'vec']:
                    return (self.function(*self.children), None)
                elif type(self.children[0]) == Vector:
                    return (self.function(*self.children[0].data), None)
                else: 
                    return (self.function(*self.children), None)
            else:
                dice_test = 0
                outstring = []
                #print('here we almost are', self.children)
                for ind, i in enumerate(self.children):
                    if type(i) == AdvancedRoll.Die or isinstance(i, numbers.Number):
                        outstring.append(str(i))
                        dice_test += 1
                    elif type(i)==AdvancedRoll.OpNode:
                        newi = i.math_resolve()
                        if type(newi[0]) == list:
                            self.children[ind] = Vector(newi[0])
                            for j in newi[0]:
                                outstring.append(str(j))
                        else:
                            s = newi[0]
                            if newi[1]==None:
                                self.children[ind] = s
                            else:
                                if i.type == 'in' and (self.type != 'in' or AdvancedRoll.operators[i.op]['pre'] < AdvancedRoll.operators[self.op]['pre']):
                                    s = '('+s+')'
                            if newi[1]==True:
                                dice_test += 1
                            outstring.append(str(s))
                    elif type(i)==Vector:
                        outstring.append(str(i))
                        dice_test += 1
                if dice_test == len(self.children):
                    dice_test = True
                else:
                    dice_test = False
                #print('here we are', self.printType[0], outstring,self.printType[1], AdvancedRoll.OpNode.printF[self.printType[0]](outstring, self.printType[1]))
                return (AdvancedRoll.OpNode.printF[self.printType[0]](outstring, self.printType[1]), dice_test)

        def dice_resolve(self):
            rollstring = []
            outstring = []
            for ind, i in enumerate(self.children):
                if type(i) == AdvancedRoll.Die:
                    r, o = i.roll()
                    rollstring.append(r)
                    outstring.append(str(o))
                    self.children[ind] = o
                elif isinstance(i, numbers.Number):
                    rollstring.append(str(i))
                    outstring.append(str(i))
                elif type(i) == Vector:
                    r,o = i.dice_resolve()
                    rollstring.append(r)
                    outstring.append(o)
                else:
                    r, o = i.dice_resolve()
                    if i.type == 'in' and (self.type != 'in' or AdvancedRoll.operators[i.op]['pre'] < AdvancedRoll.operators[self.op]['pre']):
                                    r = '('+r+')'
                                    o = '('+o+')'
                    rollstring.append(r)
                    outstring.append(o)
            return (AdvancedRoll.OpNode.printF[self.printType[0]](rollstring, self.printType[1]),AdvancedRoll.OpNode.printF[self.printType[0]](outstring, self.printType[1]))

        def get_opstring(self):
            outstring = []
            for i in self.children:
                if isinstance(i, numbers.Number) or type(i) in [AdvancedRoll.Die, Vector]:
                    outstring.append(str(i))
                else:
                    s = i.get_opstring()
                    #print(AdvancedRoll.operators[i.op], AdvancedRoll.operators[self.op])
                    if i.type == 'in' and (self.type != 'in' or AdvancedRoll.operators[i.op]['pre'] < AdvancedRoll.operators[self.op]['pre']):
                                    s = '('+s+')'
                    outstring.append(s)
            return AdvancedRoll.OpNode.printF[self.printType[0]](outstring, self.printType[1])

        def duplicate(self):
            newChildren = [(i.duplicate() if type(i)==AdvancedRoll.OpNode else i) for i in self.children]
            return AdvancedRoll.OpNode(self.op, self.type, newChildren)

    def __init__(self):
        self.index = 0
        self.roll = ''
        self.lastToken = None
        self.lparen = '(['
        self.rparen = '])'

    def parseString(self,string):
        #get the tree from the string
        tree = self.__infix_to_tree(string)
        #set test to false
        test = False
        #get the original string, and initialize lastmath to that
        outstring = lastmath = tree.get_opstring()
        #print the initial tree and string
        #print('init', outstring, '\n', tree)
        #while we don't get errors
        try:
            #and test isn't equal to None:
            while test != None:
                #while test is false, we can resolve math
                while test == False:
                    #print the tree we're resolving
                    #print(tree)
                    #put the math_resolve into out
                    out = tree.math_resolve()
                    #print('math_resolve',out)
                    #set test equal to out[1]
                    test = out[1]
                    #if the math_resolve returned an opnode, set the tree equal to that node, and test equal to false
                    if type(out[0]) == AdvancedRoll.OpNode:
                        tree = out[0]
                        test = False
                    #Set lastmath equal to out[0]
                    lastmath = str(out[0])
                    #print(out)
                #if test is true, we can resolve dice
                if test == True:
                    #Resolve the relevant dice
                    out = tree.dice_resolve()
                    #print('out',out)
                    #Append to outstring
                    outstring += '='+lastmath+'\n'+out[0]
                    #Set lastmath
                    lastmath = out[1]
                    #Set test back to false
                    test = False
        finally:
            pass
        """except TypeError as e:
            print(e)
            return outstring + '='+lastmath+'\nEnded in TypeError'"""
        #print('children',tree.children)
        #If the last child is a die, roll it.
        if type(out[0]) == AdvancedRoll.Die:
            #print('here')
            r = out[0].roll()
            outstring += '='+str(lastmath)+'\n'+r[0]+'='+str(r[1])
        #If the last child is a Vector, and it has dice in it, resolve those
        elif type(out[0]) == Vector and not(out[0].numTest()):
            #print('there', out[0].data)
            r = out[0].dice_resolve()
            outstring += '='+str(lastmath)+'\n'+r[0]+'='+str(r[1])
        #Otherwise, just finish off the string
        else:
            outstring += '='+str(lastmath)
        #print(tree)
        #print(outstring)
        return outstring

    def __infix_to_tree(self,string):
        #get the postfix tokens
        elements = self.__infix_to_postfix(string)
        #print the elements for debug
        #print('elements',elements)
        #initialize the stack
        stack = []
        #iterate over elements
        for i in elements:
            #if it's a number, append its number form to the stack
            if i['type'][0] == 'num':
                stack.append(simplifyNumber(complex(i['val'])))
            else:
                #if it's an infix, get rid of the last two elements from the stack, and make them the arguments (we don't pop, so as to preserve order)
                if i['type'][0] == 'in':
                    stack, args = stack[:-2], stack[-2:]
                #vectors and reses are the only elements with variable arguments, so remove and get the correct number from the stack (we don't pop, so as to preserve order)
                elif i['type'][0] in ['vec', 'res']:
                    stack, args = stack[:-i['type'][1]], stack[-i['type'][1]:]
                #otherwise, it has one argument, so the args should just be the last element
                else:
                    args = [stack.pop()]
                #append the node with the current value and type, and with args as its children
                #print('i',i)
                stack.append(AdvancedRoll.OpNode(i['val'], i['type'][0], args))
        #in the event that stack is more than one elment, return a vector node of those elements:
        if len(stack) > 1:
            return AdvancedRoll.OpNode('vec','vec',stack)
        #otherwise, just return the first element
        else:
            return stack[0]

    def __infix_to_postfix(self,string):
        #make self.index is 
        self.index = 0
        self.roll = string

        #get our in-loop variables
        output = []
        stack = []
        comma_count = [0,[]]
        lastToken = None
        #while the index less than the length of the roll, iterate
        while self.index < len(self.roll):
            #Get the next token
            t = self.__nextToken(lastToken)
            #if we're a number, append to output
            if t['type'][0] == 'num':
                output.append(t)
            #if we're a func, append to the stack
            elif t['type'][0] == 'func':
                stack.append(t)
            #if we're a left paren:
            elif t['type'][0] == 'lparen':
                #append to the stack
                stack.append(t)
                #add a new number to the comma stack
                comma_count[1].append(0)
            #if we're a right paren
            elif t['type'][0] == 'rparen':
                #while there's still elements in the stack, and they're not a left paren, pop them into output
                while len(stack) > 0 and not(stack[-1]['val'] in '(['):
                    output.append(stack.pop())
                #if there's still elements in the stack, it has to be a left paren, so:
                if len(stack) > 0:
                    #pop it off the stack
                    stack.pop()
                    #and pop the last comma count into v
                    v = comma_count[1].pop()
                #otherwise, we've got a ghost paren, so:
                else:
                    #put the spare comma count into v
                    v = comma_count[0]
                    #reset the spare comma count
                    comma_count[0]=0
                #if we've got commas around
                if v > 0:
                    #if the last token wasn't a seperator, we need to add an extra element to the comma count to get the elements of the list
                    if lastToken['type'][0] != 'sep':
                        v += 1
                    #If we're a bracket
                    if t['val'] == ']':
                        #append a res to the stack
                        output.append({'index':None, 'type':('res', v), 'val':'r'+str(v)})
                    #Otherwise
                    else:
                        #append a vector to the stack
                        output.append({'index':None, 'type':('vec', v), 'val':'v'+str(v)})
                #Reses can have one element, so we have to consider that
                elif t['val'] == ']':
                    output.append({'index':None, 'type':('res', 1), 'val':'r1'})
                #if there's a function at the end of the stack, we need to pop that into output, as this is the argument list for the function
                if len(stack) > 0 and stack[-1]['type'][0] in ['func', 'res']:
                    output.append(stack.pop())
            #if we're a seperator:
            elif t['type'][0] == 'sep':
                #if there's no elements in the comma stack, we add to the spare comma count
                if len(comma_count[1]) == 0:
                    comma_count[0] += 1
                #otherwise, add to the last comma count
                else:
                    comma_count[1][-1] += 1
                #pop elments from the stack to output until the stack is empty or ends with a lparen 
                while len(stack) > 0 and not(stack[-1]['val'] in '(['):
                    output.append(stack.pop())
            #if we're an infix operator
            elif t['type'][0] == 'in':
                #get the operator from the list
                op = AdvancedRoll.operators[t['val']]
                #if the op is overloaded, determine which definition to use
                if op['type'][0] == 'special':
                    op = AdvancedRoll.weirdops[t['val']]['in']
                #while the stack is non-empty and last element is an operator:
                while len(stack) > 0 and stack[-1]['type'][0] in ['in','pre','post','special']:
                    #get the operator that matches the last element of the stack
                    otherop = AdvancedRoll.operators[stack[-1]['val']]
                    #if said operator is special, determine which type to use
                    if otherop['type'][0] == 'special':
                        otherop = AdvancedRoll.weirdops[stack[-1]['val']][stack[-1]['type'][0]]
                    #if the current operator is not associative, the next element is not associative, and the precedents are equal, throw an error
                    if op['assoc'] == 'None' and otherop['assoc']=='None' and op['pre']==otherop['pre']:
                        raise ValueError('Op '+t['val']+' cannot be chained with '+stack[-1]['val']+' at '+str(t['index']))
                    #otherwise, if the precedent of the current operator is lower than the next operator, or if we're left associative and the precedents are equal:
                    elif op['pre'] < otherop['pre'] or (op['assoc'] == 'Left' and op['pre'] == otherop['pre']):
                        #pop the next operator into output.
                        output.append(stack.pop())
                    #otherwise, stop the iteration
                    else:
                        break
                #append the current operator to the stack
                stack.append(t)
            #otherwise, we should be a unary operator
            else:
                #get the operator from the list
                op = AdvancedRoll.operators[t['val']]
                #if the op is overloaded, determine which definition to use
                if op['type'][0] == 'special':
                    op = AdvancedRoll.weirdops[t['val']][t['type'][0]]
                #if we're a postfix operator
                if op['type'][0] == 'post':
                    #while the stack is non-empty and last element is an operator:
                    while len(stack) > 0 and stack[-1]['type'][0] in ['in','pre','post','special']:
                        #get the operator that matches the last element of the stack
                        otherop = AdvancedRoll.operators[stack[-1]['val']]
                        #if said operator is special, determine which type to use
                        if otherop['type'][0] == 'special':
                            otherop = AdvancedRoll.weirdops[stack[-1]['val']][stack[-1]['type'][0]]
                        #if our precendence is less than the other, pop it into output
                        if op['pre'] < otherop['pre']:
                            output.append(stack.pop())
                        #break the iteration
                        else:
                            break
                    #put the current operator into the output
                    output.append(t)
                else:
                    #put the current operator into the stack
                    stack.append(t)
            #save the lastToken for future reference
            lastToken = t
        #while there are leftover elements in the stack
        while len(stack) > 0:
            #if it's a paren
            if stack[-1]['type'][0]=='lparen':
                #pop it off of the stack
                stack.pop()
                #get the last element in the comma stack (it exists, as there was a lparen)
                v = comma_count[1].pop()
                #if there's commas in this paren
                if v > 0:
                    #if the lastToken didn't exist, or it wasn't a seperator, add one to the comma count to get the number of arguments
                    if not(lastToken) or lastToken['type'][0] != 'sep':
                        v += 1
                    #Add a vector to the list
                    output.append({'index':None, 'type':('vec', v), 'val':'v'+str(v)})
            #otherwise, just pop it into the stack
            else:
                output.append(stack.pop())
            #Clear lastToken as we've gone through this process once
            lastToken == None
        #return output
        return output

    def __skipwhitespace(self):
        while self.index+1 < len(self.roll) and self.roll[self.index] in ' \n\t':
            self.index += 1

    def __nextToken(self, lastToken):
        #Skip whitespace and set the startindex
        self.__skipwhitespace()
        startindex = self.index
        #Check if the current char is a digit
        if self.roll[startindex].isdigit():
            #if lastToken is a closing paren, add multiplication rather than the number
            if lastToken and lastToken['type'][0] == 'rparen':
                out = {'index':startindex, 'type':('in',),  'val':'*'}
            else:
                #while we have a digit, keep going
                while self.index<len(self.roll) and self.roll[self.index].isdigit():
                    self.index += 1
                #if the next character is a dot, continue
                if self.index<len(self.roll) and self.roll[self.index] == '.':
                    self.index += 1
                #continue on with the digits after the decimal point
                while self.index<len(self.roll) and self.roll[self.index].isdigit():
                    self.index += 1
                #might be a complex number, so check for j
                if self.index<len(self.roll) and self.roll[self.index] == 'j':
                    self.index += 1
                #make the token
                out = {'index':startindex, 'type':('num',), 'val':self.roll[startindex:self.index]}
        #check if the current char is an operator
        elif self.roll.startswith(AdvancedRoll.operatorkeys,self.index):
            #figure out which operator we're looking at; operatorkeys is sorted from longest to shortest, so simply iterate them in order
            for i in AdvancedRoll.operatorkeys:
                if self.roll.startswith(i,self.index):
                    #if we're -, we're overloaded; have to check which token to return
                    if i=='-':
                        #print()
                        if lastToken and (lastToken['type'][0]=='num' or lastToken['type'][0]=='rparen'):
                            out = {'index':startindex, 'type':('in',),  'val':i}
                        else:
                            out = {'index':startindex, 'type':('pre',), 'val':i}
                        self.index += 1
                    #if we're !, we're overloaded, have to check which token to return
                    elif i=='!':
                        self.__skipwhitespace()
                        if self.index<len(self.roll) and self.roll[self.index].isdigit():
                            out = {'index':startindex, 'type':('in',),  'val':i}
                        else:
                            out = {'index':startindex, 'type':('post',), 'val':i}
                        self.index += 1
                    #otherwise, we know what we're working with; return the relevant token
                    else:
                        self.index += len(i)
                        out = {'index':startindex, 'type':AdvancedRoll.operators[i]['type'], 'val':i}
                    break
        # check if the current char is a left paren
        elif self.roll[startindex] in self.lparen:
            # if we're a left paren, and the last token is a number or a right paren, append multiplication instead of the paren
            if lastToken and (lastToken['type'][0]=='num' or (lastToken['type'][0] == 'rparen')):
                out = {'index':startindex, 'type':('in',),  'val':'*'}
            else:
                self.index += 1
                out = {'index':startindex, 'type':('lparen',), 'val':self.roll[startindex]}
        elif self.roll[startindex] in self.rparen:
            self.index += 1
            out = {'index':startindex, 'type':('rparen',), 'val':self.roll[startindex]}
        # check if we're a comma
        elif self.roll[startindex] == ',':
            self.index += 1
            out = {'index':startindex, 'type':('sep',), 'val':','}
        #otherwise, we've got a problem
        else:
            raise ValueError('Unknown token at '+str(startindex))
        #return
        return out