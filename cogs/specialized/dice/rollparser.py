import re
import math
import random
import functools
import operator
import numbers
from fractions import Fraction as frac

def singleRoll(s):
    t = s.split('d')
    if t[0]=='':
        t[0]=1
    else:
        t[0]=int(t[0])
    t[1]=int(t[1])
    return [random.randint(1,t[1]) for i in range(t[0])]

def generalSplit(stringIn,markerDict={'-':-1,'+':1,None:1}):
    output = []
    sign = markerDict[stringIn[0]] if stringIn[0] in markerDict else markerDict[None]
    if stringIn[0] in markerDict:
        stringIn = stringIn[1:]
    while len(stringIn) > 0:
        ind = next((i for i, ch in enumerate(stringIn) if ch in markerDict),None)
        if ind == None:
            output.append((sign,stringIn))
            break
        else:
            oneRoll = stringIn[:ind]
            output.append((sign,oneRoll))
            stringIn = stringIn[ind:]
            sign = markerDict[stringIn[0]] if stringIn[0] in markerDict else markerDict[None]
            stringIn = stringIn[1:]
    return output

def generalParse(s,n=1,markerDict={'-':-1,'+':1,None:1}):
    array = generalSplit(s,markerDict)
    output = ([],[])
    for i in range(n):
        strResult = ''
        numResult = 0
        for j in array:
            if j[1].find('d')>-1:
                rollout = singleRoll(j[1])
                strResult += ('-' if j[0]<0 else '+')+'('+'+'.join([str(k) for k in rollout])+')'
                numResult += j[0]*sum(rollout)
            else:
                try:
                    strResult += ('-' if j[0]<0 else '+')+j[1]
                    numResult += j[0]*simplifyNumber(complex(j[1]))
                except:
                    pass
        if strResult[0]=='+':
            strResult[1:]
        output[0].append(strResult)
        output[1].append(numResult)
    return output

class BasicRoll:
    def single_match(self,roll,dicereg):
        output = ""
        if roll.find('#')>-1:
            number, individual = roll.split('#')
            roll = [int(number),individual]
        else:
            roll = [1,roll]
        result = generalParse(roll[1],roll[0])
        string = ','.join([result[0][i]+"="+str(result[1][i]) for i in range(len(result[0]))])
        if string[0]=='+':
            string = string[1:]
        output += '`'+string+'`'
        return output

    async def roll(self,ctx, message):
        regexTest = r"(([1-9]\d*#)?([1-9]\d*)?d[1-9]\d*((\+|-)(([1-9]\d*)?d)?[1-9]\d*)*"
        work = message
        test = re.search(regexTest,message)
        roll = message[test.start():test.end()]
        output = ctx.author.display_name+":\n" + roll + ':'
        while test != None:
            output += single_match(roll,characters,regexTest)
            test = re.search(dicereg,work)
            if test != None:
                work = work[test.end():]
                test = re.search(regexTest,work)
                roll = message[test.start():test.end()]
                output += '\n' + roll + ':'
        await ctx.send(output)