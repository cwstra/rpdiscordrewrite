#!/usr/bin/env python
import discord
import asyncio
import random
import json
import os
import re
import shlex
import math
import numpy
import scipy.special
import asteval
import itertools
import time
import matplotlib.pyplot as plt
from collections import OrderedDict
from fuzzywuzzy import fuzz
from fuzzywuzzy import process

#Adding functions to asteval
aeval = asteval.Interpreter()
aeval.symtable["r_randint"]=random.randint
aeval.symtable["r_randrange"]=random.randrange
aeval.symtable["r_choice"]=random.choice
if hasattr(random,'choices'):
    aeval.symtable["r_choices"]=random.choices
else:
    def f(population, weights=None, k=1):
        """
        Replacement for `random.choices()`, which is only available in Python 3.6+.
        """
        return numpy.random.choice(population, size=k, p=weights)
    aeval.symtable["r_choices"]=random.choices
aeval.symtable["r_shuffle"]=random.shuffle
aeval.symtable["r_sample"]=random.sample
aeval.symtable["r_random"]=random.random

#print(aeval.symtable)
random.seed(os.urandom(64))

#Building myself
myself = {}

with open('settings.json') as data_file:    
    settings = json.load(data_file)
myself['git'] = settings['git_link']
myself['prefix'] = settings['prefix']
myself['charsign'] = settings['charsign']
myself['help'] = settings['help']

with open('characters.json') as data_file:    
    characters = json.load(data_file)
myself['characters'] = characters

with open('statistics.json') as data_file:    
    statistics = json.load(data_file, object_pairs_hook=OrderedDict)
myself['statistics'] = statistics

with open('bookdata.json') as data_file:    
    bookdata = json.load(data_file)
myself['bookdata'] = bookdata


plt.bar([1],[1])
plt.savefig('work.png')
plt.clf()

token = settings['token']

#string concat: 'your %s is in the %s' % (object, location)

client = discord.Client()
   
def gBinom(n,k):
    if n<0:
        return (-1)**k*scipy.special.binom(-n,k)
    else:
        return scipy.special.binom(n,k)

def diceProb(n,t,d):
    def singleRollSum(j):
        return ((-1)**j)*gBinom(n,j)*gBinom(t-d*j-1,n-1)
    return d**(-n)*sum([singleRollSum(j) for j in range(int((t-n)/d)+1)])

@client.event
async def on_ready():
    global myself
    print('Logged in as')
    print(client.user.name)
    print(client.user.id)
    print('------')

@client.event
async def on_message(message):
    global myself
    if message.content.startswith(myself['prefix']):
        print(message.content)
        elif message.content.startswith(myself['prefix']+'statistics'):
            work = shlex.split(message.content)
            if len(work)==1:
                await client.send_message(message.channel, '<@'+message.author.id+'>: You need to look up someone in particular')
            else:
                if "<@" in work[1]:
                    lookup = work[1][1:-1]
                    try:
                        mentioned = message.server.get_member(lookup[1:])
                        mentioned = mentioned.nick
                        if not(mentioned):
                            raise ValueError('No nickname')
                    except:
                        mentioned = await client.get_user_info(lookup[1:])
                        mentioned = mentioned.name
                else:
                    if work[1].lower()=="myself":
                        mentioned = message.author.name
                        lookup = '@'+message.author.id
                    else:
                        mentioned = work[1]
                        if message.server:
                            lookup = "@"+message.server.get_member_named(work[1])
                        else:
                            lookup = "@"+message.channel.recipients[0].id
                if not(lookup):
                    await client.send_message(message.channel, '<@'+message.author.id+">: There doesn't seem to be a member of this server that goes by "+mentioned+'.')
                else:
                    if lookup in myself['statistics']:
                        stats = myself['statistics'][lookup]
                        probs = myself['statistics']['probability']
                        if len(work)==2:
                            await client.send_message(message.channel, '<@'+message.author.id+'>: Results for '+mentioned+"'s rolls.\n")
                            for i,j in stats.items():
                                n = j['rolls']
                                t = [[],[]]
                                for k,l in j.items():
                                    if k!='rolls':
                                        t[0].append(int(k))
                                        t[1].append(l/n)
                                if len(t[0])>100:
                                    plt.plot(t[0],t[1],'b-')
                                else:
                                    plt.bar(t[0],t[1])
                                plt.savefig('work.png')
                                plt.clf()
                                await client.send_file(message.channel, 'work.png',content="Recorded "+i)
                                if 'image' in probs[i]:
                                    await client.send_file(message.channel, probs[i]['image'],content="Expected "+i)
                                else:
                                    t = [[],[]]
                                    for k,l in probs[i].items():
                                        t[0].append(int(k))
                                        t[1].append(l)
                                    if len(t[0])>100:
                                        plt.plot(t[0],t[1],'b-')
                                    else:
                                        plt.bar(t[0],t[1])
                                    plt.savefig('Probabilities/'+i+'.png')
                                    plt.clf()
                                    await client.send_file(message.channel,'Probabilities/'+i+'.png',content="Expected "+i)
                                    probs[i]['image']='Probabilities/'+i+'.png'
                                    with open('statistics.json','w') as data_file:    
                                        json.dump(myself['statistics'],data_file,indent=4)
                        else:
                            await client.send_message(message.channel, '<@'+message.author.id+'>: Results for '+mentioned+"'s rolls of "+work[2]+".\n")
                            if work[2] in stats:
                                s="Recorded "+work[2]+":\n"
                                n = stats[work[2]]['rolls']
                                t = [[],[]]
                                for i,j in stats[work[2]].items():
                                    if i!='rolls':
                                        t[0].append(int(i))
                                        t[1].append(j/n)
                                plt.bar(t[0],t[1])
                                plt.savefig('work.png')
                                plt.clf()
                                await client.send_file(message.channel, 'work.png',content="Recorded "+work[2])
                                if 'image' in probs[work[2]]:
                                    await client.send_file(message.channel ,probs[work[2]]['image'],content="Expected "+work[2])
                                else:
                                    t = [[],[]]
                                    for i,j in probs[work[2]].items():
                                        t[0].append(int(i))
                                        t[1].append(j)
                                    plt.bar(t[0],t[1])
                                    plt.savefig('Probabilities/'+i+'.png')
                                    plt.clf()
                                    await client.send_file(message.channel,'Probabilities/'+i+'.png',content="Expected "+work[2])
                                    probs[work[2]]['image']='Probabilities/'+work[2]+'.png'
                                    with open('statistics.json','w') as data_file:    
                                        json.dump(myself['statistics'],data_file,indent=4)
                            else:
                                await client.send_message(message.channel, '<@'+message.author.id+'>: '+mentioned+" doesn't seem to have ever rolled "+work[2]+".")
                    else:
                        await client.send_message(message.channel, '<@'+message.author.id+'>: '+mentioned+" doesn't have any rolls on record")
        elif message.content.startswith(myself['prefix']+'newchar'):
            work = shlex.split(message.content)
            if len(work)==1:
                await client.send_message(message.channel, 'newchar usage: '+myself['prefix']+'newchar [character name] <attributes>')
            else:
                char = work[1]
                if char in myself['characters']:
                    await client.send_message(message.channel, "That character already exists; use editattr to change attributes.")
                else:
                    myself['characters'][char]={'__creator__':message.author.id,'__hidden__':False}
                    work = work[2:]
                    while len(work)>0:
                        if work[0].find('=')!=-1:
                            w = work[0].split('=')
                            if not(w[0] in ['__creator__']) and w[0].find('!')!=0:
                                myself['characters'][char][w[0]]=w[1]
                        elif work[0].find(':')!=-1:
                            w = work[0].split('=')
                            if not(w[0] in ['__creator__']) and w[0].find('!')!=0:
                                myself['characters'][char][w[0]]=w[1]
                        work = work[1:]
                    with open('characters.json','w') as data_file:
                        json.dump(myself['characters'],data_file,indent=4)
                    await client.send_message(message.channel, char + " created! Use "+myself['prefix']+"editattr to change attributes, or "+myself['prefix']+"viewchar to view.")
        elif message.content.startswith(myself['prefix']+'viewchar'):
            work = shlex.split(message.content)
            if len(work)==1:
                await client.send_message(message.channel, 'viewchar usage: '+myself['prefix']+'viewchar [character name] <attribute>')
            else:
                char = work[1]
                if char in myself['characters'] and (not(myself['characters'][char]['__hidden__']) or message.author.id == myself['characters'][char]['__creator__'] or message.author.server_permissions.administrator):
                    if len(work)==3:
                        if work[2] in myself['characters'][char]:
                            await client.send_message(message.channel, "%s's %s attribute: %s"%(char,work[2],myself['characters'][char][work[2]]))
                        else:
                            await client.send_message(message.channel, "%s doesn't have an attribute called %s."%(char,work[2]))
                    else:
                        await client.send_message(message.channel, char+" is a character created by <@"+ myself['characters'][char]['__creator__'] +"> with the following attributes:") 
                        attrs = []
                        for i,j in myself['characters'][char].items():
                            if not(i in ["__creator__",'__hidden__']):
                                attrs.append(i)
                        if attrs==[]:
                            attrs="None"
                        elif len(attrs)==1:
                            attrs = attrs[0]
                        elif len(attrs)==2:
                            attrs = attrs[0]+' and '+attrs[1]
                        else:
                            attrs = ', '.join(attrs[:-1])+', and '+attrs[-1]
                        await client.send_message(message.channel, attrs)    
                else:
                    await client.send_message(message.channel, "That character does not exist, or is hidden.") 
        elif message.content.startswith(myself['prefix']+'hidechar'):
            work = shlex.split(message.content)
            if len(work)==1:
                await client.send_message(message.channel, 'hidechar usage: '+myself['prefix']+'hidechar [character name]')
            else:
                char = work[1]
                if char in myself['characters'] and (message.author.id == myself['characters'][char]['__creator__'] or message.author.server_permissions.administrator):
                    myself['characters'][char]['__hidden__'] = not(myself['characters'][char]['__hidden__'])
                    if myself['characters'][char]['__hidden__']:
                        await client.send_message(message.channel, char + " hidden.")
                    else:
                        await client.send_message(message.channel, char + " unhidden.")
                else:
                    await client.send_message(message.channel, "That character does not exist, or does not belong to you.") 
        elif message.content.startswith(myself['prefix']+'delchar'):
            work = shlex.split(message.content)
            if len(work)==1:
                await client.send_message(message.channel, 'delchar usage: '+myself['prefix']+'delchar [character name]')
            else:
                char = work[1]
                if char in myself['characters']:
                    if message.author.id == myself['characters'][char]['__creator__']:
                        await client.send_message(message.channel, 'Are you sure you want to delete your character %s? Use %sYes or %sNo'%(char,myself['prefix'],myself['prefix']))
                        myself['waiting'][message.author.id]=['delete',char]
                    elif message.author.server_permissions.administrator:
                        await client.send_message(message.channel, "Are you sure you want to delete %s's character %s? Use %sYes or %sNo"%(myself['characters'][char]['__creator__'],char,myself['prefix'],myself['prefix']))
                        myself['waiting'][message.author.id]=['delete',char]
                    else:
                        await client.send_message(message.channel, char+' belongs to <@'+myself['characters'][char]['__creator__']+'>. Only the creator or an administrator can delete them.')
                else:
                    await client.send_message(message.channel, "That character does not exist.") 
        elif message.content.startswith(myself['prefix']+'editattr'):
            work = shlex.split(message.content)
            if len(work)==1:
                await client.send_message(message.channel, 'editattr usage: '+myself['prefix']+'editattr [character name] <attributes>')
            else:
                char = work[1]
                if char in myself['characters']:
                    if message.author.id == myself['characters'][char]['__creator__'] or message.author.server_permissions.administrator:
                        work = work[2:]
                        while len(work)>0:
                            if work[0].find('!')==0:
                                w = work[0][1:]
                                if w in myself['characters'][char]:
                                    del myself['characters'][char][w]
                            elif work[0].find('=')!=-1:
                                w = work[0].split('=')
                                if not(w[0] in ["__creator__",'__hidden__']):
                                    print(w)
                                    myself['characters'][char][w[0]]=w[1]
                            elif work[0].find(':')!=-1:
                                w = work[0].split(':')
                                if not(w[0] in ["__creator__",'__hidden__']):
                                    print(w)
                                    myself['characters'][char][w[0]]=w[1]
                            work = work[1:]
                        with open('characters.json','w') as data_file:
                            json.dump(myself['characters'],data_file,indent=4)
                        await client.send_message(message.channel, char + " edited!")
                    else:
                        await client.send_message(message.channel, char+' belongs to <@'+myself['characters'][char]['__creator__']+'>. Only the creator or an administrator can edit them.')
                else:
                    await client.send_message(message.channel, "That character does not exist; use newchar to create them.") 
sleeptime=0
sleeptimes=[5,5,30,60,60*5]
while True:
    try: 
        client.loop.run_until_complete(client.start(token))
    except discord.errors.LoginFailure as e:
        print("The client failed to login with error: "+e.args[0])
        if e.args[0]=="Improper token has been passed.":
            print("Did you put a valid token into settings.json?")
            break
    except BaseException as e:
        print("Something went wrong:") 
        print("Error: "+e.args[0])
        time.sleep(sleeptimes[sleeptime])
        if sleeptime<4:
            sleeptime+=1