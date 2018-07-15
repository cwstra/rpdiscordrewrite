import sys
args = sys.argv
if len(args) > 1:
    if 'print' in args:
        logfun = print
    else:
        import logging
        logging.basicConfig(filename='logs/tes.log', format='%(asctime)s %(message)s')
        logger = logging.getLogger('Logger')
        print(args, type(args), args[1])
        logger.setLevel(args[1])
        logfun = logger.info
else:
    import logging
    logging.basicConfig(filename='logs/tes.log', format='%(asctime)s %(message)s')
    logger = logging.getLogger('Logger')
    logger.setLevel('INFO')
    logfun = logger.info

import json 
logfun('json imported')
import asyncio
logfun('asyncio imported')
import asyncpg
logfun('asyncpg imported')

import discord
logfun('discord imported')
from discord.ext import commands
logfun('commands imported')

