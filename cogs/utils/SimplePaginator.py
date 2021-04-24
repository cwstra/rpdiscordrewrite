import discord
import asyncio
import concurrent.futures

async def pager(entries, chunk: int):
    for x in range(0, len(entries), chunk):
        yield entries[x:x + chunk]


class SimplePaginator:

    __slots__ = ('entries', 'extras', 'title', 'description', 'colour', 'footer', 'length', 'prepend', 'append',
                 'fmt', 'timeout', 'ordered', 'freeze', 'controls', 'controller', 'pages', 'current', 'previous', 'eof', 'base',
                 'names')

    def __init__(self, **kwargs):
        self.entries = kwargs.get('entries', None)
        self.extras = kwargs.get('extras', None)

        self.title = kwargs.get('title', None)
        self.description = kwargs.get('description', None)
        self.colour = kwargs.get('colour', 0xffd4d4)
        self.footer = kwargs.get('footer', None)

        self.length = kwargs.get('length', 10)
        self.prepend = kwargs.get('prepend', '')
        self.append = kwargs.get('append', '')
        self.fmt = kwargs.get('fmt', '')
        self.timeout = kwargs.get('timeout', 90)
        self.ordered = kwargs.get('ordered', False)
        self.freeze = kwargs.get('freeze', True)

        self.controller = None
        self.pages = []
        self.names = []
        self.base = None

        self.current = 0
        self.previous = 0
        self.eof = 0

        self.controls = {'⏮': 0.0, '◀': -1, '⏹': 'stop', '⏸️': 'freeze',
                         '▶': +1, '⏭': None}


    async def indexer(self, ctx, ctrl):
        if ctrl == 'stop':
            ctx.bot.loop.create_task(self.stop_controller(ctx, self.base))
            return False
        if ctrl == 'freeze':
            ctx.bot.loop.create_task(self.stop_controller(ctx, self.base, True))
            return False
        if isinstance(ctrl, int):
            self.current += ctrl
            if self.current > self.eof or self.current < 0:
                self.current -= ctrl
            return True
        self.current = int(ctrl)
        return True

    async def reaction_controller(self, ctx):
        bot = ctx.bot
        author = ctx.author

        self.base = await ctx.send(embed=self.pages[0])

        if len(self.pages) == 1:
            await self.base.add_reaction('⏹')
        else:
            for reaction in self.controls:
                try:
                    await self.base.add_reaction(reaction)
                except discord.HTTPException:
                    return

        def check(raw):
            if str(raw.emoji) not in self.controls.keys():
                return False
            elif raw.user_id == bot.user.id or raw.message_id != self.base.id:
                return False
            elif raw.user_id != author.id:
                return False
            return True

        while True:
            done, pending = await asyncio.wait([
                bot.wait_for('raw_reaction_add',check=check,timeout=self.timeout),
                bot.wait_for('raw_reaction_remove',check=check,timeout=self.timeout)
            ], return_when=asyncio.FIRST_COMPLETED)
            for future in pending:
                future.cancel()
            try:
                bot.logger('try')
                raw_action = done.pop().result()
            except (asyncio.TimeoutError, concurrent.futures.TimeoutError):
                bot.logger('except')
                for future in done:
                    future.exception()
                return ctx.bot.loop.create_task(self.stop_controller(ctx, self.base, self.freeze))


            control = self.controls.get(str(raw_action.emoji))

            self.previous = self.current
            continue_loop = await self.indexer(ctx, control)

            if not(continue_loop):
                break

            if self.previous == self.current:
                continue

            try:
                await self.base.edit(embed=self.pages[self.current])
            except KeyError:
                pass

    async def stop_controller(self, ctx, message, freeze=False):
        try:
            curmsg = await message.channel.fetch_message(message.id)
            for i in curmsg.reactions:
                if i.me:
                    await message.remove_reaction(i, ctx.me)
            if not(freeze):
                await message.edit(content = 'Paginator Deleted', embed = None)
        except discord.HTTPException:
            pass

        try:
            self.controller.cancel()
        except Exception:
            pass

    def formmater(self, chunk):
        return '\n'.join(f'{self.prepend}{self.fmt}{value}{self.fmt[::-1]}{self.append}' for value in chunk)

    async def paginate(self, ctx):
        if self.extras:
            self.pages = [p for p in self.extras if isinstance(p, discord.Embed)]

        if self.entries:
            chunks = [c async for c in pager(self.entries, self.length)]

            for index, chunk in enumerate(chunks):
                page = discord.Embed(title=f'{self.title} - {index + 1}/{len(chunks)}', color=self.colour)
                page.description = self.formmater(chunk)

                if self.footer:
                    page.set_footer(text=self.footer)
                self.pages.append(page)

        if not self.pages:
            raise Exception('There must be enough data to create at least 1 page for pagination.')

        self.eof = float(len(self.pages) - 1)
        self.controls['⏭'] = self.eof
        self.controller = ctx.bot.loop.create_task(self.reaction_controller(ctx))
