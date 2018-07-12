.. RPDiscordRewrite documentation master file, created by
   sphinx-quickstart on Mon May 28 13:33:53 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _server_settings:

Settings
============================================

These commands allow authorized users to change the current server''is settings. All of the following commands can only be successfully used by admins or those with a role authorized to do so via the ``botmodroles`` command.

.. _botmodroles:

``botmodroles``
------------------

``botmodroles`` allows the user to change which roles are allowed to change the server''is settings. Regardless of the roles which are have permission given or taken away, users with admin privilages will always be able to change the server''is settings.

Syntax

::

	botmodroles [role_ping]

Optional Fields:
	``[role_ping]``:
		A ping of a given role. If the role does not have bot-changing permissions, a confirmation to add the role to the list of authorized roles will be presented. If the role already has bot-changing permissions, a confirmation to remove the role to the list of authorized roles will be presented.

.. _charsign:

``channel_charsign`` / ``server_charsign``
------------------------------------------

The ``channel_charsign`` and ``server_charsign`` commands allow the user to change the channel''is or server''is charsign, respectively. Charsigns are used to reference character attributes in dice rolls. See the Dice Reference for further information.

Syntax

::

	channel_charsign [new_sign]

OR

::

	server_charsign [new_sign]

Optional Fields:
	``[new_sign]``:
		Character(s) that could be used as a charsign. The only symbol that is not allowed in a charsign is the @ symbol. If the symbol is not currently a charsign for the server, and the server has less than 10 charsigns, a dialogue to add the charsign to the list will be presented. If the symbol is currently a charsign for the server, and it is not the last charsign for the server, a dialogue to remove the charsign will be presented.

		If no argument is provided to ``charsign``, it will list the current charsigns for this server.

.. _charsep:

``channel_charsep`` / ``server_charsep``
----------------------------------------

The ``channel_charsep`` and ``server_charsep`` commands allow the user to change the channel''s or server''s charsep, respectively. Charseps are used to reference character attributes in dice rolls. See the Dice Reference for further information.

Syntax

::

	channel_charsep [new_sep]

OR

::

	server_charsep [new_sep]

Optional Fields:
	``[new_sep]``:
		Character(s) that could be used as a charsep. The only symbol that is not allowed in a charsep is the @ symbol. If the symbol is not currently a charsep for the server, and the server has less than 10 charseps, a dialogue to add the charsep to the list will be presented. If the symbol is currently a charsep for the server, and it is not the last charsep for the server, a dialogue to remove the charsep will be presented.

		If no argument is provided to ``charsep``, it will list the current charseps for this server.

.. _codex:

``channel_codex`` / ``server_codex``
------------------------------------

The ``channel_codex`` and ``server_codex`` commands allow the user to change the channel''s or server''s codex, respectively. The command does not take arguments, instead providing a list of possible codices in a dialogue. A server can only have one codex.

Syntax

::

	channel_codex

OR

::

	server_codex

.. _inline_toggle:

``channel_inline_toggle`` / ``server_inline_toggle``
----------------------------------------------------


The ``channel_inline_toggle`` and ``server_inline_toggle`` commands allow the user to toggle inline rolling on the channel or server, respectively. The command does not take arguments, instead providing a toggle dialogue. By default, the inline roller is disabled.

Syntax

::

	channel_inline_toggle

OR

::

	server_inline_toggle

.. _prefix:

``channel_prefix`` / ``server_prefix``
--------------------------------------

The ``channel_prefix`` and ``server_prefix`` commands allow the user to change the channel''s or server''s prefix, respectively. A prefix is the text used prior to each command.

Syntax

::

	channel_prefix [new_prefix]

OR

::

	server_prefix [new_prefix]

Optional Fields:
	``[new_prefix]``:
		Character(s) that could be used as a prefix. If the symbol is not currently a prefix for the server, and the server has less than 10 prefixes, a dialogue to add the prefix to the list will be presented. If the symbol is currently a prefix for the server, and it is not the last prefix for the server, a dialogue to remove the prefix will be presented.

		If no argument is provided to a ``prefix`` command, it will list the current prefixes for this server.
