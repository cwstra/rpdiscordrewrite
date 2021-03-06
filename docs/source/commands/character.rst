.. RPDiscordRewrite documentation master file, created by
   sphinx-quickstart on Mon May 28 13:33:53 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _character:

Character
============================================

These commands allow users to create, edit, and view characters for their current server.

.. _newchar:

``newchar``
------------------

``newchar`` creates a new character on the current server.

Syntax

::

	newchar <character_name> [list of attr = value]

Required Fields:
    ``<character_name>``:
    	The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with `\\`.

Optional Fields:
    ``[attr = value]``:
    	Zero or more Name=Value pairs. If Name or Value contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with `\\`.

Examples:
	Empty character creation:
		``newchar Steve``
	Character creation with attributes:
		``newchar Steve Strength=2d6``
	Empty character creation with spaces in name:
		``newchar "Jim the Test Muffin"``
	Character creation with multiple initial attributes:
		``newchar "Jim the Test Muffin" Flavor=Blueberry "Star Rating"=5``

.. _editchar:

``editchar``
------------------

``editchar`` edits an existing character's attributes.

Syntax

::

	editchar [--n new_name] <character_name> <list of attr [= value]>

Required Fields:
    ``<character_name>``:
    	The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with `\\`.

    ``<list of attr [= value]>``:
    	One or more Name=Value attribute pairs, or lone attribute Names; if it's a single attribute name, that attribute will be deleted, if it exists. If Name or Value contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with `\\`.


Options:
    ``[--n new_name]``:
    	Tell the bot to rename your character. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with `\\`.

Examples:
	Example character creation:
		``newchar Steve Strength=2d6``
	Editing character:
		``editchar Steve Strength=3d6``
	Adding new attribute:
		``editchar Steve Wisdom=2d6``
	Deleting attribute:
		``editchar Steve Wisdom``
	Changing character name:
		``editchar --n Jeff Steve``


.. _viewchar:

``viewchar``
------------------

``viewchar`` gets information about an existing character. If the command has no attribute fields, it will return a list of attributes the character has. Otherwise, it will return the values of the requested attributes.

Syntax

::

	viewchar <character_name> [attributes]

Required Fields:
    ``<character_name>``:
    	The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with `\\`.

Optional Fields:
	``[attributes``:
		A space-separated list of attribute names. If a name contains spaces, quotation marks are necessary. If a name contains quotation marks, they must be escaped with `\\`.

Examples:
	Example character creation:
		``newchar Steve Strength=2d6 Wisdom=1d6``
	Viewing character's attribute list:
		``viewchar Steve``
	Viewing one of a character's attributes:
		``viewchar Steve Wisdom``
	Viewing multiple attributes:
		``viewchar Steve Wisdom Strength``

.. _delchar:

``delchar``
------------------

``delchar`` deletes an existing character.

Syntax

::

	delchar <character name>

Required Fields:
    ``<character_name>``:
    	The character's name. If the name contains spaces, quotation marks are necessary. If the name contains quotation marks, they must be escaped with `\\`.

Optional Fields:
	``[attributes``:
		A space-separated list of attribute names. If a name contains spaces, quotation marks are necessary. If a name contains quotation marks, they must be escaped with `\\`.

Examples:
	Example character creation:
		``newchar Steve Strength=2d6``
	Deleting a character:
		``delchar Steve``
