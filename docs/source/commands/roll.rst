.. RPDiscordRewrite documentation master file, created by
   sphinx-quickstart on Mon May 28 13:33:53 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _reference:

Reference
============================================

Reference commands are used to look up data from a server's codex. A server's codex is set at initialization, or via the :ref:```codex`` [codex]` command.

.. _ref:

``ref``
--------------------------------------------

``ref`` looks through the server's codex to try to find and return a good match for the given search term.

Syntax

::

	ref <request>

Required Fields:
	``<request>``:
		A database search query. If a strong response is found, it will be returned. Otherwise, an error message will be returned.

.. _schema:

``schema``
--------------------------------------------

``schema`` serves two functions:

	* If called without an argument, ``schema`` will list the categories of the current codex, along with their structure, as described in :ref:`request_syntax`.

	* If called with an argument, the argument will be parsed as a partial request, and then return all subentries or partial fields (as relevant) falling under that request.

Syntax

::

	schema [request]

Optional Fields:
	``[request]``:
		A database search query.

.. _top:

``top``
--------------------------------------------

``top`` returns a number of the top results in the server's codex for a given search term.

Syntax

::

	top [number] <request>

Required Fields:
	``<request>``:
		A database search query.

Optional Fields:
	``[number]``:
		The number of top results to show for the given request. The default number is five; if you request more than five results, the subsequent message will be sent in a PM.

.. _request_syntax:

Request Syntax
--------------------------------------------

A codex is composed of categories, of which there are two varieties: `flat` and `deep`.

The entries in a `flat` category are straightforward. They are composed of a `key` and a `value`; when you issue a request, your request is compared to all keys, and, if there is a strong match, the corresponding `value` is returned.

Entries in a `deep` category are more complex. Any entry in a `deep` category has a given number of subfields, which may, in turn, also have subfields, and so on.

There are two types of subfield a `deep` category may have:

	Subentries:
		Subentries in a `deep` category are completely independent `values` that, for one reason or another, are listed under the same `key` in the category. These subentries are, in turn, each listed under their own `key` in the main entry. There is generally a default `key` for these subentries; if no subentry `key` is provided in a request, the subentry matching the default `key` is returned.
	Partial Fields:
		Partial Fields in a `deep` category are individual parts of an entry that may be shown or hidden, depending on the request. These `keys` may be provided alone or in a comma delimited list; the result will then contain only the requested partial fields. If, on the other hand, no field `key` is given, all partial fields will be returned, when possible.
