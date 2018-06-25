.. RPDiscordRewrite documentation master file, created by
   sphinx-quickstart on Mon May 28 13:33:53 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _roll:

Roll
============================================

The roll command, as might be expected, is the command to use the command-based dice roller.

``roll``
--------------------------------------------

``roll`` uses the command-based dice roller to resolve a dice expression.

Syntax

::

	roll <roll_expression>

Required Fields:
	``<roll_expression>``:
		A roll expression. See Dice Reference for more details.

.. _stats:

``stats`` / ``statistics``
--------------------------------------------

``stats`` and ``statistics`` generates a chart from stored dice rolling statistics. If a request generates at most one chart, the results will be displayed in chat; otherwise, they will sent via DM.

Syntax

::

	stats [request]

Optional Fields: 
	``[request]``
        A statistics request. Syntax for requesting various statistics are as follows:
            
            * If the ``[request]`` field is absent, all stats for the server will be displayed, along with the expected distribution for each.

            * If the ``[request]`` field has the form ``XdY``, the server-wide statistics for all rolls of the form ``XdY`` will be displayed, along with the expected distribution.
            
            * If the ``[request]`` field has the form ``<name>:XdY``, the statistics for all rolls of the form ``XdY`` from the user with username ``<name>`` for the server will be displayed, along with the expected distribution.

            * If the ``[request]`` does not match any of the above forms, the statistics for all roll from the user with name ``[request]`` for the server will be displayed, along with the expected distribution.

``text_stats``
--------------------------------------------

``text_stats`` returns the raw stored dice rolling statistics as a JSON via DM.

Syntax

::

	text_stats [request]

Optional Fields: 
	``[request]``
        A statistics request. Syntax for requesting various statistics are as follows:
            
            * If the ``[request]`` field is absent, all stats for the server will be sent.

            * If the ``[request]`` field has the form ``XdY``, the server-wide statistics for all rolls of the form ``XdY`` will be sent.
            
            * If the ``[request]`` field has the form ``<name>:XdY``, the statistics for all rolls of the form ``XdY`` from the user with username ``<name>`` will be sent.

            * If the ``[request]`` does not match any of the above forms, the statistics for all roll from the user with name ``[request]`` will be sent.
