.. RPDiscordRewrite documentation master file, created by
   sphinx-quickstart on Mon May 28 13:33:53 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _command_functions:

Command Functions
============================================

Below is the large list of command functions available for use in the dice parser, along with instructions on how to read the documentation. (Well, there will be, soon enough. :P)

.. command_format:

Format
============================================

Functions are displayed as follows:

.. _sample_function:

Sample Function Name
--------------------------------------------

A brief description of the function will be displayed here.

Syntax:

::

	Basic usage syntax will be in a bar like this

Operator Type:
Operators can be infix, prefix, postfix, or functional. While the distinction should be obvious from the syntax, their type is listed here for clarity.

Precedence:
Non-functional operators have a precedence, which dictates the order of operations for function resolution. Higher precedence operators are resolved first.

Operands:

* The first operand's function will be described here
	- Valid Types:
		* Valid input types will be listed in a sublist,
		* like this one. Sometime, additional restrictions will also be listed
* Then we'll move onto the second operand, and so on
	- Valid Types:
		* If a function is overloaded, some additional information
		* may be displayed in these sublists

.. _dice_functions:

Dice Functions
============================================

The following commands allow the creation or manipulation of dice.

.. _d_function:

Dice Creation ( ``d`` )
--------------------------------------------

The `d` function creates new dice pools.

Syntax:

::

	XdY

OR

::

	dY

Operator Type: Prefix or Infix

Precedence: 12

Operands:

* ``X``: pool number. The resulting die pool will roll this many dice.

	- Valid Types:

		* Integers: Must be greater than zero

		* Dice: Will be evaluated, then, if the result is a positive integer, it will be used as in the above case.


* ``Y``: dice face. This represents the faces present on each die.

	- Valid Types:

		* Integers: Must be greater than zero. This will return dice with faces from one to ``Y``.

		* Dice: Will be evaluated, then, if the result is a positive integer, it will be used as in the above case.

		* Vectors: Must contain only numbers. This will return dice whose faces are the elements of the vector.

.. _fudge_dice:

Fudge Dice ( ``dF`` )
--------------------------------------------

The ``dF`` function creates a Fudge dice pool

Syntax:

::

 	XdF

Operator Type: Postfix

Precedence: 11

Operands:

* ``X``: pool number. The resulting die pool will roll this many fudge dice.

	- Valid Types:

		* Integers: Must be greater than zero

		* Dice: Will be evaluated, then, if the result is a positive integer, it will be used as in the above case.

.. _keep_drop:

Keep/Drop Functions ( ``kh``, ``kl``, ``dh``, ``dl``)
-----------------------------------------------------

The ``kh``, ``kl``, ``dh``, and ``dl`` functions all serve similar functions; they modify an existing dice pool to (k)eep or (d)rop either (h)igh or (l)ow dice.

Syntax:

::

	D{ kh | kl | dh | dl }N

Operator Type: Infix

Precedence: 10

* ``D``: The dice pool to modify.

	- Valid Types:

		* Dice

* ``N``: The number of high or low dice to keep or drop.

	- Valid Types:

		* Integers: Must be greater than zero

		* Dice: Will be evaluated, then, if the result is a positive integer, it will be used as in the above case.

.. _reroll:

Reroll Functions ( ``r`` and friends)
--------------------------------------------

The ``r``, ``r<``, ``r<=``, ``r>``, ``r>=``, ``rIn``, and ``rOut`` functions all modify an existing dice pool, passed as a left argument, to reroll dice that meet a specific predicate:

* ``r`` rerolls dice that are equal to its right-hand argument

* ``r<`` rerolls dice that are less than its right-hand argument

* ``r<=`` rerolls dice that are less than or equal to its right-hand argument

* ``r>`` rerolls dice that are greater than its right-hand argument

* ``r>=`` rerolls dice that are greater than or equal to its right-hand argument

* ``rIn`` rerolls dice that are greater than or equal to the first element of its right hand argument AND less than or equal to the second element of its right-hand argument.

* ``rOut`` rerolls dice that are less than or equal to the first element of its right hand argument OR less than or equal to the second element of its right-hand argument.

If a single dice pool is modified multiple times, rolls that meet the outer-most provided stipulation will be rerolled.

Syntax:

::

	D{ r | r< | r<= | r> | r>= | rIn | rOut }N

Operator Type: Infix

Precedence: 10

* ``D``: The dice pool to modify.

	- Valid Types:

		* Dice

* ``N``: The number to test against.

	- Valid Types:

		- ``rIn`` and ``rOut``

			* Vector: ``rIn`` and ``rOut`` both take a two-element vector of real numbers.

		- Remaining functions

			* Numbers: If the function is ``r``, ``N`` may be any number. Otherwise, ``N`` must be a real number.

			* Dice: Will be evaluated, then, if the result is a valid number, it will be used as in the above case.

.. _exploding:

Exploding Functions ( ``!`` and friends)
--------------------------------------------

The ``!``, ``!<``, ``!<=``, ``!>``, and ``!>=`` functions all modify an existing dice pool, passed as a left argument, to have dice explode when they meet a specific predicate:

* ``!`` can either be an infix or a postfix operator. If it has a right argument, ``!`` will cause dice to explode when equal to its right argument; otherwise, it will cause dice to explode when equal to the maximum possible face of the die.

* ``!<`` causes dice whose results are less than its right-hand argument to explode.

* ``!<=`` causes dice whose results are less than or equal to its right-hand argument to explode.

* ``!>`` causes dice whose results are greater than its right-hand argument to explode.

* ``!>=`` causes dice whose results are greater than or equal to its right-hand argument to explode.

* ``!In`` causes dice whose results are greater than or equal to the first element of its right hand argument AND less than or equal to the second element of its right-hand argument to explode.

* ``!Out`` causes dice whose results are less than or equal to the first element of its right hand argument OR less than or equal to the second element of its right-hand argument to explode.

If a single dice pool is modified multiple times, rolls that meet *any* of the provided stipulations will explode.

Syntax:

::

	D{ ! | !< | !<= | !> | !>= | !In | !Out }N

Operator Type: Infix

Precedence: 10

* ``D``: The dice pool to modify.

	- Valid Types:

		* Dice

* ``N``: The number to test against.

	- Valid Types:

		- ``!In`` and ``!Out``

			* Vector: ``!In`` and ``!Out`` both take a two-element vector of real numbers.

		- Remaining functions

			* Numbers: If the function is ``!``, ``N`` may be any number. Otherwise, ``N`` must be a real number.

			* Dice: Will be evaluated, then, if the result is a valid number, it will be used as in the above case.

.. _success_functions:

Success / Comparator Functions ( ``==`` and friends)
----------------------------------------------------

The ``==``, ``/=``, ``<``, ``<=``, ``>``, ``>=``, ``In``, and ``Out`` operators serve two functions:

	* If their left-hand argument is a die, it will be changed into a success-based pool: all dice that meet the function's specific qualification will be counted as a success, and otherwise as a failure.

	* If their left-hand argument is not a die, the operator's arguments will be compared based on the function, and turned into a boolean.

The operator's qualifications are:

* ``==`` tests the operands for equality.

* ``/=`` tests the operands for inequality.

* ``<`` takes a real number on the right. If the first operand is less than the second, it returns ``True``; otherwise, it returns ``False``.

* ``<=`` takes a real number on the right. If the first operand is less than or equal to the second, it returns ``True``; otherwise, it returns ``False``.

* ``>`` takes a real number on the right. If the first operand is greater than the second, it returns ``True``; otherwise, it returns ``False``.

* ``>=`` takes a real number on the right. If the first operand is greater than or equal to the second, it returns ``True``; otherwise, it returns ``False``.

* ``In`` takes a vector containing two real numbers on the right. If the first operand is within the inclusive range of the vector's elements, it returns ``True``; otherwise, it returns ``False``.

* ``Out`` takes a vector containing two real numbers on the right. If the first operand is not within the inclusive range of the vector's elements, it returns ``True``; otherwise, it returns ``False``.

If a single dice pool is modified multiple times, rolls that meet *any* of the provided stipulations will explode.

Syntax:

::

	X{ == | /= | < | <= | > | >= | In | Out }Y

Operator Type: Infix

Precedence: 1

* ``X``

	- Valid Types:

		* Dice: If a die is the left operand, its results will be deemed a success if they would return true under the relevant function.

		* Non-Dice: None-dice will simply be compared to the right operand, as per the above rules. ``==`` and ``/=`` can take any literal, while the other functions can only take real numbers.

* ``Y``: The number to test against.

	- Valid Types:

		* Dice: If a die is the right operand, it will be evaluated, and its result passed as the function's right argument.

		* Non-Dice: None-dice will be used to judge the results of the die passed as the left operand, or simply compared to the left operand, as per the above rules. ``==`` and ``/=`` can take any literal, while the other functions can only take real numbers.

.. _logical_functions:

Logical Functions
============================================

The following commands are all logical operators, acting on their operands based on their Truthy-ness.

.. _bool:

Forced Boolean ( ``bool``)
--------------------------------------------

The ``bool`` function transforms its argument to its logical equivalent: Truthy values will be converted to ``True``, and Falsey values will be converted to ``False``.

Syntax:

::

	bool(X)

Operator Type: Functional

* ``X``: The value to transform to a boolean.

	- All types can be passed to ``bool``. Dice will be evaluated prior to the operator being called.

.. _not:

Logical Negation ( ``~``, ``not``)
--------------------------------------------

The ``~`` and ``not`` functions transform their arguments to their logical opposites: Truthy values will be converted to ``False``, and Falsey values will be converted to ``True``.

Syntax:

::

	~X

OR

::

	not(X)

Operator Type: Prefix ( ``~``) or Functional ( ``not``)

Precedence (for Prefix): 6

* ``X``: The value to logically negate.

	- All types can be passed to ``~`` and ``not``. Dice will be evaluated prior to the operator being called.

.. _and:

Logical Conjunction ( ``&&``, ``and``)
--------------------------------------------

The ``&&`` and ``and`` functions take the logical conjunction of their arguments. In other words, if both of their operands are Truthy, they return ``True``; otherwise, they return ``False``

Syntax:

::

	X&&Y

OR

::

	X and Y

Operator Type: Infix

Precedence: 3

* ``X`` and ``Y``:

	- All types can be passed to ``&&`` and ``and``. Dice will be evaluated prior to the operator being called.

.. _or:

Logical Disjunction ( ``||``, ``or``)
--------------------------------------------

The ``||`` and ``or`` functions take the logical disjunction of their arguments. In other words, if both of their operands are Falsey, they return ``False``; otherwise, they return ``True``

Syntax:

::

	X||Y

OR

::

	X or Y

Operator Type: Infix

Precedence: 2

* ``X`` and ``Y``:

	- All types can be passed to ``||`` and ``or``. Dice will be evaluated prior to the operator being called.

.. _if:

If Function ( ``if``)
--------------------------------------------

The ``if`` function is the primary use for booleans. It takes three arguments: A boolean (or argument to be interpreted as a boolean), followed by two statements. If the first argument is Truthy, ``if`` returns the second argument; otherwise, it returns the third argument.

Syntax:

::

	if(X,Y,Z)

Operator Type: Functional

* ``X``:

	- All types can be passed to ``X``, to be read as Truthy or Falsey. Dice will be evaluated prior to the operator being called.

* ``Y`` and ``Z``:

	- All types can be passed to ``Y`` and ``Z``.

.. _numeric_functions:

Numeric Functions
============================================

The following commands all perform standard operations on numeric values.

.. _exponentiation:

Exponentiation ( ``^``, ``**``)
--------------------------------------------

The ``^`` and ``**`` functions raise their left-hand argument to the power of their right-hand argument.

Syntax:

::

	X^Y

OR

::

	X**Y

Operator Type: Infix

Precedence: 4

* ``X``: The value to logically negate.

	- All types can be passed to ``~`` and ``not``. Dice will be evaluated prior to the operator being called.

.. _multiplication:

Multiplication ( ``*``)
--------------------------------------------

The ``*`` function performs multiplication, as follows:

* If both arguments are numbers, they will be multiplied. If either is a float, floating point arithmetic will be used; otherwise, values will be exactly calculated.

* If one argument is a number and the other is a numeric vector, each element in the vector will be multiplied by the single element.

* If both arguments are numeric vectors of the same length, corresponding elements will be multiplied.

Syntax:

::

	X*Y


Operator Type: Infix

Precedence: 3

* ``X`` and ``Y``.

	- Numbers

	- Numeric Vectors: If both arguments are vectors, they must be of the same length.

.. _division:

Divison ( ``/``)
--------------------------------------------

The ``/`` function performs divison on numbers.

Syntax:

::

	X/Y


Operator Type: Infix

Precedence: 3

* ``X`` and ``Y``.

	- Numbers: ``Y`` must be non-zero.

.. _modulus:

Modulus ( ``%``)
--------------------------------------------

The ``%`` function returns the remainder of its left argument when divided by its right argument.

Syntax:

::

	X%Y


Operator Type: Infix

Precedence: 3

* ``X`` and ``Y``.

	- Integers: ``Y`` must be non-zero.

.. _addition:

Addition ( ``+``)
--------------------------------------------

The ``+`` function performs addition. It can add pairs of numbers or pairs of vectors; vectors will be added componentwse.

Syntax:

::

	X+Y


Operator Type: Infix

Precedence: 2

* ``X`` and ``Y``.

	- Both can be numbers

	- Both can be numeric vectors

.. _subtraction:

Subtraction ( ``-``)
--------------------------------------------

The ``-`` function performs subtraction and negation:

	* It can subtract pairs of numbers or pairs of vectors; vectors will be subtract componentwse.

	* If there's nothing to be subtracted from, ``-`` will negate its number or vector operand.

Syntax:

::

	X-Y

OR

::
	-X

Operator Type: Infix or Prefix

Precedence: 2 (Infix) or 6 (Prefix)

* ``X`` and (Possibly) ``Y``.

	- Both can be numbers

	- Both can be numeric vectors

.. _sum:

Sum ( ``sum``)
--------------------------------------------

The ``sum`` function takes an indefinite number of numbers, and adds them.

Syntax:

::

	sum(A,B,...,Z)

Operator Type: Functional

* ``A``, ``B``, ..., ``Z``

	- Numbers

.. _prod:

Product ( ``prod``)
--------------------------------------------

The ``prod`` function takes an indefinite number of numbers, and multiplies them.

Syntax:

::

	prod(A,B,...,Z)

Operator Type: Functional

* ``A``, ``B``, ..., ``Z``

	- Numbers

.. _ceiling:

Ceiling ( ``ceil``)
--------------------------------------------

The ``ceil`` function gets the ceiling of a real number. The ceiling of a real number is the smallest integer greater than or equal to said real number; in other words, ``ceil`` rounds a number up.

Syntax:

::

	ceil(X)

Operator Type: Functional

* ``X``

	- Real Numbers

.. _floor:

Floor ( ``floor``)
--------------------------------------------

The ``floor`` function gets the floor of a real number. The floor of a real number is the largest integer less than or equal to said real number; in other words, ``floor`` rounds a number down.

Syntax:

::

	floor(X)

Operator Type: Functional

* ``X``

	- Real Numbers

.. _round:

Round ( ``round``)
--------------------------------------------

The ``round`` function rounds a real number in the traditional way; if a number's fractional part is less than ``0.5``, round down; otherwise, round up.

Syntax:

::

	round(X)

Operator Type: Functional

* ``X``

	- Real Numbers

.. _max:

Maximum ( ``max``)
--------------------------------------------

The ``max`` function takes an indefinite number of real numbers, and returns the largest one.

Syntax:

::

	max(A,B,...,Z)

Operator Type: Functional

* ``A``, ``B``, ..., ``Z``

	- Real Numbers

.. _min:

Maximum ( ``min``)
--------------------------------------------

The ``min`` function takes an indefinite number of real numbers, and returns the smallest one.

Syntax:

::

	min(A,B,...,Z)

Operator Type: Functional

* ``A``, ``B``, ..., ``Z``

	- Real Numbers
