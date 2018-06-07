# -*- coding: UTF-8 -*-
import json

table = {}

table["Deflect"]="""Deflect
Shield
Range: Self
Reaction
Use Requirement: You are the target of a Melee, Cone, Line,hspace{1cm} Sweep, or Projectile attack
Effect: You resist the attack two additional steps."""

table["Parry"]="""Parry
Any Weapon
Range: Self
Reaction
Use Requirement: You are the target of a Melee, Line, or Sweep attack
Effect: You resist the attack two additional steps."""

table["Use Rod"]="""Use Rod
Rod
Range: Ranged Projectile 4
Damage: 1W + Willpower"""

table["Use Staff"]="""Use Staff
Staff
Range: Ranged Projectile 8
Damage: 1W + Willpower"""

table["Stab"]="""Stab
Spear
Range: Line 2
Damage: 1W + Combat"""

table["Strike"]="""Strike
Any 1h weapon
Range: Melee 1
Damage: 1W + Combat"""

table["Sweep"]="""Sweep
Any 2h weapon
Range: Sweep 2
Damage: 1W + Combat"""

table["Shoot Arrow"]="""Shoot Arrow
Bow
Range: Ranged Projectile 8
Damage: 1W + Combat
Effect: Consumes 1 Arrow on use; may use other types of Arrows for additional effects."""

table["Throw 1h Weapon"]="""Throw 1h Weapon
Any 1h weapon
Range: Ranged Projectile 6
Damage: 1W + Combat"""

table["Throw 2h Weapon"]="""Throw 2h Weapon
Any 2h weapon
Range: Ranged Projectile 4
Damage: 1W + Combat"""

table["Dross Toss"]="""Dross Toss
Empty Hand
Range: Projectile 6
Damage: Combat"""

table["Painful Touch"]="""Painful Touch
Fist Weapon or Empty Hand
Range: Melee 1
Damage: 1W + Willpower"""

table["Punch"]="""Punch
Fist Weapon or Empty Hand
Range: Melee 1
Damage: 1W + Combat"""

table["Compel Surrender"]="""Compel Surrender
Roll: Intimidate, Influence, or Command v. (Discipline or Insight) + (Rank x 2)
Range: Visual
Effect on Success: The target willingly gives up. They may lay down their arms, allow themselves to be taken prisoner, or simply walk away from the battle, depending on circumstance.
Special: The foe's roll to resist takes a -2 penalty for each of the following circumstances:
• Foe is in Peril
• Foe believes they have little-to-no chance to win the fight
• Foe believes you will be merciful"""

table["Disarm"]="""Disarm
Roll: Combat v. Agility, Combat, or Fortitude
Effect on Success: Select one item that the target currently has in their hands. They drop this item in a square adjacent to them (your choice)."""

table["Examine Armaments"]="""Examine Armaments
Roll: Perception v. Agility
Range: Visual
Effect on Success: You gain a visual description of all items the target has visible, including their currently-equipped armor and weapons. You may make a follow-up Examine Closely Maneuver of your choice for free."""

table["Examine Closely: Diet"]="""Examine Closely: Diet
Roll: Cooking v. ((Rank of Foe + 1) x 3)
Range: Visual
Effect on Success: You learn information about what Food and Elixir effects the foe is currently under. This includes their duration, strength, type, and may include what ingredients were used."""

table["Examine Closely: Enchantments"]="""Examine Closely: Enchantments
Roll: Enchanting v. ((Rank of Foe + 1) x 3)
Range: Visual
Effect on Success: You learn information about what Enchantments the foe's weapons and armor have on them, and any Spell bonuses the foe is under. This includes how powerful they are, what their type is, and the duration (for Spells)."""

table["Examine Closely: Equipment"]="""Examine Closely: Equipment
Roll: Smithing v. ((Rank of Foe + 1) x 3)
Range: Visual
Effect on Success: You learn information about the equipment the foe is using. This includes Crafting Style, the type of item (weapon type and weight of armors), their Rank, and any other information that might be useful, interesting, or pertinent. This does not include information on the equipment's Enchantments."""

table["Examine Closely: Lore"]="""Examine Closely: Lore
Roll: Arcana, Civilization, or Nature v. ((Rank of Foe + 1) x 3); the Trait required depends on the foe's origins
Range: Visual
Effect on Success: You learn information about the foe's biology or construction. This includes information on any Weak Points or Weaknesses it might have, and common attributes and tactics of its species."""

table["Flirt"]="""Flirt
Roll: Influence v. Discipline, Fortitude, or Insight
Effect on Success: Target cannot target you with attacks, and will not intentionally cause harm to you, until the end of their next Turn."""

table["Grapple"]="""Grapple
Roll: Athletics v. Agility, Athletics, or Fortitude
Use Requirement: One hand free (for foes smaller than you), or two hands free (for foes your size). You cannot grapple foes larger than you.
Effect on Success: You grapple the target until the end of your next turn. While grappled, the foe is Halted. When you move, your foe moves with you; your Movement is halved while you are grappling. Your foe has a -3 penalty on To-Hit checks against anyone besides you. You must repeat this check at the beginning of every turn (as a Free Action); if you do not, or you fail it, the target is released. If the grappled target is Pushed away from you, you must repeat this check to maintain your hold on the target."""

table["Hobble"]="""Hobble
Roll: Athletics v. Agility, Athletics, or Fortitude
Effect on Success: Until the end of their next turn, the target treats all terrain as difficult terrain."""

table["Intercept"]="""Intercept
Roll: Agility v. Accuracy
Use Requirement: You are standing next to an ally who is the target of an attack.
Effect on Success: You push the ally one square, and take their place - and take the attack. You cannot use Reactions against this attack."""

table["Low Blow"]="""Low Blow
Roll: Combat v. Agility, or Fortitude
Effect on Success: The target is put Off-Balance until end of their next turn."""

table["Menace"]="""Menace
Roll: Intimidate v. Composure, Insight, or Discipline
Effect on Success: The target cannot use Spells or Techniques until the end of their next Turn. """

table["Predict"]="""Predict
Roll: Insight v. Discipline or Guile
Effect on Success: You learn what the opponent is thinking of doing next, at that moment. If they intend to attack you, follow through on that intention before the end of their next turn, and you use a Reaction to reduce their attack's damage, you will resist the damage by an additional step. Note that "yelling out what the foe intends to do" may make it change its mind, if it is an intelligent foe that can understand your words."""

table["Push"]="""Push
Roll: Athletics v. Agility, Athletics, or Fortitude
Effect on Success: The target is pushed 1 square away from you. """

table["Sand Attack"]="""Sand Attack
Roll: To-Hit v. Evasion
Effect on Success: The target is Blinded until the end of their next turn."""

table["Taunt"]="""Taunt
Roll: Command v. Insight or Discipline
Effect on Success: The target cannot use consumable items or make Trait Checks, and they are compelled to attack the user if at all feasible. Both effects last until the end of their next turn."""

table["Trip"]="""Trip
Roll: Combat v. Agility, or Fortitude
Effect on Success: The target is Knocked Prone."""

table["Animal's Heritage"]="""Animal's Heritage
6 Tokens
Trait Requirement: Fortitude 6, Talking Animal
Effect: Select a benefit of being the Talking Animal race that you do not have already. You gain that benefit."""

table["Artillery Wizard"]="""Artillery Wizard
3 Tokens
Trait Requirement: Willpower 3
Effect: Increase your range on your Rod and Staff Basic Attacks by 2 squares."""

table["Clutch Attacker"]="""Clutch Attacker
3 Tokens
Trait Requirement: Agility 3
Effect: When in Peril, you do 2 more damage with every attack."""

table["Clutch Dodger"]="""Clutch Dodger
3 Tokens
Trait Requirement: Agility 3
Effect: When in Peril, your Evasion increases by 2."""

table["Combat Chemist"]="""Combat Chemist
3 Tokens
Trait Requirement: Accuracy 3
Effect: You may use Elixirs on targets by using the 1H Throw Basic Attack with that Elixir. This does not cause damage to the target, and merely uses the Elixir on them as if they were in melee range of you."""

table["Crafting Style: Bokoblin"]="""Crafting Style: Bokoblin
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon costs 2 fewer Materials, has (Rank+1) less Attack Base Stat, and (Rank+1) less Durability Base Stat."""

table["Crafting Style: Deku"]="""Crafting Style: Deku
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon costs 1 fewer Material, and has (Rank+1) less Durability Base Stat."""

table["Crafting Style: Demon"]="""Crafting Style: Demon
6 Tokens
Trait Requirement: Smithing 6
Effect: You may choose to use this whenever crafting a weapon. The weapon costs 1 more Materials, has (Rank+1) greater Attack Base Stat, (Rank+1) greater Durability Base Stat, and 1 less Accuracy Base Stat."""

table["Crafting Style: Fairy"]="""Crafting Style: Fairy
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon has (Rank+1) less Attack Base Stat, but the user may use Willpower in place of Combat for all Basic Attacks and Techniques made with this weapon."""

table["Crafting Style: Gerudo"]="""Crafting Style: Gerudo
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon costs 1 more Material, and has (Rank+1) greater Attack Base Stat."""

table["Crafting Style: Goron"]="""Crafting Style: Goron
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon has (Rank+1) greater Durability Base Stat, and (Rank+1) less Attack Base Stat."""

table["Crafting Style: Guardian"]="""Crafting Style: Guardian
9 Tokens
Trait Requirement: Smithing 9
Effect: You may choose to use this whenever crafting a weapon primarily using Ancient materials. The weapon costs 3 more Materials, has (Rank+1) greater Attack and Durability, and 1 more Accuracy."""

table["Crafting Style: Kokiri"]="""Crafting Style: Kokiri
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon has (Rank+1) less Attack Base Stat, and 1 greater Accuracy Base Stat."""

table["Crafting Style: Lynel"]="""Crafting Style: Lynel
6 Tokens
Trait Requirement: Smithing 6
Effect: You may choose to use this whenever crafting a weapon. The weapon costs 1 more Materials, has ((Rank+1) x 2) greater Attack Base Stat, and (Rank+1) less Durability Base Stat."""

table["Crafting Style: Neglected"]="""Crafting Style: Neglected
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon using Metal materials. The weapon costs 1 fewer Materials, has ((Rank+1) x 2) less Durability Base Stat, cannot rust, and does not conduct electricity."""

table["Crafting Style: Rito"]="""Crafting Style: Rito
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon has (Rank+1) less Attack Base Stat, and can be equipped while Flying or Gliding without penalty."""

table["Crafting Style: Royal Guard"]="""Crafting Style: Royal Guard
6 Tokens
Trait Requirement: Smithing 6
Effect: You may choose to use this whenever crafting a weapon. The weapon has ((Rank+1) x 2) greater Attack Base Stat, and ((Rank+1) x 2) less Durability Base Stat."""

table["Crafting Style: Sheikah"]="""Crafting Style: Sheikah
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon has (Rank+1) greater Attack Base Stat, and (Rank+1) less Durability Base Stat."""

table["Crafting Style: Subrosian"]="""Crafting Style: Subrosian
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon has (Rank+1) greater Durability Base Stat, and 1 less Accuracy Base Stat."""

table["Crafting Style: Talking Animal"]="""Crafting Style: Talking Animal
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon has (Rank+1) greater Attack Base Stat, and 1 less Accuracy Base Stat."""

table["Crafting Style: Twili"]="""Crafting Style: Twili
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon has (Rank+1) less Durability Base Stat, and 1 greater Accuracy Base Stat."""

table["Crafting Style: Zora"]="""Crafting Style: Zora
3 Tokens
Trait Requirement: Smithing 3
Effect: You may choose to use this whenever crafting a weapon. The weapon costs 1 more Materials, cannot rust, and can be equipped while Swimming without penalty. If the weapon is a Bow or Crossbow, Fire and Bomb Arrows shot from it work just as well underwater as they would on dry land."""

table["Double Identifier"]="""Double Identifier
3 Tokens
Trait Requirement: Perception 3
Effect: When you use the Identify Armaments Combat Maneuver, you may choose two separate skills to use to further gain information about the foe's loadout. Both of these skills are considered part of the same Identify Armaments action."""

table["Elemental Specialist"]="""Elemental Specialist
3 Tokens
Trait Requirement: Arcana 3
Effect: Select an Element. When you make an attack with that Element, it deals 2 additional damage."""

table["Fairy Friend"]="""Fairy Friend
3 Tokens
Requirement: Quest Reward or GM Approval Only
Effect: You gain a Fairy Friend. As a Minor action, you may have this Fairy Friend Mark a target, illuminating it, and providing a +1 bonus to all to-hit rolls (by you and your allies) against that target. If the target turns invisible, teleports, or otherwise disappears, your Fairy Friend loses track of the foe and the Mark is lost. Only one Fairy Friend may mark a target at a time, and a Fairy Friend may only mark one target at a time."""

table["Fairy Friend Focus"]="""Fairy Friend Focus
6 Tokens
Requirement: Fairy Friend
Effect: Your Fairy Friend takes on a particular color, and gains additional benefits to marking targets. Select one of the bonuses below. This choice is permanent.
Black: Your Fairy now glows black. You gain a +1 on all Agility, Combat, and Athletics Trait checks made as part of a Combat Maneuver against the target.
Blue: Your Fairy now glows blue. You gain +1 on on all Examine Armaments and Examine Closely Combat Maneuvers against the target.
Gold: Your Fairy now glows gold. You have 3 additional Defense for all attacks the target initiates against you.
Green: Your Fairy now glows green. You gain a +1 Evasion bonus against attacks made by the marked target.
Red: Your Fairy now glows red. All attacks by you against the marked target do (Weapon Rank) more damage.
Pink: Your Fairy now glows pink. You gain a +1 on all Intimidate, Influence, Insight, and Command Trait checks made as part of a Combat Maneuver against the target."""

table["Fairy Friends"]="""Fairy Friends
This Fairy Friend does not count towards your normal limit on Companions. This Fairy cannot be attacked in combat, nor can it attack, pick up or manipulate objects, or use items, weapons, or armor. Unlike other Fairies, Fairy Friends cannot provide healing. Your Fairy Friend can hide on your person (under your clothing), or provide light up to a Burst 2. Your Fairy Friend can move up to 16 squares away from you; if it moves any further, it reappears at your position. Your Fairy Friend may occupy the same space as you, and is considered Tiny-sized."""

table["Fleet-Footed"]="""Fleet-Footed
6 Tokens
Trait Requirement: Athletics 6
Effect: Your per-round Movement cap increases by 2, to a new maximum of 8."""

table["Invigorating Artist"]="""Invigorating Artist
6 Tokens
Trait Requirement: Perform 6
Effect: Select one of Health, Stamina, or Magic. When you perform a Magical Song, you and all allies who can hear you gain 4 Temporary points in that Pool."""

table["Longshot"]="""Longshot
3 Tokens
Trait Requirement: Accuracy 3
Effect: Increase your range on your Bow, Crossbow, 1h Throw, and 2h Throw Basic Attacks by 2 squares."""

table["Mageslayer"]="""Mageslayer
6 Tokens
Trait Requirement: Arcana 6
Effect: When you defeat a foe with a Spell, you gain 2 Temporary Magic."""

table["Magical Voice"]="""Magical Voice
3 Tokens
Trait Requirement: Perform 3
Effect: You can perform Magical Songs without the aid of an Instrument."""

table["Maneuver Mastery"]="""Maneuver Mastery
6 Tokens
Trait Requirement: Combat Maneuver's used Trait at 6
Effect: Select a specific Combat Maneuver. When you roll to use that Combat Maneuver, add 2 to the roll.
Special: This can be chosen multiple times, each time selecting a different Combat Maneuver."""

table["Natural Movement"]="""Natural Movement
9 Tokens
Requirement: A race capable of a Movement type, but not a Natural at that Movement type.
Effect: You are a Natural at that Movement type."""

table["Practiced Maneuver"]="""Practiced Maneuver
3 Tokens
Trait Requirement: Combat Maneuver's used Trait at 3
Effect: Select a specific Combat Maneuver. Reduce that Combat Maneuver's Stamina cost by 2.
Special: This can be chosen multiple times, each time selecting a different Combat Maneuver."""

table["Quick-Witted"]="""Quick-Witted
3 Tokens
Trait Requirement: Perception 3
Effect: You gain a +2 bonus to Initiative rolls.hspace{5cm}"""

table["Restful Respite"]="""Restful Respite
3 Tokens
Trait Requirement: Stamina 3
Effect: Short Rests provide 4 Temporary Stamina. Extended Rests provide 8 Temporary Stamina. This does not stack with the effects of any bed or bedroll you use during the Rest."""

table["Restorative Respite"]="""Restorative Respite
3 Tokens
Trait Requirement: Hearts 3
Effect: Short Rests provide 4 Temporary Health. Extended Rests provide 8 Temporary Health. This does not stack with the effects of any bed or bedroll you use during the Rest."""

table["Ruminative Respite"]="""Ruminative Respite
3 Tokens
Trait Requirement: Magic 3
Effect: Short Rests provide 4 Temporary Magic. Extended Rests provide 8 Temporary Magic. This does not stack with the effects of any bed or bedroll you use during the Rest."""

table["Skilled Smith"]="""Skilled Smith
6 Tokens
Trait Requirement: Smithing 6
Effect: Select a Weapon type, and select one of Attack, Durability, or Accuracy. When you make a Weapon of that Type, it gains 1 point in that stat."""

table["Slow Metabolism"]="""Slow Metabolism
3 Tokens
Trait Requirement: Fortitude 3
Effect: The effects of Elixirs you consume last an additional round.hspace{5cm}"""

table["Specialist"]="""Specialist
3 Tokens
Trait Requirement: Chosen Trait at 3
Effect: Select a specific non-combat, non-crafting use of a Trait. This use must have a condition to it, such as "using Influence on Shopkeepers", "using Nature to harvest Food ingredients", or "using Civilization to know about Zora culture". When you roll that Trait for that use, add 2 to the roll.
Special: This can be chosen multiple times, each time selecting a different use of a Trait (previously selected or not). If multiple instances of Specialist apply to a given roll, you only gain the bonus once."""

table["Three's Company"]="""Three's Company
3 Tokens
Trait Requirement: Command 3
Effect: You may have up to 2 Companions travelling with you at a time. You still must share your actions with both Companions."""

table["Twinrova's Efficiency"]="""Twinrova's Efficiency
6 Tokens
Trait Requirement: Arcana 6, Gerudo
Effect: Your Spells cost 1 less Magic. For Spells that consider how much Magic was used in the spell, do not include this cost reduction (for instance, if you spend 5 Magic on a Spell, and it is reduced to 4 with Twinrova's Efficiency, then that Spell still counts as having cost 5 Magic for its effect)."""

table["Weapon Specialist"]="""Weapon Specialist
3 Tokens
Trait Requirement: Combat 3
Effect: Select a Weapon type. You add 1 to all To-Hit rolls you make with that Weapon type."""

table["Aimed Shot"]="""Aimed Shot
3 Tokens
Cost: 6 Stamina
Use Requirement: Bow or Crossbow
Range: Weapon
Damage: 1W + Combat
Effect: Treat the target as one step Weaker to this attack."""

table["Arc Shot"]="""Arc Shot
3 Tokens
Cost: 4 Stamina
Use Requirement: Bow or Crossbow
Range: Ranged Projectile 12; Ranged Projectile 16 squares if the target is below your elevation
Damage: 1W + Combat
Effect: You may ignore allies, enemies, and any obstacles that do not reach higher than 6 squares above you, for determining line of sight for this attack."""

table["Armor-Piercing Shot"]="""Armor-Piercing Shot
6 Tokens
Learn Requirement: Aimed Shot
Cost: 10 Stamina
Use Requirement: Bow or Crossbow
Range: Weapon
Damage: 1W + Combat
Effect: Treat the target as two steps Weaker to this attack."""

table["Blade Grasp"]="""Blade Grasp
9 Tokens
Cost: 10 Stamina
Use Requirement: Target of a Basic Attack or Technique; attacker is adjacent to you; two hands empty (or using Fist weapons with no items in them)
Range: Self
Reaction
Effect: You suffer no damage from the attack, but still suffer any other negative effects (status ailments, forced movement, etc.)"""

table["Combo"]="""Combo
6 Tokens
Cost: 10 Stamina
Use Requirement: A Weapon capable of making the Strike Basic Attack
Range: Weapon
Effect: Make three separate Strike Basic Attacks with your weapon. These Strikes do not need to target the same enemy, but you cannot move between one Strike and the next."""

table["Captain's Command"]="""Captain's Command
6 Tokens
Cost: 4 Stamina
Range: 1 ally within 8 squares
Minor Action
Effect: That ally gains +1 Evasion until the end of your next turn, and may move 1 square (if able)."""

table["Darmani's Defense"]="""Darmani's Defense
3 Tokens
Cost: 10 Stamina
Use Requirement: You are struck with an attack.
Learn Requirement: Goron
Reaction
Effect: You resist the attack an addition two steps, and cannot be pushed, knocked prone, or set off-balance by the attack."""

table["Dash Attack"]="""Dash Attack
6 Tokens
Cost: 8 Stamina
Use Requirement: A Weapon capable of making the Strike Basic Attack
Range: See Effect
Damage: 1W + Combat
Effect: During this attack, you may use your remaining Movement as normal. You may move through foes, hitting them with a Strike Basic Attack as you do so. You may only attack a given target once during this attack."""

table["Doppleganger's Movement"]="""Doppleganger's Movement
3 Tokens
Trait: Accuracy
Cost: 12 Stamina
Use Requirement: You are struck by a Melee Basic Attack or Technique.
Learn Requirement: Demon
Range: Self
Reaction
Effect: You suffer no damage from the attack, but still suffer any other negative effects (status ailments, forced movement, etc.)"""

table["Fastball Special"]="""Fastball Special
3 Tokens
Cost: 12 Stamina
Use Requirement: A grappled creature smaller than you
Range: Projectile 8
Damage: Combat
Effect: Damage from this attack ignores Defense. Deal equal damage to the thrown creature, also ignoring Defense. The thrown creature ends in a square adjacent to the hit target."""

table["Fatal Blow"]="""Fatal Blow
6 Tokens
Cost: 10 Stamina
Use Requirement: Target is in Peril
Range: Melee 1
Damage: 3W + Combat"""

table["Flying Fins"]="""Flying Fins
3 Tokens
Cost: 3 Stamina
Learn Requirement: Zora
Range: Projectile 8
Damage: 1W + Combat"""

table["Goron Goroll"]="""Goron Goroll
3 Tokens
Cost: 8 Stamina (Bind)
Learn Requirement: Goron
Minor Action
Effect: You roll into a tight, round ball. While in this stance, your Movement is halved, and you resist all attacks by 1 step. In addition, you cannot use any Actions (Standard, Minor, or Reaction) that do not have the Goron race as a Learn or Use Requirement. You may uncurl as a Minor action during your turn, ending this stance."""

table["Goron Ground Pound"]="""Goron Ground Pound
3 Tokens
Cost: 8 Stamina
Learn Requirement: Goron
Range: Burst 1
Damage: 1W + Combat
Effect: Make a To-Hit check against the target; on success, the target is knocked Prone."""

table["Great Spin"]="""Great Spin
6 Tokens
Learn Requirement: Spin Attack
Cost: 12 Stamina
Use Requirement: 1h Weapon in-hand
Range: Burst 1
Damage: 2W + Combat"""

table["Helm Splitter"]="""Helm Splitter
6 Tokens
Learn Requirement: Jump Attack
Cost: 8 Stamina
Use Requirement: 1h or 2h weapon, clear space on other side of target
Range: Melee 1
Effect: You leap over the target, and land on the other side of them, in a straight line. While overtop the target, you may make a Disarm attempt against the foe; if successful, you knock their Head armor off, depriving them of its benefits."""

table["Hurricane Spin"]="""Hurricane Spin
9 Tokens
Learn Requirement: Great Spin
Cost: 20 Stamina
Use Requirement: 1h Weapon in-hand
Range: Burst 1
Damage: 2W + Combat
Effect: During the attack, you may move as if through Difficult Terrain, moving the Burst of this attack with you. Any targets caught in the Burst before, during, and after you move are struck."""

table["Jump Attack"]="""Jump Attack
3 Tokens
Cost: 4 Stamina
Use Requirement: 1h or 2h Weapon
Range: Melee 1
Damage: 2W + Combat
Effect: Before performing the attack, you must jump towards your target, using your maximum horizontal jump, even if you have already reached you maximum Movement for the turn. You are Off-Balance until the start of your next turn."""

table["Kinetic Shot"]="""Kinetic Shot
6 Tokens
Cost: 10 Stamina
Use Requirement: Bow or Crossbow
Range: Projectile 8
Damage: 1W + Combat
Effect: The target is pushed back 2 squares. If they would impact another creature, that second creature is also struck by this attack (minus the push), and the push ends."""

table["Lizalfos Leap"]="""Lizalfos Leap
3 Tokens
Cost: 8 Stamina
Use Requirement: A foe moves into a square adjacent to you
Reaction
Effect: You immediately move 3 squares away from them, in a non-vertical direction of your choice. This does not consume your Movement."""

table["Lynel's Roar"]="""Lynel's Roar
9 Tokens
Cost: 16 Stamina
Range: Burst 2
Damage: 1W + Combat
Effect: All targets are pushed to just outside the radius of the attack."""

table["Orbit Throw"]="""Orbit Throw
6 Tokens
Learn Requirement: At least one other Technique that requires a Boomerang
Cost: 8 Stamina
Use Requirement: Boomerang
Range: Weapon
Effect: This attack does not harm the target. However, it does create a Burst 1 around the target, striking all targets within that burst."""

table["Ordon Rush"]="""Ordon Rush
6 Tokens
Cost: 8 Stamina
Use Requirement: Target must be one size larger than you or smaller
Range: Melee 1
Damage: Athletics + Combat
Effect: You slam into the target and begin pushing them. You may continue to move, but it must be in a straight line, and your movement has the cost of Sprinting without the additional movement benefits. Your target is pushed in front of you for the duration of this further movement."""

table["Peril Beam"]="""Peril Beam
6 Tokens
Learn Requirement: Sword Beam
Cost: 2 Stamina
Use Requirement: 1h or 2h Weapon, User in Peril
Range: Projectile 8
Damage: 1W + Combat"""

table["Phrenic Eye"]="""Phrenic Eye
6 Tokens
Learn Requirement: Arc Shot
Cost: 8 Stamina
Use Requirement: Bow or Crossbow
Range: Ranged Projectile 20
Damage: 1W + Combat"""

table["Pick-Up Sticks"]="""Pick-Up Sticks
6 Tokens
Learn Requirement: At least one other Technique that requires a Boomerang
Cost: 8 Stamina
Use Requirement: Boomerang
Range: Weapon; must be against an unattended, unsecured item
Effect: The boomerang grabs the item in question, and returns it to you. If you have a hand free, you may immediately put the item in your hand (if possible). Otherwise, the item lands in your square, at your feet. This does not trigger things that occur when an item is struck by an attack, such as a bomb."""

table["Pinning Shot"]="""Pinning Shot
3 Tokens
Cost: 4 Stamina
Use Requirement: Bow, Crossbow, or Thrown Weapon
Range: Weapon
Damage: 1W + Combat
Effect: Make a To-Hit check against the target; on success, the target is Halted until the end of their next turn."""

table["Push-Off Kick"]="""Push-Off Kick
3 Tokens
Cost: 4 Stamina
Use Requirement: Your legs must be free, and you must be able to move.
Range: Melee 1
Effect: You push the target away from you one square, and you push yourself away from the target 1 square."""

table["Riposte"]="""Riposte
6 Tokens
Learn Requirement: Shield Bash
Cost: 4 Stamina
Use Requirement: 1h weapon and Shield
Range: Weapon
Free Action
Effect: When you successfully Deflect an attack with your shield, you may activate this Technique to make a follow-up Strike Basic Attack with your equipped Weapon."""

table["Serpent's Fangs"]="""Serpent's Fangs
3 Tokens
Cost: 6 Stamina
Use Requirement: Two one-handed Melee Weapons (one in each hand)
Range: Weapon
Effect: Make two separate Strike Basic Attacks against a single foe, each using a different hand's weapon. If both attacks hit, your second hit deals additional damage equal to your Combat."""

table["Shield Bash"]="""Shield Bash
3 Tokens
Cost: 4 Stamina
Use Requirement: Shield
Range: Weapon
Free Action
Effect: When you successfully Deflect an attack with your shield, you may activate this Technique to make a follow-up Strike Basic Attack with the Shield you used."""

table["Shield Clash"]="""Shield Clash
3 Tokens
Cost: 8 Stamina
Use Requirement: Shield and 1h Weapon
Range: Burst 1
Effect: Targets struck by this attack must make a Discipline check v. DC 13. All targets affected must include you in their attack (if they choose to make one) on their next turn."""

table["Skirmish Strike"]="""Skirmish Strike
3 Tokens
Cost: 4 Stamina
Use Requirement: 1h Weapon
Range: Weapon
Damage: 1W + Combat
Effect: Following the attack, you may move 1 square in any direction your movement capabilities allow. Your opponent then moves into the space you previously occupied (if able)."""

table["Spin Attack"]="""Spin Attack
3 Tokens
Cost: 8 Stamina
Use Requirement: 1h Weapon in-hand
Range: Burst 1
Damage: 1W + Combat"""

table["Stunner"]="""Stunner
6 Tokens
Cost: 8 Stamina
Use Requirement: A free hand, or a Fist weapon
Range: Melee 1
Damage: 2W + Combat
Effect: Make a To-Hit check against the target; on success, the target is Off-Balance until the end of their next turn."""

table["Sword Beam"]="""Sword Beam
3 Tokens
Cost: 2 Stamina
Use Requirement: 1h or 2h Weapon, User at Full Health
Range: Projectile 8
Damage: 1W + Combat
Effect: This attack can only be used while at full health."""

table["Trick Shot"]="""Trick Shot
3 Tokens
Cost: 4 Stamina
Use Requirement: Bow, Crossbow, or Boomerang
Range: Special
Damage: 1W + Combat
Effect: This attack can hit one target that you can trace an 8-square path to from your location. This path can pass through creatures' spaces, and even through very small holes, but not through solid objects."""

table["Two Against One"]="""Two Against One
6 Tokens
Cost: 12 Stamina
Use Requirement: Two one-handed Melee Weapons (one in each hand); attacked by a foe within weapon range
Range: Weapon
Reaction
Effect: Select one weapon to Parry the attack with. With your other weapon, make a Basic Attack against the foe that triggered this Technique."""

table["Uppercut"]="""Uppercut
3 Tokens
Cost: 4 Stamina
Use Requirement: A free hand, or a Fist weapon
Range: Melee 1
Damage: 1W + Combat
Effect: Push the target back one square."""

table["Venom Strike"]="""Venom Strike
3 Tokens
Cost: 4 Stamina
Learn Requirement: Talking Animal, with Natural Weapon benefit
Use Requirement: A free hand, or a Fist weapon
Damage: 1W + Combat
Effect: Make a To-Hit check against the target; on success, the target is made Sick."""

table["Wide Return"]="""Wide Return
3 Tokens
Cost: 6 Stamina
Use Requirement: Boomerang
Range: Weapon
Damage: 1W + Combat
Effect: After resolving the attack against the original target, repeat it against a different target within 4 squares of the original target."""

table["Windup Strike"]="""Windup Strike
3 Tokens
Cost: 8 Stamina
Use Requirement: Melee Weapon
Range: Weapon
Damage: 2W + Combat"""

table["Bari's Coat"]="""Bari's Coat
3 Tokens
Cost: 4 Magic (Bind)
Range: Self
Effect: You are Electrified. If a foe takes damage from you being Electrified, the effect ends and the Magic bound in this spell becomes Spent."""

table["Blizzagia's Breath"]="""Blizzagia's Breath
6 Tokens
Cost: 8 Magic (Bind)
Range: Close Blast 3
Effect: The squares turn to Slick Ice."""

table["Bombos"]="""Bombos
9 Tokens
Learn Requirement: One other Spell that deals Fire-type damage
Cost: 20 Magic
Range: 4 Targets within 12 squares
Damage: 2W + Willpower, Fire
Effect: Make a To-Hit check against the target; on success, target is On Fire."""

table["Bubble's Touch"]="""Bubble's Touch
3 Tokens
Cost: 4 Magic
Range: Melee 1
Damage: 1W + Willpower, Light
Effect: Make a To-Hit check against the target; on success, target is Cursed."""

table["Camera"]="""Camera
3 Tokens
Cost: 2 Magic (Bind)
Range: Your current field of vision
Effect: You may perfectly remember what you are looking at in this exact moment - well enough to read written words, or produce a high-quality reproduction by sketch. This does not provide you with details that you could not see at the time, nor any capacity to 'zoom in' on the mental picture. When the spell ends, your perfect recall of what you saw also ends, though you may remember it normally."""

table["Chilfos' Javelin"]="""Chilfos' Javelin
3 Tokens
Cost: 3 Magic
Range: Projectile 8
Damage: 1W + Willpower, Ice"""

table["Create Ammunition, Lesser"]="""Create Ammunition, Lesser
3 Tokens
Cost: 1 Magic (Burn)
Use Requirement: At least one empty hand, or space in your inventory.
Range: Self
Effect: Select either an Arrow or a Bomb. When you cast this spell, that piece of ammunition appears in your hand, ready to be used. (If your hand is not empty, the ammunition appears in your inventory.) The ammunition is ephemeral, and obviously both magical and temporary to anyone who observes or handles it. This may only create normal Bombs and Arrows. The ammunition in question lasts until you next take a Short or Extended Rest."""

table["Create Ammunition, Greater"]="""Create Ammunition, Greater
6 Tokens
Learn Requirement: Create Ammunition, Lesser
Cost: 2 Magic (Burn)
Use Requirement: At least one empty hand, or space in your inventory.
Range: Self
Effect: Select either an Arrow or a Bomb. When you cast this spell, that piece of ammunition appears in your hand, ready to be used. (If your hand is not empty, the ammunition appears in your inventory.) The ammunition is ephemeral, and obviously both magical and temporary to anyone who observes or handles it. This may create any Bomb or Arrow type worth no more than 50 rupees. The ammunition in question lasts until you next take a Short or Extended Rest."""

table["Cryonis"]="""Cryonis
3 Tokens
Cost: 1 Magic per square (Bind)
Range: Up to 3 water squares within 8 squares
Effect: Each square of water freezes into a 2-meter tall, 1-meter wide block of solid ice. Any creatures or objects in the square are raised by the freezing ice, and end up on top of the block. This ice can be climbed and stood on. No matter where it was formed - in a current, on a waterfall, or at the entrance of a gushing pipe - the block will stay put, and cannot be moved by natural means. During their turn as a Free Action, the caster may destroy the block(s). It can also be destroyed by any attack that damages it; a block destroyed does not return its bound Magic to the caster."""

table["Dark Brambles"]="""Dark Brambles
3 Tokens
Cost: 2 Magic per square (Bind)
Range: Up to 3 squares within 8 squares
Effect: A tangle of dark brambles sprout from the ground. These brambles count as Difficult Terrain. Any creature starting their turn in, or entering, a square of brambles takes damage equal to your Willpower score. During their turn as a Free Action, the caster may destroy the brambles patch(es). Brambles can also be destroyed by any Fire, Light, or Sharp attack."""

table["Daruk's Protection"]="""Daruk's Protection
6 Tokens
Cost: 4 Magic
Use Requirement: You or an ally within range is struck with a Melee or Projectile attack
Range: 1 target within 8 squares
Reaction
Effect: The target resists the attack 2 additional steps."""

table["Din's Fire"]="""Din's Fire
6 Tokens
Cost: 8 Magic
Range: Burst 1
Damage: 1W + Willpower, Fire
Effect: Make a To-Hit check against the target; on success, target is On Fire."""

table["Fairy's Light"]="""Fairy's Light
3 Tokens
Cost: 2 Magic (Burn)
Range: Self, or 1 Target within 8 squares
Effect: The target regains Health equal to (your Willpower / 2)."""

table["Fairy's Rejuvenation"]="""Fairy's Rejuvenation
6 Tokens
Learn Requirement: One other Spell that restores Health
Cost: 12 Magic (Burn)
Range: 1 Target within 8 squares who is at 0 HP or less
Effect: The target's Health is set to 0, and then they regain Health equal to (your WIllpower)."""

table["Freezor's Touch"]="""Freezor's Touch
9 Tokens
Learn Requirement: One other Spell that deals Ice-type damage
Cost: 12 Magic
Range: Melee 1
Damage: 3W + Willpower, Ice
Effect: Make a To-Hit check against the target; on success, the target is Frozen."""

table["Ghirahim's Daggers"]="""Ghirahim's Daggers
6 Tokens
Cost: 10 Magic
Range: Projectile 8
Damage: 2W + Willpower, Dark
Effect: Make a To-Hit check against the target; on success, the target is Halted until the end of their next turn."""

table["Great Bay Barrier"]="""Great Bay Barrier
3 Tokens
Cost: 4 Magic
Range: Self
Reaction
Damage: 1.5 x Willpower, Shock
Learn Requirement: Zora
Use Requirement: You are the target of a Melee, Sweep, or Projectile attack
Effect: You resist the attack one additional step. The foe who targeted you takes damage as listed by this ability."""

table["Hylia's Light Arrow"]="""Hylia's Light Arrow
9 Tokens
Learn Requirement: One other Spell that deals Light-type damage
Cost: 12 Magic
Range: Projectile 12
Damage: 3W + Willpower, Light
Effect: Make a To-Hit check against the target; on success, target is Cursed."""

table["Impa's Disguise"]="""Impa's Disguise
3 Tokens
Cost: 2 Magic (Bind)
Range: Self
Effect: You alter details about your appearance as you see fit, up to and including your race, gender, and worn armor. (Your weapons' appearances are not altered.) This spell does not alter the smell, sound, or tactile feeling of your person or weapons, and cannot change your visible size category. When you make Guile checks related to this disguise, the spell grants you a +2 Spell Bonus to the check. The effect persists even through Short or Extended Rests, so long as you keep the Magic cost bound."""

table["Jabu-Jabu's Switchhook"]="""Jabu-Jabu's Switchhook
6 Tokens
Learn Requirement: Ravio's Hookshot
Cost: 8 Magic
Use Requirement: You and the target are not in mid-air or in damaging terrain.
Range: Projectile 8
Effect: You switch places with the target. """

table["Korok Leaf's Wind"]="""Korok Leaf's Wind
3 Tokens
Cost: Up to (Willpower) Magic
Range: Line (Magic spent on spell)
Effect: You create a gust of wind, blowing in the direction you are facing. The wind lasts only for a few seconds, but is strong enough to propel a small sailing-raft, knock apples out of trees, fan flames, or otherwise affect the environment. It will also push creatures 2 squares along the spell's Line, unless they are secured, Halted, very heavy, or otherwise immovable."""

table["Keeta's Touch"]="""Keeta's Touch
3 Tokens
Cost: 4 Magic
Range: Melee 1
Damage: 1W + Willpower, Earth
Effect: Make a To-Hit check against the target; on success, one of the target's currently-equipped weapons are Rusted."""

table["Koume's Fire Blast"]="""Koume's Fire Blast
6 Tokens
Cost: 6 Magic
Range: Line 4
Damage: 1W + Willpower, Fire
Effect: Make a To-Hit check against the target; on success, target is On Fire."""

table["Kotake's Ice Beam"]="""Kotake's Ice Beam
6 Tokens
Cost: 6 Magic
Range: Line 4
Damage: 1W + Willpower, Ice
Effect: Make a To-Hit check against the target; on success, target is Frozen."""

table["Kyameron's Splash"]="""Kyameron's Splash
3 Tokens
Cost: 6 Magic
Range: Projectile 8
Damage: 1W + Willpower, Water
Effect: The Projectile from this Spell may bounce off of a single wall, solid object, or other creature, but may only move 8 squares in total. (If it bounces off of a creature, that creature is also considered a target of this spell.) Make a To-Hit check against the target; on success, the target is Soaked."""

table["Lanmola's Stonespray"]="""Lanmola's Stonespray
6 Tokens
Cost: 8 Magic
Range: Projectile 8
Damage: 1W + Willpower, Earth
Effect: Make a To-Hit check against the target; on success, all targets with 1 square of the struck foe take half the damage you rolled as an additional attack."""

table["Magnesis"]="""Magnesis
3 Tokens
Cost: 4 Magic
Range: 1 metallic object within 8 squares, weighing less than (Willpower x 10) pounds.
Effect: You may move a single metallic object in any direction or maneuver you like: up, down, side to side, towards or away from you. The object must stay within the range of the spell, or it immediately falls to the ground. The object can only move up to 6 squares in one round.
If you attempt to take a metallic object from another creature's hands, make a Disarm attempt, using Arcana in place of your Combat. On success, the item is yanked from the creature's hands, and you may move it up to 3 squares away from the target. If you fail, your magnetic hold on the object ends.
They may attempt to take it back on their turn, which is a Combat vs. your Arcana check. If they succeed, your magnetic hold on the item ends.
If you attempt to slow down a creature wearing metal armor, make an Arcana vs. Fortitude check; on success, the creature is Slowed until the end of their next turn.
If you attempt to wield a metallic weapon at range with this spell, you may only use Strike with it, substituting your Willpower for Combat.
You may draw an unattended metallic item to or from an empty hand with this spell, immediately equipping it."""

table["Mipha's Grace"]="""Mipha's Grace
6 Tokens
Cost: 20 Magic (Burn)
Range: 1 target within 8 squares
Effect: The next time the target reaches 0 HP, they are instantly revived, healed (Willpower x 2) Health, and granted bonus Temporary Health equal to (your Willpower / 2). This effect ends whenever you next take an Extended Rest. This spell cannot be cast on someone who is already at 0 HP."""

table["Moa's Invisibility"]="""Moa's Invisibility
9 Tokens
Cost: 12 Magic
Range: Self
Effect: You become Invisible until the end of the encounter. Being successfully struck with an attack will end the effect, as will initiating an attack."""

table["Morpha's Tentacle"]="""Morpha's Tentacle
6 Tokens
Cost: 12 Magic
Range: Line 4
Damage: 2W + Willpower, Water
Effect: Make a To-Hit check against the target; on success, the target is Soaked."""

table["Nayru's Love"]="""Nayru's Love
9 Tokens
Learn Requirement: One other Spell that prevents or reduces damage
Cost: 8 Magic (Burn)
Range: Self
Effect: While the spell is active, damage you take is subtracted (Burned) from your Magic pool, rather than from your Health. If an attack would take you below 0 Magic, then any remaining damage after taking you to 0 Magic is done to your Health, and the spell ends. You may end this effect at any time, though it does not refund any Magic to do so."""

table["Nightmare's Shadow"]="""Nightmare's Shadow
9 Tokens
Learn Requirement: One other Spell that does Dark-type damage
Cost: 12 Magic
Range: 1 target within 8 squares
Damage: 3W + Willpower, Dark
Effect: Make a To-Hit check against the target; on success, the target is Blinded."""

table["Pacci's Flip"]="""Pacci's Flip
3 Tokens
Cost: 4 Magic
Range: 1 target in 8 squares, or 1 unattended object within 8 squares
Effect: If the target is an object, it is flipped upside-down perfectly. This does not guarantee that its new configuration is stable; it may tip over shortly thereafter, to disastrous effects. If the target is a creature, roll To-Hit; on a success, the target is Prone and Off-Balance."""

table["Plasmarine's Orb"]="""Plasmarine's Orb
3 Tokens
Cost: 3 Magic
Range: Projectile 8
Damage: 1W + Willpower, Shock"""

table["Rauru's Shield"]="""Rauru's Shield
3 Tokens
Cost: Up to (Willpower) Magic (Bind)
Range: Self, or 1 Target within 8 squares
Effect: Grant the target a Spell Bonus of Defense equal to (the Magic you spent on this spell / 2)."""

table["Ravio's Hookshot"]="""Ravio's Hookshot
3 Tokens
Cost: 4 Magic
Range: Projectile 8
Effect: If the target is lighter than you, you pull it towards you. If it is an unattended object, you may place it in your hand or inventory; if it is a creature, it lands in the square adjacent to you. If the target is heavier to you, you are pulled towards it, and placed in the nearest empty square adjacent to it."""

table["Reapling's Sweep"]="""Reapling's Sweep
6 Tokens
Cost: 10 Magic
Range: Sweep 3
Damage: 2W + Willpower, Dark"""

table["Redead's Shriek"]="""Redead's Shriek
6 Tokens
Cost: 10 Magic
Range: Cone 2
Damage: 1W + Willpower, Dark
Effect: Make a To-Hit check against the target; on success, target is Frozen."""

table["Revali's Gale"]="""Revali's Gale
6 Tokens
Cost: Up to (Willpower x2) Magic
Range: Burst 1
Effect: An Updraft begins in the affected squares. This updraft has a maximum height of 20 squares, and will persist for 5 minutes. The updraft blows at a rate of (the Magic you spent on this Spell / 2)."""

table["Samasa's Sands"]="""Samasa's Sands
9 Tokens
Learn Requirement: One other Spell that deals Earth-type damage
Cost: 20 Magic
Range: Close Blast 3
Damage: 2W + Willpower, Earth
Effect: Make a To-Hit check against the target; on success, the target's currently-equipped weapons are Rusted."""

table["Saria's Lifewater"]="""Saria's Lifewater
6 Tokens
Learn Requirement: One other Spell that restores Health
Cost: 4 Magic (Burn)
Range: Self, or 1 Target within 8 squares
Effect: The target regains HP equal to your Willpower."""

table["Shabom's Encasements"]="""Shabom's Encasements
9 Tokens
Learn Requirement: One other Spell that deals Water-type damage
Cost: 12 Magic
Range: 1 Target within 12 squares
Damage: 2W + Willpower, Water
Effect: Make a To-Hit check against the target; on success, target is now Soaked, and is trapped in a rainbow-colored bubble. While in the bubble, the target is considered underwater, and must use either their Swimming capability, or may walk as if Slowed. The target may attempt to wriggle free during their turn by taking a Minor or Standard Action, spending an amount of Stamina they choose, and rolling 2d6 plus the Stamina spent; a roll of 10 or more ends the bubble. Striking the foe with a Shock or Piercing attack ends the bubble. Bosses and Mini-Bosses are immune to being trapped in the bubble, but will still be Soaked on success.."""

table["Stone Spikes"]="""Stone Spikes
3 Tokens
Cost: 4 Magic
Learn Requirement: Goron
Range: Self
Effect: You sprout spikes of stone from your body. Until the end of your next turn, whenever you move adjacent to a foe or a foe moves into any square adjacent to you, that foe takes damage equal to your Willpower score. While the spell is active, you may re-cast it as a Minor Action."""

table["Tantari Jump"]="""Tantari Jump
3 Tokens
Cost: (Willpower) Magic (Bind)
Range: Self, or 1 Target within 8 squares
Effect: Add (half of the Magic you spent on this spell) to the target's Athletics Trait, for the purposes of calculating their Horizontal and Vertical jumping capability."""

table["Terrakinesis"]="""Terrakinesis
3 Tokens
Cost: 1 Magic per square (Bind)
Range: Up to 3 sand or loose earth squares within 8 squares
Effect: The earth solidifies into a 2-meter tall, 1-meter wide block of solid earth. Any creatures or objects in the square are raised by the solidifying earth, and end up on top of the block. This block can be climbed and stood on. No matter where it was formed - in a quicksand pit, on a steep hill, or at the entrance of a gushing pipe - the block will stay put, and cannot be moved. During their turn as a Free Action, the caster may destroy the block(s). It can also be destroyed by any attack that damages it; a block destroyed does not return its bound Magic to the caster."""

table["Titan's Strength"]="""Titan's Strength
3 Tokens
Cost: (Willpower) Magic (Bind)
Effect: Add (half of the Magic you spent on this spell) to the target's Athletics Trait, for the purposes of calculating their one-handed and two-handed Lifting capability."""

table["Twili Transit"]="""Twili Transit
6 Tokens
Cost: 8 Magic
Learn Requirement: Twili
Use Requirement: Must be standing in a deep shadow in the Light World
Range: Self
Effect: You merge with the shadow, becoming Invisible and intangible. While merged with the shadow, you can see, hear, and speak, but you cannot interact with objects, or take any Actions (Standard, Move, Minor, or Reaction). You cannot be harmed by any physical attack, though magical attacks will harm you and cause you to be revealed, ending the spell. You cannot move, but if your shadow's source moves, then you are carried along with it. If your shadowy spot is exposed to light or otherwise ceases to be, you are immediately revealed and the spell ends. You may end the spell as a Minor Action. This spell ends when you take a Short or Extended Rest.
Special: While in the Dark World, this spell's effects are reversed: you may hide in areas of bright light, and are revealed when shadowed."""

table["Urbosa's Fury"]="""Urbosa's Fury
6 Tokens
Cost: 10 Magic
Range: Burst 1
Damage: 1W + Willpower, Shock
Effect: Make a To-Hit check against the target; on success, target is Off Balance."""

table["Vanish"]="""Vanish
3 Tokens
Cost: 8 Magic
Range: Burst 1
Effect: Make a To-Hit check against the target; on success, target is Blinded until the end of their next turn. You may enter stealth as part of this attack; if you do, all foes caught in the burst do not see your movement for the remainder of your turn, regardless of whether or not they were Blinded (but may still perform Reactions in response to your attacks)."""

table["Wizzrobe's Wave"]="""Wizzrobe's Wave
3 Tokens
Cost: 2 Magic
Range: Ranged Projectile 8
Damage: 1W + Willpower"""

table["Wrath's Whip"]="""Wrath's Whip
3 Tokens
Cost: 4 Magic
Range: Projectile 4
Damage: 1W + Willpower, Dark
Effect: Make a To-Hit check against the target; on success, target is Off Balance."""

table["Yarna's Vortex"]="""Yarna's Vortex
6 Tokens
Cost: 8 Magic (Bind)
Range: Close Blast 3
Effect: The affected area becomes Quicksand."""

table["Zant's Shadowbolts"]="""Zant's Shadowbolts
6 Tokens
Cost: 12 Magic
Range: 3x Projectile 8
Damage: 1W + Willpower, Dark"""

table["Zola's Fireball"]="""Zola's Fireball
3 Tokens
Cost: 3 Magic
Range: Projectile 8
Damage: 1W + Willpower, Fire"""

table["Accuracy Up"]="""Accuracy Up
Requires: Enchanting 4
Uses: 4x Rank 3 gems, 2x Rank 4 gems, or 1x Rank 5 gem
Equipment Type: Any Weapon
Effect: The weapon's Accuracy is increased by (your Enchanting Trait / 4)"""

table["Amber-Coated"]="""Amber-Coated
Required: Enchanting 4
Uses: Amber
Equipment Type: Any Weapon
Effect: This weapon cannot be Rusted, does not take additional damage while its user is Sand-Covered, and otherwise does not take Durability damage from anything except normal use."""

table["Attack Up"]="""Attack Up
Requires: Enchanting 4
Uses: 4x Rank 3 gems, 2x Rank 4 gems, or 1x Rank 5 gem
Equipment Type: Any Weapon
Effect: The weapon's Attack Power is increased by (your Enchanting Trait / 2)."""

table["Cooled Weapon"]="""Cooled Weapon
Required: Enchanting 2
Uses: Sapphire
Equipment Type: Any Weapon
Effect: This weapon, and any ammunition fired from it, is immune to the Burning condition. In addition, it provides one step of resistance to Heatwave weather to its wielder."""

table["Critical Hit"]="""Critical Hit
Requires: Enchanting 6
Uses: 4x Rank 3 gems, 2x Rank 4 gems, or 1x Rank 5 gem
Equipment Type: Any Weapon
Effect: Increase this weapon's Critical Hit range by (your Enchanting / 4)."""

table["Durability Up"]="""Durability Up
Requires: Enchanting 2
Uses: 4x Rank 3 gems, 2x Rank 4 gems, or 1x Rank 5 gem
Equipment Type: Any Weapon
Effect: The weapon's Durability is increased by (your Enchanting Trait / 2)."""

table["Efficient"]="""Efficient
Required: Enchanting 8
Uses: Emerald
Equipment Type: Bow, Crossbow
Effect: When you fire a unit of ammunition with with weapon, roll 1d6. On a 6, you do not spend that unit of ammunition, but the attack continues as normal."""

table["Elemental Weapon"]="""Elemental Weapon
Requires: Enchanting 4
Uses: Varies
Equipment Type: Any Weapon
Effect: The weapon now holds elemental energy, based on the type of Gem used. Each time the weapon is used in a Basic Attack, Technique, or Spell, you may pay 2 Stamina to make the attack take on that element. In addition, the user gains access to the Elemental Chaff attack, below."""

table["Elemental Chaff"]="""Elemental Chaff
Cost: 6 Stamina
Use Requirement: A Weapon with the Elemental Weapon Enchantment
Range: Cone 2 (Melee Weapons), Line 4 (Ranged Weapons)
Damage: 1W + (Enchanting Trait used to make the Enchantment), of the weapon's Element
Effect:	Make a To-Hit check, substituting (Enchanting Trait used to make the
enchantment) for your Accuracy; on success, target is inflicted with the element's
corresponding status effect (see below).
• Opal--Water--Soaked
• Topaz--Shock--Off-Balance
• Ruby--Fire--On Fire
• Sapphire--Ice--Slowed
• Emerald--Earth--Sand-Covered
• Diamond--Light--Cursed
• Onyx--Darkness--Blinded"""

table["Heated Weapon"]="""Heated Weapon
Required: Enchanting 2
Uses: Ruby
Equipment Type: Any Weapon
Effect: This weapon, and any ammunition fired from it, counts as a heatsource, and can be used to melt ice. If you are Frozen, you can still make attacks with this weapon. In addition, it provides one step of resistance to Coldsnap weather to its wielder."""

table["Hydrodynamic"]="""Hydrodynamic
Requires: Enchanting 4
Uses: Opal
Equipment Type: Any Weapon
Effect: You can use this weapon underwater without penalty, just as if you were on dry land. Fire and Bomb Arrows shot from a Bow or Crossbow with this enchantment work just as well as they would on dry land.  This weapon does not suffer ill effects from being exposed to fresh or salt water."""

table["Longshot"]="""Longshot
Required: Enchanting 4
Uses: Diamond
Equipment Type: Any Ranged
Effect: Increase the range of any Projectile attacks made with this weapon by 4."""

table["Loyal"]="""Loyal
Required: Enchanting 6
Uses: Topaz
Equipment: Any Weapon
Effect: As a Minor Action, you may call this weapon to your empty hand. The weapon must be within line of sight, must be unattended, and must be able to pass through the intervening space between itself and your hand."""

table["Multi-Shot"]="""Multi-Shot
Required: Enchanting 8
Uses: Onyx
Equipment Type: Any Ranged
Effect: When you make a Basic Attack with this weapon, roll 1d6. On a 6, you may repeat that Basic Attack against a different target within range."""

table["Perfectly Balanced"]="""Perfectly Balanced
Requires: Enchanting 10
Uses: 4x Rank 3 gems, 2x Rank 4 gems, or 1x Rank 5 gem
Equipment Type: Any Weapon
Effect: The weapon's Attack and Durability are increased by 3, its Accuracy is increased by 2, and its Critical Hit range is increased by 1."""

table["Quickshot"]="""Quickshot
Required: Enchanting 8
Uses: Topaz
Equipment Type: Any Ranged
Effect: Targets struck by Basic Attacks made with this weapon wishing to use Reactions must make an Agility check v. DC 13. If they fail, they may not use Reactions against that attack."""

table["Tracer"]="""Tracer
Required: Enchanting 6
Uses: Luminous Stone
Equipment Type: Any Ranged
Effect: When you strike a foe with this weapon, they are illuminated until the end of your next turn. While illuminated, they cast Radius 2 light, and lose 2 Evasion."""

table["Annoying"]="""Annoying
Requires: Enchanting 4
Uses: Luminous Stone
Equipment Type: Clothing & Armor
Effect: The equipment glows in an annoying, eye-catching way, casting a Burst 0 light (lighting only itself). Foes gain +1 Accuracy and +2 damage when attempting to hit you with an attack. This effect stacks with itself, up to +3 Accuracy and +6 damage when foes attempt to attack you."""

table["Blindproof"]="""Blindproof
Requires: Enchanting 6
Uses: Luminous Stone
Equipment Type: Clothing & Armor
Effect: You are immune to the Blinded condition."""

table["Commanding"]="""Commanding
Requires: Enchanting 4
Uses: Topaz
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Commanding (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Crafty"]="""Crafty
Requires: Enchanting 8
Uses: Amber
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Crafty (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Electricity-Proof"]="""Electricity-Proof
Requires: Enchanting 6
Uses: Topaz
Equipment Type: Clothing & Armor
Effect: The wearer is immune to the effect of attacking Electrified enemies with metallic weapons."""

table["Eye-Opening"]="""Eye-Opening
Requires: Enchanting 2
Uses: Diamond
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Eye-Opening (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Glowing"]="""Glowing
Requires: Enchanting 2
Uses: Luminous Stone
Equipment Type: Clothing & Armor
Effect: The equipment provides light in a Burst 2. This light is bright enough to see, safely walk around, and even read a book. If multiple pieces of equipment with this enchantment are worn, the size of the burst stacks - a hero fully enchanted with Glowing will glow in a Burst 6."""

table["Grounded"]="""Grounded
Requires: Enchanting 6
Uses: Emerald
Equipment Type: Clothing & Armor
Effect: The wearer is immune to all damage and ill effects of lightning from thunderstorms. (This does not provide any protection against other sources of Shock damage.)"""

table["Hardened"]="""Hardened
Requires: Enchanting 4
Uses: Amber
Equipment Type: Clothing & Armor
Effect: This piece of equipment provides 1 additional Defense."""

table["Hasty"]="""Hasty
Requires: Enchanting 2
Uses: Topaz
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Hasty (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Mighty"]="""Mighty
Requires: Enchanting 6
Uses: Amber
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Mighty (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Mire-Proof"]="""Mire-Proof
Requires: Enchanting 6
Uses: Luminous Stone
Equipment Type: Clothing & Armor
Effect: The wearer is immune to the effect of toxic ground, such as Malice, poisonous swamps, and the like. """

table["Nightvision"]="""Nightvision
Requires: Enchanting 4
Uses: Luminous Stone
Equipment Type: Clothing & Armor
Effect: You can see just as well in the dark as in the light; no matter the light level, the world appears perfectly lit. This provides no protection against Blindness."""

table["No-Slip Grip"]="""No-Slip Grip
Requires: Enchanting 2
Uses: Onyx
Equipment Type: Clothing & Armor
Effect: While worn, the wearer will not drop their weapon as a result of status effects, and will not suffer penalties for climbing while Soaked. (They can still be Disarmed via Combat Maneuver.)"""

table["Resistant: Dark"]="""Resistant: Dark
Requires: Enchanting 8
Uses: Diamond
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Resist: Dark (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Resistant: Earth"]="""Resistant: Earth
Requires: Enchanting 8
Uses: Topaz
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Resist: Earth (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Resistant: Fire"]="""Resistant: Fire
Requires: Enchanting 8
Uses: Sapphire
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Resist: Fire (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Resistant: Ice"]="""Resistant: Ice
Requires: Enchanting 8
Uses: Ruby
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Resist: Ice (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Resistant: Light"]="""Resistant: Light
Requires: Enchanting 8
Uses: Onyx
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Resist: Light (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Resistant: Shock"]="""Resistant: Shock
Requires: Enchanting 8
Uses: Emerald
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Resist: Shock (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Resistant: Water"]="""Resistant: Water
Requires: Enchanting 8
Uses: Opal
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Resist: Water (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Sand-Walk"]="""Sand-Walk
Requires: Enchanting 2
Uses: Emerald
Equipment Type: Clothing & Armor
Effect: You may treat Deep Sand and other sand-based difficult terrain as normal terrain."""

table["Scholarly"]="""Scholarly
Requires: Enchanting 6
Uses: Opal
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Scholarly (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Sneaky"]="""Sneaky
Requires: Enchanting 4
Uses: Onyx
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Sneaky (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Spurred"]="""Spurred
Requires: Enchanting 4
Uses: Amber
Equipment Type: Clothing & Armor
Effect: When you ride another creature as a mount, that creature gains +1 Movement."""

table["Snow-Walk"]="""Snow-Walk
Requires: Enchanting 2
Uses: Sapphire
Equipment Type: Clothing & Armor
Effect: You may treat Deep Snow and other snow-based difficult terrain as normal terrain."""

table["Stoneborne"]="""Stoneborne
Requires: Enchanting 4
Uses: Emerald
Equipment Type: Clothing & Armor
Effect: While climbing, you can move at full speed. In addition, reduce the Accuracy penalty while climbing by 1 (to a minimum of no penalty, if you are wearing three pieces of equipment enchanted with Stoneborne). If you wear three pieces of Stoneborne armor, you are also considered a Natural Climber."""

table["Subtle"]="""Subtle
Requires: Enchanting 6
Uses: Onyx
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Subtle (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Sure-Footed"]="""Sure-Footed
Requires: Enchanting 2
Uses: Amber
Equipment Type: Clothing & Armor
Effect: You do not have any risk of slipping and falling on Slippery terrain of any kind."""

table["Strong"]="""Strong
Requires: Enchanting 4
Uses: Ruby
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Strong (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Tough"]="""Tough
Requires: Enchanting 6
Uses: Diamond
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Tough (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Unburnable"]="""Unburnable
Requires: Enchanting 6
Uses: Ruby
Equipment Type: Clothing & Armor
Effect: The wearer is immune to the On Fire status effect. (The wearer's weapons can still be set Burning, however.)"""

table["Undead Resistant"]="""Undead Resistant
Requires: Enchanting 8
Uses: Luminous Stone
Equipment Type: Clothing & Armor
Effect: While worn, the wearer gains permanent Resist: Undead (Rank 1). If multiple pieces are worn with this enchantment, the effect increases by one rank per piece (so if all three armor pieces have this enchantment, the effect is at Rank 3). This does not stack with consumable benefits - only the highest of the two have any effect."""

table["Unfreezable"]="""Unfreezable
Requires: Enchanting 6
Uses: Sapphire
Equipment Type: Clothing & Armor
Effect: The wearer is immune to the Frozen status effect."""

table["Unsoakable"]="""Unsoakable
Requires: Enchanting 4
Uses: Opal
Equipment Type: Clothing & Armor
Effect: While worn, the wearer is immune to the Soaked status effect."""

table["Waterborne"]="""Waterborne
Requires: Enchanting 2
Uses: Opal
Equipment Type: Clothing & Armor
Effect: While swimming, you can move at full speed. In addition, reduce the Accuracy penalty while swimming by 1 (to a minimum of no penalty, if you are wearing three pieces of equipment enchanted with Waterborne). If you wear three pieces of Waterborne armor, you are also considered a Natural Swimmer."""

table["Weatherproofing: Cold"]="""Weatherproofing: Cold
Requires: Enchanting 2
Uses: Ruby
Equipment Type: Clothing & Armor
Effect: When you are in Coldsnap weather, treat that weather as two ranks lower when calculating its effect on you (minimum 0). Multiple instances of this effect stack with itself, as well as with effects from food and racial bonuses."""

table["Weatherproofing: Heat"]="""Weatherproofing: Heat
Requires: Enchanting 2
Uses: Sapphire
Equipment Type: Clothing & Armor
Effect: When you are in Heatwave weather, treat that weather as two ranks lower when calculating its effect on you (minimum 0). Multiple instances of this effect stack with itself, as well as with effects from food and racial bonuses."""

table["Weatherproofing: Sandstorm"]="""Weatherproofing: Sandstorm
Requires: Enchanting 2
Uses: Opal
Equipment Type: Clothing & Armor
Effect: When you are in Standstorm weather, treat that weather as two ranks lower when calculating its effect on you (minimum 0). Multiple instances of this effect stack with itself, as well as with effects from food and racial bonuses."""

table["Weatherproofing: Rain"]="""Weatherproofing: Rain
Requires: Enchanting 2
Uses: Emerald
Equipment Type: Clothing & Armor
Effect: When you are in Rainy weather, treat that weather as two ranks lower when calculating its effect on you (minimum 0). Multiple instances of this effect stack with itself, as well as with effects from food and racial bonuses. However, if lightning is striking, this provides no protection from its effects, nor does it make it less likely to strike you."""

table["Windborne"]="""Windborne
Requires: Enchanting 4
Uses: Diamond
Equipment Type: Clothing & Armor
Effect: While flying or gliding, you can move at full speed. In addition, reduce the Accuracy penalty while flying or gliding by 1 (to a minimum of no penalty, if you are wearing three pieces of equipment enchanted with Windborne). If you wear three pieces of Windborne armor, you are also considered a Natural Flier and a Natural Glider."""

table["Balloon"]="""Balloon: (Monster Parts)
 Balloons can be filled with air, allowing them to float into the sky, or keep things from sinking into the water. A single Balloon is about the size of a Hylian's head, and can carry 1 pound of weight (about 450 grams). Inflating a Balloon requires you to have your mouth and at least one hand free. Multiple Balloons can be tied to larger objects. 

Things lifted by Balloons will rise at a rate of 3 squares/minute through the air, or 6 squares/minute through water. Balloons can be burned, cut, or pierced, popping the Balloon and dropping their cargo. Balloons pop after one hour."""

table["Candle"]="""Candle: (Monster Parts)
 Candles can be lit to provide a little bit of light. A single candle provides a Burst 1 of dim light - enough to see by. It is only bright enough to read at Burst 0, however. A candle can be placed on any stable surface, and will burn until the end of your next Short or Extended Rest."""

table["Flint"]="""Flint: (Metal)
 Flint can be struck against any hard surface to generate a brief shower of bright sparks, starting a fire on any flammable object directly beneath the Flint. This destroys the Flint."""

table["Firewood"]="""Firewood: (Wood)
 Firewood is typically made of poor-quality wood, or the spare branches from a felled and properly-cut tree. When lit, firewood makes a small campfire that will burn until the end of your next Short or Extended Rest. These small campfires are perfect for resting around, providing warmth, light, and a convenient place to cook individual Ingredients of food. Firewood, of course, is consumed upon use."""

table["Nail"]="""Nail: (Metal)
 A nail is a small sliver of metal with a flat head, used to secure things into wood, hardened dirt, or other firm surfaces. Securing a nail into harder objects, like stone or metal, may require an Athletics roll; on a failure, the nail is destroyed without piercing the hard surface. Nails are metallic, and are affected by magnetism and electricity."""

table["Oil"]="""Oil: (Monster Parts)
 Oil is a flammable, slippery substance. One unit of oil will fill a single square of land or water with Oiled terrain. It can also be used to lubricate rusted or stuck machinery, fuel combustion-powered devices, remove Rust from a weapon, or can coat a single Weapon until your next Extended Rest, protecting it from Rust.

Creatures covered in Oil are extremely flammable: being hit with any Fire-type damage, standing a full turn (start to finish) adjacent to something that is On Fire or Burning, or merely being exposed to extreme heat, will cause an Oiled square or oil-covered creature to begin Burning. 

Oil can be washed off by going swimming in a body of water, using Soap, or during a Short or Extended Rest."""

table["Soap"]="""Soap: (Critters, Food, or Monster Parts)
 Soap is a slippery substance. One unit of soap will fill a single square of land with a Slightly Slippery surface, or a single square of water with Foaming Water. It can also be used to clean dirty objects, shine up reflective surfaces. Soap can remove Oil from surfaces, people, and Critters, destroying Oil on a one-for-one basis."""

table["Bedroll"]="""Bedroll: (Cloth)
 When you take an Extended Rest using a Bedroll, you restore 50\% of your maximum Health, Stamina, and Magic, and gain Temporary Health equal to (Rank x 2). You must still be in a dry location, in an environment you can survive without aid or armor."""

table["Book, Blank"]="""Book, Blank: (Wood)
 An empty book that can be used to record thoughts, make sketches, take notes, or create Written Books."""

table["Book, Written"]="""Book, Written: (Wood)
 A book full of information, drawings, or writings. Typically sold at bookstores or found in libraries, with information about a single subject (such as a specific village, dungeon, historical event, or long-lost artefact). When used in conjunction with knowledge checks about the book's subject, the give the user a (Rank/2) bonus to the roll.

Unlike other Tools, Written Books can only be crafted by simply using an Ink and Quill on an Empty Book; further, their Rank is limited both by the Rank of the Ink and Quill and of the Empty Book used to make them, and also by the user's skill in the appropriate Trait (substituting for the Smithing skill). Books you write cannot be used to grant yourself a bonus to the skill in question, but can be used by other Heroes."""

table["Bottle"]="""Bottle: (Monster Parts)
 A Bottle can contain a single large serving of a liquid substance (about 1 Liter, or 4 cups' worth), a single dose of an Elixir, or a single Ingredient (including Critters and Small Fairies). A Bottle's contents are kept fresh and uncontaminated for as long as they are secured in the Bottle. (A creature in a Bottle does not need to worry about suffocation.) Contents in a Bottle cannot be smelled or tasted unless the Bottle is uncorked. Bottles break when thrown, spilling their contents over whatever they hit, for good or ill. Bottles can withstand most attacks from within; however, if a creature is trapped inside a Bottle, they may be able to break through the glass, or cleverly uncork the bottle from the inside. Bottles have a Defense of (Rank x 3) against attacks from inside and out; if an attack against them does not exceed their Defense, then the Bottle is not broken and its contents are not freed."""

table["Caltrops"]="""Caltrops: (Metal)
 When you use Caltrops, you cover a Cone 3 with dozens of tiny spikes. These heavy spikes are Difficult Terrain, and cause (5+Rank) damage per square passed through, unmitigated by Defense. A creature passing through a square does not consume the space's caltrops, leaving them to be stumbled upon by the next combatant. Caltrops can be removed from a single square as a Standard Action, and can be recovered after combat for use again later."""

table["Cooking Vessel, Portable"]="""Cooking Vessel, Portable: (Metal)
 The Portable Cooking Vessel allows its user to cook full-fledged Food Dishes and Elixirs, containing up to (2+(Rank/2)) ingredients. It still requires both a heat source (such as a fire, lantern, or a fire-enchanted weapon) as well as a dry environment, in order to function properly."""

table["Flint-fire Kit"]="""Flint-fire Kit: (Metal)
 A Flint-fire kit allows the user to create sparks, which can in turn ignite other objects, such as torches, campfires, lanterns, and bomb or cannon fuses. Flint-fire Kits will not operate in rainy or wet conditions."""

table["Grappling Hook"]="""Grappling Hook: (Metal)
 A Grappling Hook can help attach one thing to another securely. When used in conjunction with a rope, the rope can wrap around and cling to outcroppings on walls. When attached to other objects, it can be used to help secure or carry things that would otherwise be awkward to handle. Grappling Hooks are metallic, subject to magnetic and electrical effects."""

table["Ink and Quill"]="""Ink and Quill: (Monster Parts)
 An Ink and Quill set will let you write on paper with clear and legible writing."""

table["Korok Leaf"]="""Korok Leaf: (Cloth)
 This large fan-like leaf can be waved to generate large gusts of wind, and requires two hands to wield. When waved, it casts the Korok Leaf's Wind spell, substituting Stamina for Magic. You cannot spend Stamina on the spell greater than (Rank x 2)."""

table["Lantern"]="""Lantern: (Metal)
 When lit, a Lantern provides light, heat, and a source of fire. Lanterns can stay lit even in wet or rainy conditions (but not when fully submerged underwater). It can either provide light in a Burst (2+(Rank/2)), or a Cone (5+Rank). Lanterns are automatically extinguished at the end of any Short or Extended Rest. Lanterns cast their light while placed on the ground, held in the hand, or placed on the hip, but not when stored in the Pack."""

table["Musical Instrument"]="""Musical Instrument: (Various)
 A Musical Instrument allows the user to play music, including Magical Songs that they may know. It can also be used to make noise, entertain, or distract. The material type needed depends on what kind of Instrument is being made: for example, Drums may require Monster Parts, a Violin would require Wood, and a Saxophone would require Metal."""

table["Net"]="""Net: (Cloth)
 Nets cover a 3x3 square, and can be cast as a Blast 3. When under a Net, foes are Halted until they can cut their way free by attacking the Net. Nets will be set Burning if exposed to fire or extreme heat. Nets have a Health of (Rank x 6) and a Defense of (Rank); the Net is destroyed (and its contents are freed) when the Net runs out of Health."""

table["Rope"]="""Rope: (Cloth)
 Rope comes in 6-square lengths. When used to climb cliffs and walls, it provides +2 to the Athletics roll to do so. A Rope will be set Burning if exposed to fire or extreme heat. Rope is instantly cut in two by any Piercing or Sharp attack of sufficient strength, and will suffer only (Rank) full rounds of Burning before being consumed utterly. Burning or cutting a rope will consume its Durability. Ropes have a Defense of (Rank x 2) against attacks; if any attack against them does not exceed their Defense, then the Rope is not broken."""

table["Sack"]="""Sack: (Cloth) 
Sacks are a handy way to carry a lot of loose or unmanageable material. Sacks are not waterproof, and can be cut open by any piercing or sharp attack. Sacks cannot be stored in your Pack while they contain other objects. Sacks will be set Burning if exposed to fire or extreme heat. Sacks have a Health of (Rank x 6) and a Defense of (Rank); the Sack is destroyed (and its contents are freed) when the Sack runs out of Health."""

table["Shovel"]="""Shovel: (Metal) 
Shovels (and other digging tools) are useful for digging through snow and dirt, and can even help cut through solid rock. When using a shovel, a Hero can dig through loose sand or snow at a rate of (Rank + 1) squares per hour, dirt and soil at a rate of ((Rank + 1) / 2) squares per hour, or through stone at a rate of ((Rank + 1) / 4) squares per hour."""

table["Telescope"]="""Telescope: (Metal) 
Telescopes allow you to see details from far away, but narrow your field of view significantly. When using a Telescope, you may see objects up to ((Rank+1) x 20) squares away, if you have clear line of sight on them. However, you cannot see anything that is not directly in front of you. Telescopes are sensitive to extreme light - staring at a bright light source may cause the user Light-element damage and/or Blind them for a time. Using a Telescope to focus light to cause heat, or to alter a beam of light, will consume the item's Durability."""

table["Tent"]="""Tent: (Cloth) 
Tents provide shelter from the elements. While in a tent, you are shielded from rain and wind. When properly set up, Tents provide (Rank) resistance from all weather to all creatures within. A Tent has enough room for four heroes to rest comfortably (if cozily)."""

table["Tool (Harvesting)"]="""Tool (Harvesting): (Varies) 
Harvesting Tools are keyed to a particular kind of material: Ancient-, Metal-, Gem-, or Wood-harvesting Tools are made from their target Material; Critter-harvesting Tools are made from Cloth; Food-harvesting Tools from Metal; and all others from Wood or Metal.

A Harvesting Tool allow the wielder to harvest its designated kind of Material without consuming the Durability of a weapon (or of the Tool itself). Harvesting Tools also provide a bonus to the scavenging rolls for their particular kind of Material equal to (Rank/2)."""

table["Toolkit (Profession)"]="""Toolkit (Profession): (Varies) 
Every Toolkit is keyed to a particular profession or skill, such as "lock-picking", "climbing", "forging documents", or the like. Some tasks in a profession may require a Toolkit to even be attempted at all. When used for their chosen profession or task, Toolkits provide a bonus of (Rank/2) to a single roll for one hero. A Toolkit's profession is decided when it is created, and is permanent."""

table["Torch"]="""Torch: (Wood) 
When lit, Torches provide a source of light, heat, and fire. Torches need an external source of fire or sparks to be lit, and will not stay lit in rainy or wet conditions. They provide light in a Burst (4+(Rank/2)). Torches, despite being wooden, will never take Durability damage from Burning, and will thus never be consumed by fire; they still take Durability damage when striking foes or otherwise used as a weapon. Torches are automatically extinguished at the end of any Short or Extended Rest, or when entering wet or rainy conditions. Torches can be lit or extinguished as a Standard action."""

table["Song of Awakening"]="""Song of Awakening
4 Magic (Burn)

When you play this song, all sleeping creatures that can hear it are awoken the moment the song ends. This automatically succeeds against any mundane slumber, no matter how tired the target might be. Against magically-induced slumber, you must make a Perform check against the curse-casting creature's Arcana; you awaken the target only if you succeed."""

table["Song of Birds"]="""Song of Birds
8 Magic (Burn)

When you play this song, you catch the attention of all birds in the area. This song does not alter their disposition towards you - if a bird does not like you before the song, it will not like you after.

Most birds that are neutrally or positively disposed towards you will stop and pay attention to you. This allows you to pick them up and gently manipulate them. If you attempt to hurt or use the bird, it will escape your grasp and fly off - and may become negatively disposed to you.

If a bird was already positively disposed to you, you may give it a simple command that it will follow to the best of its ability. This might be to fly to a particular place, to search for someone wearing a certain color of cloak, or other tasks that a bird could reasonably perform.

If a bird was negatively disposed towards you, it may attack immediately following the end of the song. Any such angry birds will prefer to attack the song's performer, if at all possible.

Perform 1 - Perform Trick: The bird immediately performs a simple trick, much like a pet would.

Perform 2 - Carry Message: The bird can carry a letter or other written message to a location within a day's flight from its current location.

Perform 3 - Act as Guide: The bird can guide you to a specific place you can name, within a day's flight from its current location.

Perform 4 - Fetch the Shiny: The bird can seek out, pick up, and return to you, with an item you can name within a day's flight. The item must be unsecured and not inside a building or any kind of bag or container, and must be small and light enough for the bird to pick up.

Perform 5 - Keep a Watch: The bird can keep watch until the end of your next Extended Rest, cawing when triggered."""

table["Song of Blooming"]="""Song of Blooming
8 Magic (Burn)

Upon completion of the song, you cause all plant-based outdoor terrain within (Perform x 4) squares of you to bloom with new growth and foliage. The affected area becomes difficult terrain.

In addition, the song causes crops, fungi, and fruit-bearing plants within (Perform x 4) squares of you to yield food. When harvesting these plants, add +3 to the scavenge roll."""

table["Command Melody"]="""Command Melody
12 Magic (Burn)

When you play this song, select one inanimate object or willing creature. For the next hour, you control that object or creature.

The size of the inanimate object or creature you can control depends on your Perform skill. If you can play the Command Melody, you can control creatures or objects of 1 meter cubed or smaller - the size of a child, fairy, or Deku. At Perform 4, you can control creatures and objects up to 2 meters cubed in size, allowing you to control Hylian-sized statues and the like. At Perform 8, you can control creatures and objects up to 5 meters cubed in size.

If you control an inanimate object, the object can animate by using any obvious joints, but does not gain any additional powers or durability. If the object does not have any suitable leg analogues, it can still hop along awkwardly (treating all terrain as rough terrain). You can see through any obvious eyes on the object, or use any other senses that a creature of its shape or description would obviously have - for instance, if you took control of a wolf statue, you might be able to detect scents you otherwise could not.

When attacking with a statue, use your Perform Trait in place of Combat, Willpower, and Agility. Typically, Statues have Health equal to ((Rank+1) x 10), and Defense equal to (Rank x 2), based on the Rank of the Materials they are made out of. You may use any weapons the statue obviously has (such as a stone sword, or a mouth with which to bite); treat these as weapons of the same Rank as the Statue's Rank.

If you control a willing creature, you can use that creature's abilities, equipment, Feats, Traits, Spells, and Techniques. You sense everything the creature does, using all of its senses. The creature's original mind is still present, and also senses and remembers everything occurring around it. The creature can end your control of it at any time, for any reason, whenever it likes. Further, if the creature ever reaches 0 Health, your control over it ends."""

table["Song of Communication"]="""Song of Communication
4 Magic (Burn)
Composable (Perform 4, 3 Tokens)

When learning this song, select a specific sentient being (such as a learned scholar, a Sage, or a dragon). Give this song a new name, signifying the target creature.

When played, the caster can communicate verbally with the song's specified being, as if they were adjacent to the caster; both sides can hear anything that the other party can hear. The song does not provide any visual of the target, or any information about their current whereabouts, and does not allow the two parties to transfer items to one another or to cast magic on one another: it is, essentially, a phone call.

The conversation may last for as long as both parties like, up until the next Extended Rest. Either party may end the conversation at will. However, a given creature may only be party to a single Song of Communication at a time; if contacted by another caster's Song, the target may choose whether to end their current communication or ignore the incoming one."""

table["Song of Companions"]="""Song of Companions
8 Magic (Burn)

When played, the caster and any allies near them may designate up to (Perform) Companions in total. Those designated Companions will then appear over the nearest ridge, just around the corner, or from behind some convenient obstacle, as if they had already been on their way, and merely happened to arrive just as the song finished.

The Companions designated in this song must already be registered to that Hero at a Stable or the like, having sworn their service to either the caster or one of their nearby allies. The Companions must themselves not have been in danger at the time, such as being in the midst of a battle, a cell in a prison, or a particularly high-stakes card game.

This song only works if the Companions in question could theoretically have safely travelled to the caster on their own, given infinite time: you can use Song of Companions to call a favorite horse from halfway across a continent, but not to call it to a kingdom of floating clouds, or across the sea, where it could never get to in a million years. Similarly, this song only works across Worlds if the Companions in question have their own means of crossing between Worlds.

If no Companions designated in the casting can reach the caster, then the song fails, and no Magic is spent."""
table["Song of Discovery"]="""Song of Discovery
12 Magic (Burn)

When you perform this song, the location of secrets within (Perform x 2) squares are revealed to the caster. Any objects that are hidden, locked away, or buried will glow to the caster's eye, even through stone or metal.

This song does not reveal what the hidden things are, nor how to get to them; just that there is something hidden or secret in that space. For example, if the object is in a safe, it will not reveal the combination, just that "there is something in the safe"; if there is a secret door, it will reveal that the door exists, and where in the wall it is, but not how to open it or where it leads."""

table["Elegy of Emptiness"]="""Elegy of Emptiness
8 Magic (Burn)

When you finish playing this song, you step forward one square. In the square you left behind, a statue facsimile of you appears. (If you cannot step forward one square, the song fails, and no Magic is spent.)

The statue facsimile of you is made of stone, is the same size as you, and weighs twice as much as you do. It can be moved, shoved, and picked up. At a glance, it appears to look like you, but any close inspection will reveal flaws and inconsistencies that clearly reveal its nature, both as a statue, and as not-quite-like-you. (To discern the difference, the observer must roll Perception v. your Perform.)

You may only have (1+(Perform / 3)) statues of yourself active at any one time. When you play this song again while at that limit, select one already-existing statue of you to make disappear. All statues created with this song disappear at the end of your next Extended Rest."""

table["Farore's Wind"]="""Farore's Wind
12 Magic (Burn)

The first time Farore's Wind is sung, the user marks their present location with a beacon of green light. The beacon hangs in the air for a moment, and then fades from view. If the user attempts to mark a location that is not safe or stationary, the spell fails (the Magic used is refunded) and the original marked location (if there is one) remains marked.

On subsequent casts, the user may choose to either change the location they have marked previously, or to transport themselves and up to (Perform) other persons with them to their marked location."""

table["Future's Fugue"]="""Future's Fugue
20 Magic (Burn)

When you perform this song, you enter a fugue state, where the mists of time envelop you and reveal insights into things that have not yet occurred.

While in this fugue state, you may ask up to (Perform/2) questions. These questions must relate to a specific action, event, or goal that has not yet happened. The answers to your question do not have to be known by any living or dead creature - the Goddess of Time (or perhaps time itself) is answering your question, and all things submit before time. However, the event in question must be occurring within the next 10 days, or the vagaries of causality will be too vast to make out any information of use.

When a question is asked, the performer is treated to a vision of what will happen, or what is most likely to happen. However, even visions of certain doom should not be taken as a guarantee - Heroes, in particular, have a long and storied history of thwarting the inevitable.

Once the performer has seen a vision, they may use their remaining questions to ask what might occur if they take a different course of action or change the circumstances of the event. They may also ask to see what caused such an outcome to occur, or to ask about a different event entirely."""

table["Minish Minuet"]="""Minish Minuet
12 Magic (Burn)

When you perform this song, you and up to (Perform x 2) of your compatriots transform from your normal size, to the size of the fabled Minish - for every foot tall you normally are, you are now only an inch tall (1/12th your normal size).

While at Minish size, your attacks can only deal 1 damage to normal-sized foes. You may find that things that were non-issues at your normal size - puddles, Critters, the wind - might be terrible obstacles or deadly foes at Minish size.

You may also use this song, at no Magic cost, to return to your normal size following a use of this song. This fails if there is not enough room to accommodate you and your compatriots at their normal size."""

table["Oath to Order"]="""Oath to Order
20 Magic (Burn)

When you learn this song, it is tied to a single divine entity (such as Farore, Goddess of Courage; the Giants of Terminia; or Eldin, guardian spirit of Eldin Province). It is also tied to at least one specific task, threat, or obstacle that the Divinity will help you overcome, of sufficient importance and difficulty as to warrant divine help.

When you play this song, that divine entity appears in a dazzling display, and sets about helping you conquer the obstacle, threat, or task that was specified. They may not be able to accomplish the entire task - often, even when the divine steps in, there is still call for Heroes - but the divine entity will do the best they can to ease the Heroes' burdens and open the path they need to follow.

If played without need, or without profit to the divine entity, they may take the false summons poorly. Beware the story of the boy who cried Wolfos.

This song may be learned multiple times, each time targeting a different divine entity. If a divine entity wishes to pledge its help to more than one task, threat, or obstacle, it does not need to teach a separate song for the different tasks - the divine entity may expand the scope of the song's designated tasks, but it cannot limit the scope."""

table["Song of Return"]="""Song of Return
8 Magic (Burn)
Composable (Perform 6, 6 Tokens)

When learning this song, select a specific location. The location must be generally safe and free from enemies and hazards (though that may not be a guarantee). Give this song a new name, signifying its target location.

When played, the performer and up to (Perform x 2) creatures, and their possessions, turn into motes of light, and drift into the breeze. They reappear at the song's specified location five minutes later, reassembling from motes of light over the course of about fifteen seconds.

The caster must be in the same World as the target location for this song to work: if they are not, then the song fails, and no Magic is expended.

Heroes may learn multiple versions of this song, each time targeting a different location."""

table["Royal Password"]="""Royal Password
(No Magic cost)

When learning this song, it is tied to a single group (such as a particular royal family, merchant house, or secret society). When you play this song, you clearly identify yourself as having the blessing of that group, both to creatures who work with or serve them, and to any doors, objects, or artifacts relating to that group. In short, playing this song at the right place, or for the right people, may open doors for you, both literally and metaphorically.

A curious feature of this song is its inimitability: even if one hears it a thousand times, they will not be able to replay it by ear. The song must be specifically taught to you, by someone who knows it and is 'in the group'.

If a person loses the favor of the group the song represents, and the group formally revokes their membership, the offender will be unable to play the song properly - it will be off-key, poorly-timed, and otherwise obviously incorrect.

Heroes may learn multiple versions of this song, each time representing a different group."""

table["Song of Soul"]="""Song of Soul
12 Magic (Burn)

When you play this song near a deceased body, the body's soul returns to its corpse. The soul is bound to the corpse for an amount of time based on your Perform rank. After the time expires, the soul automatically leaves its body. Check the book for details.

The corpse's capabilities depend on just how old it is, how well preserved, and how much of it is left - the Song of Soul does not heal or restore the corpse in any way. If the corpse still has muscle, it can move (and fight) as it did in life; if it's not well-preserved, it may have less Health than when it was alive; if it lacks a head, the corpse will not be able to navigate very well, lacking eyes.

The sole exception is the ability to speak: so long as there is at least a skull, the soul within will be able to speak aloud.

The Song of Soul does not force the soul to take any actions or to obey any commands - it still has free will, and may need to be cajoled, convinced, or otherwise forced to answer your questions or act on your behalf."""

table["Symphony of Sages"]="""Symphony of Sages
12 Magic (Burn)

When you perform this song, you and your allies gain an audience with an innumerable host of mystic sages, each with a deep knowledge of their favored subject; some may still be living, and others might have passed. The sages appear before you as ghostly figures, unable to physically interact with the world, and will leave if attacked or ignored. You may ask the sages a number of questions equal to (your Perform rank).

For the sages to be able to answer a question, the answer must be known by at least one sentient creature, living or dead, who would be willing to share it with your or those allied with you. The sages do not know what will happen in the future, and cannot make predictions any more accurately than a normal, reasonable person could. The sages' answers may not necessarily be easy to understand, but they will communicate to you in a language you know, and will attempt to make themselves understood - if a sage answers with something vague, it is because they themselves do not know the full answer, or because the vague answer is the most truthful or useful answer.

If your campaign has specific sages, or other mystical, scholarly, or powerful beings that the Heroes have met and allied with, you may instead use this song to 'conference call' them. In this case, no unknown host of sages appear; only sages you have met and allied with will answer your call, though they may also decline to participate if they are busy or indisposed. This allows Heroes to get specific insights from the sages, and allow them to speak with both yourself, your allies, and one another."""

table["Wind's Requiem"]="""Wind's Requiem
This song can only be played at the end of a Short or Extended Rest. When you play this song, select one of the following results and pay that Magic cost.

8 Magic (Burn), Requires Perform 3 - You may determine the result of the last Weather Check: whether the weather gets more intense, less intense, or stays at its present intensity. In addition, you may also determine the direction the wind will be blowing, however strong it may be. The wind will continue blowing in this direction until your next Short or Extended Rest.

16 Magic (Burn), Requires Perform 6 - You may determine the result of the last three Weather Checks: whether the weather gets more intense, less intense, or stays at its present intensity. In addition, you may also determine the direction the wind will be blowing, however strong it may be. The wind will continue blowing in this direction until your next Extended Rest.

24 Magic (Burn), Requires Perform 9 - You may select any Weather type, and any rank of that Weather. Over the next five minutes, the weather changes to match your selection, no matter how wild the change. If you chose a Weather type not normal for this area, then any Weather check that would result in the weather becoming more intense will instead keep it at its present intensity, and any Weather check result that would keep it at the same intensity will instead reduce its intensity. When the weather gets to Rank 0, it returns to the type of weather native to this area. In addition, you may also determine the direction the wind will be blowing, however strong it may be. The wind will continue blowing in this direction until your next Extended Rest."""

table["Song of the World"]="""Song of the World
16 Magic (Burn)
Composable (Perform 8, 9 Tokens)

When learning this song, select a specific World (such as "Light World", "Dark World", "World of Twilight", "Subrosia", etc.). Give this song a new name, signifying the target World.

When played, the performer and up to (Perform x 2) creatures, and their possessions, turn into silhouettes of rainbow colors; the targets see the world change in much the same way. The targets fade from their current World, and fade into being in the song's target World, at the same physical location. The whole process takes about a minute.

The caster must not be in the same World as the target location for this song to work. The location the caster and all targets are standing in must also be free of rock and solid matter in the target world. If these conditions are failed, then the song fails, and no Magic is expended.

Heroes may learn multiple versions of this song, each time targeting a different World."""

table["Boosted Trait"]="""Boosted Trait:
 Boosts always come with a rank. The Trait(s) in question is/are increased by 2 for each Rank of the effect. (You cannot use a boosted Trait to qualify for Token purchases.)"""

table["Blinded"]="""Blinded:
While Blinded, you cannot see clearly. Subtract 5 from any To-Hit checks you make. Being Blinded ends at the end of your next turn, unless otherwise specified. Mini-Bosses and Bosses subtract 2 from their To-Hit Checks, instead."""

table["Burning"]="""Burning:
While a weapon is Burning, all attacks with it are Critical Hits, and do Fire-type damage. It also casts light as a lit torch of its Rank would. However, the weapon loses 1 Durability at the start of your turn, and loses 1 Durability from every foe it hits. A Burning weapon can be extinguished as a Minor Action, or as part of storing it in your pack. Burning weapons are also extinguished when you are hit with a Water-type attack, or when you enter the water. Only Wooden weapons can begin Burning."""

table["Cursed"]="""Cursed:
While you are Cursed, you cannot use any Basic Attacks, Techniques, or Spells. You can still move, use items, speak, perform Combat Maneuvers, and otherwise take action. Mini-Bosses and Bosses instead do 5 less damage with their Basic Attacks, Techniques, and Spells, but suffer no limits on what they can and cannot do from being Cursed. Being Cursed lasts until the end of your next turn, unless stated."""

table["Electrified"]="""Electrified:
When Electrified, any metallic weapons that come in contact with you will conduct electricity into their user. Anyone who attacks you with a metallic melee weapon suffers Shock damage equal to the damage they inflict upon you. This damage is not modified by your Defense, though it is modified by your weakness or resistance to Shock."""

table["Frozen"]="""Frozen:
While Frozen, you cannot take actions or move. The next attack to connect with you deals Critical damage, and releases you from being Frozen. You may attempt to wriggle yourself free during your turn by taking a Minor or Standard Action, spending an amount of Stamina you choose, and rolling 2d6 plus the Stamina you spent; a roll of 10 or more allows you to escape. Mini-Bosses and Bosses are Slowed when Frozen, instead of rendered entirely unable to act, with everything else proceeding as normal."""

table["Halted"]="""Halted:
While Halted, you cannot move, but you can still take actions and use items or Techniques or Spells. Typically, being Halted lasts until you free yourself of the hazard that is halting you."""

table["Hasty"]="""Hasty:
Boosts always come with a Rank. Hasty increases your Movement by 1 square per rank. Hasty also increases your Initiative by 2 for each Rank."""

table["Invisibility"]="""Invisibility:
While Invisible, enemies have difficulty targeting you, and effects based on sight do not trigger for you. However, you still cast a shadow, allowing perceptive foes to know your location. If you are Invisible, add +3 to your Evasion. Typically, Invisibility ends once you are struck or you make an attack."""

table["Mighty"]="""Mighty:
Boosts always come with a Rank. When you deal damage, increase the damage you do by 25\% for each Rank of the effect, after factoring in the target's armor and any other defenses."""

table["Off Balance"]="""Off Balance:
While Off Balance, you cannot use Reaction abilities, nor can you use Combat Maneuvers. You can still use Basic Attacks, Techniques, Spells, swap gear, move, and do other things. Unless the inflicting attack or effect says otherwise, Off Balance ends at the beginning of your next turn."""

table["On Fire"]="""On Fire:
While you are On Fire, any wooden weapons that come into contact with you (including any you are holding, but not those stored in your pack) instantly begin Burning. While you are On Fire, you suffer 4 Fire damage per turn, taken at the start of your turn. This damage is not modified by your Armor, though it is modified by your weakness or resistance to Fire."""

table["Prone"]="""Prone:
While Prone, you are flat on the ground. It takes half of your Movement to stand up. While Prone, you cannot move around or make attacks, but you can still perform other actions."""

table["Resistant"]="""Resistant:
Resistance always specifies the type of damage you are susceptible to, such as "Shock" or "Piercing". Resistance has three levels. When you take damage from an attack of that type, reduce the damage you take from that attack by 25\% at Lv. 1, 50\% at Lv. 2, or 75\% at Lv. 3, after factoring in your armor and any other defenses. (If you are both Resistant and Weak to the attack, then ranks of Weakness cancel out ranks of Resistance.)"""

table["Rusted"]="""Rusted:
While a weapon is Rusted, its Attack Power is cut in half. Only Metal weapons can become Rusted. Rust persists until the rust is removed by applying Oil."""

table["Sand-Covered"]="""Sand-Covered:
While Sand-Covered, your weapons take twice the Durability damage from being used. Being Sand-Covered typically ends when you take a short or extended rest, when you are struck with a Water-element attack, or when you are Soaked."""

table["Sick"]="""Sick:
While Sick, every further action causes you pain. Whenever you perform a Standard Action, you suffer 4 damage after resolving that action. This damage cannot be mitigated by armor. Sickness persists until you restore Health (such as through magic, rest, or consuming food)."""

table["Slowed"]="""Slowed:
While Slowed, you find movement more difficult (but not impossible). Treat all terrain as Difficult Terrain - whether or not it would normally be, and whether or not you could normally walk through Difficult Terrain of that type."""

table["Soaked"]="""Soaked:
While Soaked, any Shock attacks that strike you turn into a Burst 1 that include you (you are only struck once by the attack, regardless). Further, any Ice attacks that hit you will automatically inflict Freezing on you. If you were previously On Fire or your weapons were Burning, they are now extinguished."""

table["Tough"]="""Tough:
Boosts always come with a Rank. When you suffer damage, reduce the damage you receive by 25\% for each Rank of the effect, after factoring in your armor and any other defenses."""

table["Weakness"]="""Weakness:
Weakness always specifies the type of damage you are susceptible to, such as "Shock" or "Piercing". Weakness has three levels. When you take damage from an attack of that type, increase the damage you take from that attack by 25\% at Lv. 1, 50\% at Lv. 2, or 75\% at Lv. 3, after factoring in your armor and any other defenses. (If you are both Resistant and Weak to the attack, then ranks of Weakness cancel out ranks of Resistance.)"""

table["Cultural Monuments"]="""Cultural Monuments
Cultural Monuments are a special kind of building - they make no profits on their own, but they enhance the culture and prosperity of the community around them. 
Museums, Obelisks, Statues, and Temples are all examples of kinds of Cultural Monuments. Sites of historical importance, such as old battlegrounds or tombs of important historical figures, might also be built up and turned into Cultural Monuments.
Cultural Monuments provide a bonus to the Rupees earned by the Owners of Buildings that are adjacent to them, based on the Rank of the Statue. These bonus profits are in addition to whatever profits (Rupee, Material, or otherwise) that a Building might earn for its Owners.
Rank 0–5
Rank 1–10
Rank 2–20
Rank 3–40
Rank 4–100
Rank 5–200
A single Cultural Monument can only be adjacent to (and thus, can only benefit) up to 8 different Buildings, and a single Building can only benefit from up to 4 different Cultural Monuments. Cultural Monuments may also provide other benefits to the community, such as increased tourism in the area, or prestige for the community."""

table["Cultural Wonder"]="""Cultural Wonder
A Cultural Wonder is a special kind of Cultural Monument. It represents the power, wisdom, or courage of the entire community, and can draw admirers from all corners of the world.
A Cultural Wonder can be built like any Cultural Monument, but costs 5 times the Materials in each category. Cultural Wonders tend to be quite massive: temples that can be mistaken for mountains, a statue of a goddess at full scale, or a network of catacombs the size of a city."""

table["Bank"]="""Bank
Banks provide item storage, at 25 Rupees per slot. Banks can provide up to (Rank x 4) slots of Storage per person. Each slot of Storage acts just like a slot of Overflow Inventory - any kind of item may go into it, and items that stack may stack as large as they like. A Bank’s Owners have their available item storage at the Bank doubled: for example, if they have purchased 3 slots, then they have 6 available.
Banks can also store your Rupees, to prevent them from being stolen or eaten by Like-Likes. Storing your Rupees costs nothing, and you do not need to purchase or use a Storage slot for them. You may withdraw your Rupees or your items whenever you visit the Bank in person. You may also withdraw items and Rupees from the Bank whenever you communicate with them, such as by mail - so long as you’re willing to deal with the time it takes for the post to deliver."""

table["Enchanter"]="""Enchanter
Enchanters can enchant weapons and armor, for a price.
Enchanters can perform any Enchantment with a required Enchanting Trait Rank of (Building Rank x2) or lower. Enchanters do not provide the Gems necessary for Enchantment: you must bring your own Gems for the Enchanter to use. If you wish to buy Gems, you will have to find a Gem Store and purchase from them.
Enchanting 2–200
Enchanting 4–400
Enchanting 6–600
Enchanting 8–800
Enchanting 10–1,000
An Enchanter’s Owners get a small portion of their earnings, based on the Rank of the shop."""

table["Farm"]="""Farm
There are two types of Farms: those that provide Food Ingredients, and those that provide Critters. Both types of Farm provide their Ingredients to the community.
Most farms specialize in a certain type of plant or animal that they tend to - some food farms may grow wheat, while others rear cattle or pigs, and some grow fruits or vegetables.
A Farm will often sell its wares to anyone who comes by - typically, ((Rank+1)/2) different types of Food or Critter Ingredient (of a type that the farm would logically produce), of a Rank up to the Farm’s Rank. Thus, a Rank 3 Food Farm could sell two different kinds of Rank 3 Food Materials; a Rank 2 Critter Farm could sell two different kinds of Rank 2 Critters. These Materials may be limited by season or circumstance: for instance, if the Farm specializes in summer vegetables, it may not have anything to sell in the winter, or might sell different things.
For their Owners, Farms provide 1 unit either Food Ingredients or Critters (depending on type), of a Rank equal to the Building’s Rank, and of a species that the farm produces, per day."""

table["Forge"]="""Forge
Forges allow people to craft Weapons and Armor. 
Forges also allow you to provide your own materials, and they can make weapons and armor for you from them - for a price. Forges can only make items of the Building’s Rank or lower. Weapons made by a Forge’s attendants may be of a Crafting Style that the Forge’s attendants know - for instance, Weapons made at a Forge staffed by a Goron might be of the Goron style. 
Smithing 0 (Rank 0 Items)–100
Smithing 2 (Rank 1 Items)–200
Smithing 4 (Rank 2 Items)–400
Smithing 6 (Rank 3 Items)–600
Smithing 8 (Rank 4 Items)–800
Smithing 10 (Rank 5 Items)–1,000
A Forge’s Owners get a small portion of their earnings, based on the Rank of the shop."""

table["Healer"]="""Healer
Healers craft Elixirs from ingredients that people bring them - for a price. They also provide medical advice and knowledge to the community, serving as doctors, midwives, and surgeons.
A Healer can only create Elixirs with a number of Ingredients equal to the Rank of the Building.
Cooking 2 (1 Ingredient)–20
Cooking 4 (2 Ingredients)–40
Cooking 6 (3 Ingredients)–60
Cooking 8 (4 Ingredients)–80
Cooking 10 (5 Ingredients)–100
A Healer’s Owners get a small portion of their earnings, based on the Rank of the shop."""

table["House"]="""House
A House’s Owner can rent out the house, allowing others to live in it. When they do this, the Owner receives a small amount of Rupees per day based on the Rank of the House, much as if it were a Shop or other Building. However, an Owner cannot then use a rented-out House for their own purposes, including as a place to rest.
If a House’s Owner does not rent out the house, they may use it for themselves. This allows them to fill it with their own belongings, customize its colors and furnishings, and to rest there for free. When resting in a House, Heroes restore all of their Burnt and Spent Health, Magic, and Stamina, as well as gaining an amount of Temporary HP based on the Rank of the House.
Houses include a reasonable suite of furniture to fill the space - so a House always includes a bed, table, chairs, and so on. A House’s owners are free to obtain and use their own furniture, as well.
All Houses contain a kitchen, which includes a (non-portable) cooking vessel and a place to have a fire. This means that residents in a House can use it to cook without further expenditure. Houses do not come equipped with a Forge, however.
Houses can be used to store items, containing (2 x Rank) slots. Each slot of Storage acts just like a slot of Overflow Inventory - any kind of item may go into it, and items that stack may stack as large as they like.
Be cautious of storing a large number of valuable items or materials in your home - Houses are easier to break into, and are generally less well-guarded, than a Bank of equal rank. Further, the more valuable the goods in the home, the more likely it is that thieves will strike, breaking in while any resident Heroes are away and looting the place. If you have a large number of valuable objects you wish to store for a long time, buying space at a Bank may be a better, safer option.
House Rank–Temp HP on Rest–Item Storage Slots
Rank 0–0–0
Rank 1–4–2
Rank 2–8–4
Rank 3–12–6
Rank 4–16–8
Rank 5–20–10"""

table["Renting a House"]="""Renting a House
Sometimes, Heroes may be staying in an area long enough that they won’t want to purchase a stay in an Inn room, but will not be staying long enough (or may not be rich enough) to outright buy or build their own House. In this case, they can choose to rent a House that someone else owns. 
The cost for doing so is the same as the profits the Owner of a House would make per day. However, Houses must be rented for a minimum of 5 days at a time, and are rented in increments of 5 days thereafter - so Inn rooms are more efficient if you intend to spend only a night or two in town, or if you need to be able to leave the moment that duty calls. 
Not every community will have Houses for rent, either - it’s not a guarantee, especially in smaller communities where there are fewer travelers and little-to-no real estate market."""

table["Inn"]="""Inn
Inns allow travellers, Heroes, and tourists to stay the night away from their own beds and without camping outdoors, for the cost of a per-day fee.
An Inn’s comfortable beds and luxurious services provide a full night’s rest, restoring all Spent and Burned Health, Magic, and Stamina. It also provides an amount of Temporary HP to anyone who rests there, based on the Rank of the Inn. A single Room can support up to 4 Heroes comfortably, and includes a nourishing (albeit benefit-less) breakfast.
All Inns provide a place for Heroes to cook their own Dishes, as well: smaller Inns might just have a single communal cooking area, while fancier Inns may provide each room with their own private kitchen.
Building Rank–Cost–Temp HP
Rank 0–25–0
Rank 1–50–4
Rank 2–100–8
Rank 3–200–12
Rank 4–500–16
Rank 5–1000–20
An Inn’s Owners get a small portion of their earnings, based on the Rank of the Inn."""

table["Library / Book Store"]="""Library / Book Store
Libraries and Book Stores both serve a similar purpose - to give their community access to literature and knowledge.
Both have Written Books of a quality up to the shop’s Rank. The Written Books they have available depends on the building’s owners, location, culture, and a host of other factors, as the GM decides. Neither Libraries nor Book Stores sell Empty Books or Ink & Quill sets; to purchase those, visit (or build!) a Tool Shop with the appropriate wares.
Book Stores sell their Books directly. Libraries, however, only loan their Books out. A Library will loan out a single Book per person, for a period of 10 days. After that, they begin charging late fees, accruing every day until the Book is returned, equal to 10% of the Book’s sale cost (typically 20% of the Book’s Market Price). Heroes are unable to borrow books from a Library while they have an outstanding late fee. If they fail to return a valuable enough Book for a long enough time, some Libraries may decide to take more drastic measures to regain their property.
A Library’s or Book Store’s Owners get a small portion of their earnings, based on the Rank of the Building. They may instead rarely receive a random free Written Book of the Building’s Rank instead of a day’s profits."""

table["Minigame"]="""Minigame
Minigames might take two forms: an actual small game that the GM invents, or an Extended Challenge. As an Extended Challenge, they last 3 Rounds, and may either be for an individual Hero or an entire party; they may be for whatever Trait or Traits make the most sense for how the Minigame is described.
A Minigame’s Owners can decide just what game they offer, but a community can only support a single instance of any one game.
Any given Minigame can only be attempted by a given Hero once per day.
The higher Rank the Minigame is, the more levels it offers - providing a higher DC to meet, but granting a higher reward for doing so. Heroes may choose what Difficulty they want to try the Minigame at when they attempt it.
To play a Minigame, the Hero making the attempt must first pay up a certain amount of money based on the level of the Mini-Game they’re going to play. When they win the Minigame, Heroes typically win back twice what they paid (or items of that value), though they may instead win other prizes, based on the individual running the Minigame and circumstances.
Building Rank–Minigame Level–Cost
Rank 0–Novice–20
Rank 1–Journeyman–50
Rank 2–Adept–100
Rank 3–Expert–200
Rank 4–Impressive–500
Rank 5–Heroic–1,000"""

table["Observatory / Fortune Teller"]="""Observatory / Fortune Teller
Observatories gaze up to the stars, while Fortune Tellers gaze into crystal balls. The result is the same - they both attempt to predict the future.
Observatories and Fortune Tellers can be employed to perform the Song “Future’s Fugue”, with a Perform Trait equal to (Building Rank x2). The exact method of their performance may vary - be it tea leaves, careful charting of the stars, or even stranger actions. The person performing the augury will relay the information they receive as best and as truthfully as they can, and will ask the mists of time the questions posed to them by customers faithfully and accurately - though they may not always understand the answers they receive from the mists of time.
Rank 1–100
Rank 2–200
Rank 3–400
Rank 4–1000
Rank 5–2000
An Observatory or Fortune Teller’s Owners get a small portion of their earnings, based on the Rank of the Building."""

table["Post Office"]="""Post Office
Post Offices can deliver letters, for 10 Rupees, or packages, for 25 Rupees. A letter may only contain written correspondence, while a package may contain both a letter and a single item (or stack of a single item).
All postal deliveries take one day to reach their destination. Typically, if you’re expecting a response, it will take about three days - one day to deliver your missive, one day for the recipient to compose a response, and a third day for their response to reach you.
All deliveries must start at the post office (there is no daily at-home pickup). Postmen can deliver mail to specific addresses, such as homes or businesses. They can also deliver directly to a person’s hands, if they catch sight of them on their route. However, no Post Office can deliver to a place it is unaware of, or one that is hidden away or shielded by magic - if a normal person (with directions and proper conveyance) couldn’t get there, then neither can the postman.
At Rank 0, there aren’t any actual postmen to make deliveries - the Post Office is simply a place where messages can be dropped off and picked up. People wishing to exchange messages will have to visit the Post Office to receive their mail, or to send replies.
At Rank 1, the Post Office becomes capable of making simple deliveries. Their messengers can deliver letters and packages to anyone within a small radius around the Post Office - about the size of a small village.
At Rank 2, the Post Office’s effective radius increases, to about the size of a large city, or a village and much of its surrounding countryside. At Rank 3, a Post Office can deliver to most anywhere within a country.
At Rank 4, the Post Office is capable of delivering packages and letter to nearly anywhere in the world, by land or sea (or in some cases, air). The postmen will use magical means to make the journey, if necessary. Fantastical realms that are still in the same metaphysical world, such as a kingdom in the clouds or hidden deep in the earth, may or may not be available to deliver to.
At Rank 5, these postmen are capable of traveling to anywhere in the world, or any other world, and deliver any message or package within a day. They can shuttle messages between the Light and Dark World, the Twilight Kingdom, or any other worlds your campaign might have.
A Post Office’s Owners get a small portion of their earnings, based on the Rank of the Post Office. However, some days (at the GM’s whims) their earnings may be replaced by receiving a random item from the ‘undeliverable packages’ pile. A Post Office’s Owners may also send and receive (Rank) letters or packages per day, for free."""

table["Shop"]="""Shop
Shops offer mundane items, of a Rank equal to the Rank of the shop.
Different kinds of shops sell different kinds of things.
Unless otherwise noted, all Shops have enough stock at-hand to satisfy all but the most ludicrous order a Hero could place. This may not be the case if a Shop is suffering in some way, such as from political sanctions, stolen or lost deliveries, or if they’re in a city under siege.
A Shop’s Owners get a small portion of their earnings, based on the Rank of the shop.
Stables
Stables allow Heroes to register their Mounts and Companions for 50 Rupees. Mounts and Companions registered at a Stable may reside there for as long as their Hero likes, allowing said Hero to take a different Mount or Companion with them on their journey. Mounts are well-fed and exercised while at a Stable, and Companions will typically find benign ways to occupy their time, either by working at the Stable, or enjoying the surrounding town or countryside.
Further, Heroes may use a Stable to summon a Mount or Companion residing at a different Stable, in order to make use of them - in some games, the Mount or Companion in question may have to physically traverse the distance themselves, while in others, Stables can magically transport Companions and Mounts through the power of loyalty and magic.
In addition, Stables provide any stabled Mounts and Companions with temporary HP, based on the quality of the Building, when the Companion is stabled at the Building for an Extended Rest.
Rank 0–0
Rank 1–4
Rank 2–8
Rank 3–12
Rank 4–16
Rank 5–20
A Stable’s Owners get a small portion of their earnings, based on the Rank of the shop. """

table["Towers & Walls"]="""Towers & Walls
Towers and Walls serve as fortifications for a community. Walls go on the outside of the community, while Towers serve as strategic garrisons and lookout points.
Walls must be in a relatively straight line (with some leeway for following terrain). This means that most towns that choose to surround themselves with walls will have to build at least four of them. Walls are (Rank+1) storeys tall, but their length is simply “an entire side of the town” or less. Walls may feature gates, allowing passage to authorized persons at a specific place along the wall. These gates may be closed and locked to secure the wall when passage is not permitted, such as at night or during times of war.
Towers can be built atop Walls, or on their own, as guard outposts or lookout posts. Towers are ((Rank+1) x 2) storeys tall, and often feature portholes to fire arrows from, and a balcony that allows the placement of siege weapons or regiments of soldiers. Towers are the tallest buildings in most communities, rivaled only by Cultural Monuments.
Castles can be built by combining an appropriate number of Walls and Towers (typically 4 of each). Castles built within or nearby a town serve as a second line of defense, should the town be taken in war."""

with open('bookdata.json','w') as data_file:    
    json.dump(table,data_file,indent=4)