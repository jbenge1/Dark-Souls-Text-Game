%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%									       %	
%                  @author    : Justin Benge, Christian Mancha		       %
%		   date       : 27 Nov, 2017				       %
%                  filename   : prog.pl					       %
%		   Description: this will be a simple text based version of    %
%		    dark souls 3 as I enjoy the game 			       %
%									       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%% TODO %%%%%%%
% 1.) Add in more areas the dungeons


% load up the file to read in sentences
:- tty_clear,write("Type start, to start the game."),nl.

% The following lines, tell prolog that we have permission to 
% change the facts has, curLoc, and itemPos, and alive.
:- dynamic has/3, curLoc/1, itemPos/3, alive/1, boss_room/3, npc/4,connected/3.
% This tells prolog that we have a fact that shows up in multiple places
:- discontiguous alive/1, describe/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DELETE THIS SECTION FOR FINAL VERSION, IT IS MERELY SOME
%% FACTS TO MAKE TESTING EASER 
%:- discontiguous has/3.
%has(vordt_soul, soul, player).
%has(transposing_kiln, key, ludleth).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Currently the only bosses I will implement are....
%    Vordt of the boreal valley
%    Cursed Rotted Greatwood
%    Crystal Wizard
%	(Hopefully this list will grow over time but I want to start of simple

% Set up the player

%
% This represents our starting location, we shall start in the
% firelink shrine....
curLoc(firelink_shrine).
has(broken_straight_sword, weapon, player).
has(loin_cloth,armor, player).
%has(transposing_kiln,key,ludleth).
% make sure the player is alive.
alive(player).
%% Predicate to list what the player has
inventory:-
	has(X,_, player),
	write_item(X),
	nl,
	fail.
inventory:-!.
%% End player setup.
%% First lets create a whole bunch of places and their names
% ( Not a complete list)
dungeon(firelink_shrine, "Firelink Shrine").
dungeon(high_wall_of_lothric, "High Wall of Lothric").
dungeon(undead_settlement, "Undead Settlement").
dungeon(road_of_sacrafices, "Road of Sacrafices").
dungeon(vordt_boreal_valley_room, "Vordt's chamber").
dungeon(cursed_rotted_greatwood_room, "Cursed Rotted Greatwood").
dungeon(crystal_sage_room, "Room of the Crystal Sage").
dungeon(cathedral_of_the_deep, "Cathedral of the Deep").
dungeon(cemetery_of_ash, "Cemetery of Ash").
dungeon(kiln_of_the_first_flame, "Kiln of the First Flame").
dungeon(lohtric_castle, "Lothric Castle").
dungeon(consumed_kings_garden, "Consumed Kings Garden").


%% Start map set up

% Now to create facts for how all the places are connected
% in the form : connected(from, direction, to)
%
% boss rooms will be a seperate location from the actual
% location they belong to 
% ie: vordt belongs to high_wall... but in the database he will
% actually be in vordt_boreal_valley with only one way in from high_wall
% using the fog_door command


%% firelink shrine is connected to high wall of lotrhic
%% as well as to the cemetery of ash (though I may have to implement
%% this later).... (same as kiln of the first flame)
connected(firelink_shrine, east, high_wall_of_lothric).
connected(firelink_shrine, west, cemetery_of_ash):-
	has(dragon_stone). % this is a bit of an over simplification of
			   % how to get here but its fine for now
connected(firelinke_shrine, fire_link , kiln_of_the_first_flame):-
	has(abyss_watcher_soul),
	has(aldrich_soul),
	has(yohrm_soul),
	has(lothric_young_prince_soul).


%% high wall is connected to...
%    firelink shrine -> so the player can go back in the map
%    undead settlement
%    consumed kings garden (optional area)
%    lothric castle 
%    vordt of the boreal valley (boss room)
connected(high_wall_of_lothric, fog_door, vordt_boreal_valley_room).
connected(high_wall_of_lothric, west    , firelink_shrine).
connected(high_wall_of_lothric, north   , lothric_castle):-
	has(dancer_soul,soul, player).
% rather than have the actual dungeon connected, you have to go through the boss room
connected(vordt_boreal_valley_room, east    , undead_settlement):-
	has(vordt_soul,soul, player).
connected(vordt_boreal_valley_room, west, high_wall_of_lothric).
connected(high_wall_of_lothric, south, consumed_kings_garden):-
	has(dancer_soul, soul, player).

%% Since the next place we should go is undead settlement lets go there now
%% it is connected to...
%    road of sacrafices
%    high wall of lothric -> so player can go back through map
connected(undead_settlement, east    , road_of_sacrafices).
connected(undead_settlement, west    , vordt_boreal_valley_room).
connected(undead_settlement, fog_door, cursed_rotted_greatwood_room).
connected(cursed_rotted_greatwood_room, west, undead_settlement).
%% Road of sacrafices connected too...
%    cathedral of the deep
%    farron keep
%    undead settlement
%  Note no souls are needed to get here so the cursed greatwood is an optional
% boss just as it was in the game
connected(road_of_sacrafices, fog_door, crystal_sage_room).
connected(road_of_sacrafices, west    , undead_settlement).
connected(crystal_sage_room, north   , cathedral_of_the_deep):-
	has(crystal_sage_soul,soul,player).
%% Now for cathedral of the deep...
%    road of sacrafices

%% now a predicate to print out the names
print_dungeon_name(X):-
	dungeon(X, N),
	!,
	write(N).


%% The following are npc's in the world, they have a name, a location, and a number associated with them. 
%  The number mereley represents
%  how many times we have spoken with them 
%  the last number represents the number of unique dialogues they have
npc(ludleth_of_courland, firelink_shrine, 0, 1).
npc(firekeeper,firelink_shrine, 0, 4).

npc_dialogue(ludleth_of_courland, 0, "This is test 0").
npc_dialogue(ludleth_of_courland, 1, "This is test 1").

npc_name([ludleth,of,courland], ludleth_of_courland).

%% Describe how we talk to npc's in the world
%% This predicate is for if we have exahusted the dialogue
talk(Person):-
	npc(Person, Place, N, N),
	curLoc(Place),
	npc_dialogue(Person,N,X),
	!,
	write(X).
%% This one is if the person still has more to say
talk(Person):-
	npc(Person,Place, N, Z),
	curLoc(Place),
	npc_dialogue(Person, N, X),
	!,
	retract(npc(Person,Place, N,Z)),
	N1 is N + 1,
	assert(npc(Person,Place, N1, Z)),
	write(X).
	
talk(Person):-
	npc(Person,_,_,_),
	!,
	write("Sorry but that NPC is not in this area, try searching around the universe for them"),
	nl.
talk(_):-
	write("Sorry but that NPC does not exist in Lothric"),
	nl.
	
% we want to be able to speak with people

%% Set up items in the map 
%
% The follwing facts describe an items location relatvie to the map
% the boss souls will not be in the boss rooms, they will just be awarded
% to the player when they boss dies...
% items will be in the form: itemPos(item,type, location)
% ie a short sword can be found in the high wall...
%    itemPos(short_sword, high_wall_of_lothric)
%
%
% for the current version ie Version 1.0 I will not have shield logic
itemPos(short_sword  , weapon,  high_wall_of_lothric).
itemPos(leather_armor, armor ,  high_wall_of_lothric).
itemPos(long_sword   , weapon,  undead_settlement).
itemPos(chain_armor  , armor ,  road_of_sacrafices).
itemPos(vordts_hammer, weapon,  special).




%% This names the items and prints out a nice name without underscores
itemName(broken_straight_sword, "Broken straight Sword").
itemName(loin_cloth, "Loin Cloth").

%% Weapons
itemName(short_sword, "short Sword").
itemName(long_sword,  "long Sword").
itemName(vordts_hammer, "vordt of the boreal valley's hammer").
itemName(hollow_slayer_greatsword, "hollow slayer greatsword").
%% Armor
itemName(leather_armor, "Leather Armor").
itemName(chain_armor, "Chain armor").
%% Key items
itemName(vordt_soul,  'soul of vordt').
itemName(rotten_soul, 'rotten soul').
itemName(transposing_kiln, "transposing kiln").


item_rating(broken_straight_sword, 0).
item_rating(short_sword, 1).
item_rating(long_sword , 2).
item_rating(vordts_hammer, 4).
item_rating(hollow_slayer_greatsword,4).

%% Used to write the name of the item
write_item(Item):-
	itemName(Item, Name),
	!,
	write(Name).
%% end items dispersement

%% Set up bosses
% 
% the actual bosses and their names
boss(vordt_of_the_boreal_valley, "Vordt of the Boreal Valley").
boss(cursed_rotted_greatwood, "Cursed Rotted Greatwood").
boss(crystal_sage, "Crystal Sage").
% the following facts just define the fact that the bosses are all
% alive and well 
% I will set up all the bosses for the game regardless of whether or not
% the dungeons have been set up
%
%  BOSS                             LOCATION
alive(vordt_of_the_boreal_valley). % high wall
alive(cursed_rotted_greatwood).    % undead settlement
alive(crystal_sage). 		   % road of sacrafices
alive(deacons_of_the_deep).        % cathedral of the deep
alive(abyss_watchers).             % farron keep
alive(high_lord_wolnir).	   % catacombs of carthus
alive(pontiff_sullivan).           % irithyll of the boreal valley
alive(yohrm_the_giant).	 	   % profaned capital
alive(aldrich_devourer_of_gods).   % anor londo
alive(nameless_king).              % archdragon peak
alive(king_of_storms).             % archdragon peak
alive(ancient_wyvern).		   % archdragon peak
alive(oceiros_the_consumed_king).  % consumed kings garden
alive(champion_gundyr).		   % untened graves
alive(lothric_first).		   % grand archvies
alive(lotrhic_and_lorian).	   % grand archives
alive(soul_of_cinder).             % kiln of the first flame
alive(dragonslayer_armor).         % lothric castle
alive(old_demon_king).             % smouldering lake
%% Predicate so we can not attack bosses from outside boss room
%boss_room(location, boss soul, boss, weapon rating kill boss, death message)
% I changed the fourth value to be a number, so if our weapon has >= to boss_rating we
% can kill it. This means I will have to assign a rating to each weapon
boss_room(vordt_boreal_valley_room, vordt_soul, vordt_of_the_boreal_valley, 1, "Vordt killed you").
boss_room(cursed_rotted_greatwood_room, rotten_soul, cursed_rotted_greatwood, 1, "The greatwood killed you").
%% I will leave this as 3 so the player has to transpose one of the boss souls to win
boss_room(crystal_sage_room, crystal_sage_soul, crystal_sage, 3,"The Crystal Sage has killed you").
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%boss_room().
%% The following predicates describe what weapons are given by boss souls
boss_weapon(vordt_soul,weapon, vordts_hammer).
boss_weapon(rotten_soul, weapon, hollow_slayer_greatsword).



% Write out the name of the boss
write_boss(X):-
	boss(X,Y),
	!,
	write(Y),
	nl.


	%% End boss setup..
%% END MAP SETUP %%

% Now for some predicates describing how to interact with the world..
%
% The pick_up predicate decides if you have one of these items in your inventory
% as I will not allow for duplicates it is of the form
%     pick_up(item).
% weapons and armors will be in the same item class
% also note that in v1.0 you will only be able to hold one item at a time
% and have one armor at a time
%
%this first predicate is for trying to pick up duplicate items
pick_up(Item):-
	has(Item,_, player),
	write("You already have "),
	write_item(Item),
	nl,
	!.
%% This is for picking up an item that the player does not currently have,
%  and it makes sure the player is in the correct location. However once
%  the player picks it up, it is irreversible
pick_up(Item):-
	has(Item2,Type,player),
	curLoc(Place),
	itemPos(Item,Type, Place),
	write("Picking up "),
	write_item(Item),
	nl,
	retract(itemPos(Item,Type, Place)),  
	retract(has(Item2,Type,player)),
	assert(has(Item,Type,player)),		
	nl,
	!.

pick_up(_):-
	write("I do not see that item here"),
	nl.

%% this predicate describes how to drop an item
%  may be useful if we want to pick up a seperate item
drop(Item):-
	has(Item,_,player),
	curLoc(Place),
	retract(has(Item,_,player)),
	assert(itemPos(Item,_, Place)),
	write("dropped "),
	write(Item),
	nl,!.
drop(_):-
	write("That item does not exist in your inventory!!!"),	
	nl.

% Now to define how we move through out the various areas(dungeons)
%
% To move north
north :- go(north).
% To move south
south :- go(south).
% To move west
west :- go(west).
% To move east
east :- go(east).
% To through fog door
fog_door :- go(fog_door).

%% Now define the actual movement steps using the go predicate
%
% This looks finds your current location in the map, and then 
% finds the area connected to it in the direction Direction
% and prints out a message telling us that we have arrived

%% If you try to leave a boss room when the boss is still alive
go([_]):-
	curLoc(Here),
	boss_room(Here,_,Boss,_,X),
	alive(Boss), % is this check necessary? since we quit the second we die....
	!,
	write(X),
	die.
go(Direction):-
	curLoc(Here),
	connected(Here, Direction, There),
	retract(curLoc(Here)),
	assert(curLoc(There)),
	write("You have arrived at "),
	print_dungeon_name(There),
	!.
go([Direction]):-
	curLoc(Here),
	connected(Here, Direction, There),
	retract(curLoc(Here)),
	assert(curLoc(There)),
	write("You have arrived at "),
	print_dungeon_name(There),
	!.
go(_):-
	write("You cannot 
%% I will leave this as 3 so the player has to transpose one of the boss souls to wingo that way... Either insufficient items or no path connecting the two areas."),
	nl.

%% Now we need to write a predicate such that the player can look
%  around the area for anything interesting.
% It only describes the area, and is up to the player to notice intersting
% things using the inspect_further command.
look:-
	curLoc(Place),
	describe(Place),
	nl.

inspect_further:-
	curLoc(Place),
	itemPos(Item, _, Place),
	write("Upon further inspection you see a "),
	write_item(Item),
	write(" here"),
	nl,	
	fail.
inspect_further.


%% Now for some rules on how to kill the different bosses
%%% I think I will merely require certain items to kill the bosses 
%%% rather than a health bar...



%% Now some predicates describing how a player dies
die :-
	!,
	retract(alive(player)),
	write("\t\t\t\t___   ___  ___  ___ "),nl, 
	write("    \\\\   //   __                |  \\   |   |    |  \\  "),nl,
	write("     \\\\ //   /  \\   |   |       |   |  |   |_   |   |  "),nl,
	write("      |||    |  |   |   |       |   /  |   |    |   / "), nl,
	write("      |||    \\__/   \\___/       |__/  _|_  |__  |__/ "), nl,
	nl,
	write("If you would like to play again, reload swipl and start over"),nl,
	halt.


%% This just tells the user how to play the game, however in keeping with 
%  the them that is dark souls, I will do my best to be vague and cryptic 
%  with my instructions so the player has a more dark soulsesque experience
instructions:-
	nl,
	write("A few commands are:"), nl,
	write("look				--look at your surroundings"),nl,
	write("go (direction)			--go in that direction"),nl,
	write("directions:			--north,east,south,west"),nl,
	write("fog door			--enter the boss room."),nl,
	write("attack				--attack the enemy."),nl,
	write("q				--to end the game and quit."),nl,
	write("instructions.			--display this message again"),nl,
	write("There are more hidden instructions it is up to you to find them"),nl,nl.

%% This rule prints out instructions and tells the player where they are,
%  maybe later I can try to add some functionality for a save game, or a restart
%  function, I can make this more interesting, but its fine for now. Also
% I need to play the game again for inspiration :)
start:-
	write("Yes indeed "), sleep(2),
	write("it is called Lothric, where the transitory lands of the lords of cinder converge"), sleep(3), nl,
	write("In ventruing north "), sleep(2),
	write("the pilgrims discover the truth of the old worlds."), sleep(2),
	nl,
	write("\"The fire fades and the lords go without thrones.\""), sleep(8),
	nl,
	write("When the link of fire is threatened, the bell tolls. "), sleep(2),
	write("Unearthing the lords of cinder from their graves."), sleep(4),
	nl,
	write("Aldrich, Saint of the Deep."), sleep(5),
	nl,
	write("Farron's Undead Legion. The Abyss Watchers."), sleep(5),
	nl,
	write("And the reclusive lord of the Profaned Capital. Yhorm the Giant."),sleep(8),
	nl, 
	write("Only in truth... "), sleep(3),
	write("The lords will abandon their thrones and sleep..."), sleep(5),
	nl,
	write("... and the unkindled will rise."), sleep(5),
	nl,
	write("Nameless, accursed Undead, unfit even to be cinder. "), sleep(2),
	nl,
	write("And so it is. That ash seeketh embers."), sleep(10),
	nl,
	nl,
	nl,
	look,
	instructions,
	main_game.

%% The following predicates are used by the look predicate and do exactly as the
%  predicate says, they describe the place that you currently are at,
%  how much information I want to reveal is still undecided
%  
% Maybe for locations, I should have to descriptons, one for the first time,
% and one for the second time.	
describe(firelink_shrine):-
	write("After awakening and defeating Iudex Gundyr, you find a coiled sword "),
	write("and proceed into the Firelink Shrine"), nl,sleep(2),
	write("Upon entering, five thrones, surrounded by candles stand before you. "),
	write("They all seem to be facing a central location."),nl, sleep(2),
	write("A small corpse approaches you..."), nl, sleep(5),
	write("Ludleth: \" Oh, thou'rt unkindled and seeker of lords,\""),nl, sleep(3),
	write("Ludleth: \"I am ludleth of Courland.\""),nl, sleep(3),
	write("Ludleth: \"Look not in bewilderment as I say ... I linked the fire long ago"),nl,
	write("becoming a lord of cinder\""), nl, sleep(3),
	write("Ludleth: \"If substantiation be thy want, set thine eyes upon my charred corpse\""), nl, sleep(3),
	write("Ludleth: \"This sad cadav'r. No need to be coy, have a closer look\""), sleep(3), nl,
	write("You venture down a stair case to find the remains of a fire. "),
	write("A woman in black robes with silver hair approaches."), sleep(3),
	nl,nl,
	write("\"Welcome to the bonfire unkindled one\""),nl, sleep(2),
	write("Firekeeper: \"I am a firekeeper. I tend to the flame, and tend"),
	write(" to thee."), nl, sleep(3),
	write("Firekeeper: \"The lords have left their thrones, and must be delivered"),nl,
	write(" to them.\""), nl,sleep(3),
	write("Firekeeper: \"To this end, I am at thy side.\""), nl,sleep(4),
	write(" You plunge the coiled sword into the remains of the fire, "), nl, 
	write("and set them ablaze, lighting up the room with a warm comforting glow."),
	nl,sleep(3),
	write("Then, darkness consumes you..."), sleep(5),
	nl,
	tty_clear.
	
%% Describe High Wall of Lothric
describe(high_wall_of_lothric):-
	write("Upon arrival, you see a large castle riddled with corpses as if a "),
	nl,
	write("Battle was fought here."),
	nl,
	write("A slight shimmer cathces you're eye, and then you notice what looks like "),
	nl,
	write("a fog door."),
	nl,
	write("Maybe there is some treasure behind the fog door, are you brave enough to "),
	nl,
	write("venture forth?"),
	nl.

%% Describe Vordt boss room
describe(vordt_boreal_valley_room):-
	has(Weapon,weapon,player),
	boss_room(vordt_boreal_valley_room, _,_,WeaponR,_),
	item_rating(Weapon, Rating),
	WeaponR > Rating,
	!,
	write("You see a giant frost boar in plated armor. "),
	nl,
	write("He lets out a loud roar, charges you ramming you into the wall."),nl,
	die.
describe(vordt_boreal_valley_room):-
	alive(vordt_of_the_boreal_valley),!,
	write("You see a giant frost boar in plated armor. "),
	%% Here is where I will implement the boss fight. For now only one swing of a short
	%  sword will kill him (not very dark soulsesque
	write("He lets out a loud roar, charges you, but you narrowly dodge."),nl,
	write("He smashes into the wall, his tusks seem to be stuck in the wall"),nl.
descirbe(vordt_of_the_boreal_valley):-
	write("You see the corpse of Vordt, and a path leading to the east"),nl.

%% Describe undead settlement
describe(undead_settlement):-
	write("Upon arrival you see only two paths, another fog door, and a small elevator to the east.").
describe(cursed_rotted_greatwood_room):-
	has(Weapon,weapon,player),
	boss_room(cursed_rotted_greatwood_room, _, _, WeaponR,_),
	item_rating(Weapon, Rating),
	WeaponR > Rating,
	!,
	write("A Giant rotten tree stands before you. It topples over on to you, "),nl,
	write("You are not fast enough to escape"),nl,	
	die.
describe(cursed_rotted_greatwood_room):-
	alive(cursed_rotted_greatwood),
	!,
	write("A giant rotten tree stand before you. It topples over"),nl,
	write("You quickly roll out of the way"),sleep(3),
	nl,
	write("The tree starts to stand again, it seems to be alive!!"),nl.
describe(cursed_rotted_greatwood_room):-
	write("You see only one path leading out. The way you came in to the west").

%% Road of sacrafices
describe(road_of_sacrafices):-
	write("Not much going on in this area. Only a small fog door"),nl.

%% End descriptions %%
%
% The following describes what happens when we attack bosses
%
% This predicate is soley for attacking the cursed greatwood as he gives us a specail item
attack:-
	curLoc(Place),
	boss_room(Place,Soul,Boss,WeaponRating,_),
	has(Weapon, weapon, player),
	item_rating(Weapon,X),
	WeaponRating =< X,
	Boss = cursed_rotted_greatwood,	
	!,
	retract(alive(Boss)),
	assert(has(Soul, soul, player)),
	assert(has(transposing_kiln, key, player)),
	write("Boss vanquished, ember restored").

%% If we are able to kill the boss
attack:-
	curLoc(Place),
	boss_room(Place,Soul, Boss,WeaponRating,_),
	has(Weapon,weapon,player),
	item_rating(Weapon, X),
	WeaponRating =< X,
	!,
	retract(alive(Boss)),
	write("Boss vanquished, ember restored"),nl,
	assert(has(Soul,soul,player)).

%% If we are unable to kill the boss
attack:-
	curLoc(Place),
	boss_room(Place,_,_,_,Msg),
	!,
	write(Msg),
	nl,nl,
	die.
%% If there is nothing to attack
attack:-
	write("Nothing to attack here").


%% Before we can transpose, we need to give ludleth the transposing kiln first
give:-
	has(transposing_kiln, key, player),
	curLoc(firelink_shrine),!,
	write("Giving ludleth the transposing kiln..... this cannot be undone"),
	nl,
	retract(has(transposing_kiln,key, player)),
	assert(has(transposing_kiln,key,ludleth)).
	
give:-
	write("You do not have anything to give"),
	nl.


%% The following is logic for the transposing kiln. For those reading this who have not
%  played any of the dark souls games, there are strong weapons called boss weapons.
%  in Dark Souls 3 the transposing kiln is what is used to get the boss weapons.
%  it must be given to ludleth and then you can exchange boss souls for the 
%  corresponding boss weapon
transpose(Soul):-
	curLoc(firelink_shrine),
	has(transposing_kiln,key,ludleth),
	%% We need to check that we have the soul
	has(Soul,soul, player),!,
%	retract(has(Soul,soul, player)),
	boss_weapon(Soul,Type,Bweapon),
	has(Weapon, Type, player),
	drop(Weapon),
	assert(has(Bweapon,Type,player)).
	
	

		
%% Now for the main Game loop logic...
start_game:-
	tty_clear,
	start,
	nl,
	main_game.

main_game:-
	alive(player),
	read_line_to_string(user_input,X),
	atomic_list_concat(L," ",X),
	command_exec(L),
	nl, main_game.
	
%% A very very simple english interpreter for a small set of instructions
% to play the game

% some simple definitions for verbs
%% Verb matching to look around the map
verb(look, [look, around|_]).
verb(look, [look |_]).
%% Verb matcihng to display instructions
verb(instructions, [help|_]).
verb(instructions, [instructions|_]).
%% Verb matching for movement through map
verb(go(fog_door), [go, fog, door|_]).
verb(go(fog_door), [go, through, fog, door|_]).
verb(go(fog_door), [go, through, the , fog, door|_]).
verb(go(fog_door), [go, to , fog, door|_]).
verb(go(X), [go|X]).
verb(go(X), [go, to|X]).
%% Verb matchgin to look closer
verb(inspect_further, [inspect|_]).
verb(inspect_further, [inspect, further|_]).
%% Verb matching for picking items up
verb(pick_up(Y), [take|X]):-
	noun(Y, X),
	!.

verb(pick_up(Y), [grab|X]):-
	noun(Y, X),
	!.

verb(pick_up(Y), [pick, up |X]):-
	noun(Y,X),
	!.

%% Verb matching for inventory list
verb(inventory, [inventory|_]).
verb(inventory, [list|_]).
verb(inventory, [items|_]).

%% Verb matching for talking
verb(talk(Y), [talk |X]):-
	npc_name(X,Y),!.
verb(talk(Y), [talk, to|X]):-
	npc_name(X,Y),!.
verb(talk(Y), [speak |X]):-
	npc_name(X,Y),!.
verb(talk(Y), [speak, with |X]):-
	npc_name(X,Y),!.
verb(talk(Y), [speak, to |X]):-
	npc_name(X,Y),!.
%% Verb matching for giving the transposing kiln
verb(give, [give|_]).
verb(give, [give, trasnposing, kiln|_]).
verb(give, [give, trasnposing, kiln, to , ludleth|_]).
%% Verb matching for trasnpose
verb(transpose(Y), [transpose|X]):-
	atomic_list_concat(X, " ",L),
	itemName(Y,L).
% The following are if we try to talk to someone who doesn't exist
verb(talk(X), [talk|X]).
verb(talk(X), [talk, to |X]).
verb(talk(X), [speak|X]).
verb(talk(X), [speak,with|X]).
verb(talk(X), [speak,to|X]).
%% Verb matching for attacking
verb(attack, [attack|_]).
verb(attack, [fight|_]).
verb(attack, [battle|_]).
%% Verb mathcing for quitting
verb(halt, [quit|_]).
verb(halt, [halt|_]).
verb(halt, [end|_]).
verb(halt, [end, game| _]).
verb(halt, [exit |_]).
verb(halt, [q|_]).
%% In case we do not recognise the command
verb(not_a_command, [_|_]).
verb(not_a_command, [_]).
 %% End verbs %%

 %% start nouns %%
%% The following is a list of nouns, which are basically just items throghout
%  the game.
noun(short_sword, [short, sword|_]).
noun(long_sword,  [long, sword |_]).
noun(leather_armor, [leather, armor|_]).
%noun(chain_armor, [chain, armor|_).
 %% end nouns %%
%% The command predicate takes in a sentence, and checks to see if we have
%  a valid sentence, ie go west
command_exec(L):-
	verb(X, L),
	call(X).
command_exec(_):-fail.
%% This just tests if I can properly execute senetences, ie
%  an example playthrough would look like: (again testing only)
%  ?- command_get.
% |:  go west.
% You cannot go that way.......
command_get:-
	read_line_to_string(user_input,X),
        atomic_list_concat(L," ",X),
        command_exec(L).

%% This is the verb that keeps things running
%  if for whatever reason, the user inputs a non valid command
not_a_command:-
	write("Sorry I do not understand, please enter a valid instruction."),
	nl.
