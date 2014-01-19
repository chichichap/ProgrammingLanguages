% state/1
% pre-conditions: L is a list
% post-conditions: true if L lists all and only the states in the database.
state(L) :- db_subset(L,[]), \+ db_proper_subset(L).

% db_subset/2
% pre-conditions: the two inputs are lists; the second should be empty when first called (used to eliminate duplicates)
% post-conditions: true if the first list is a subset of the database (every element in the list is a state in the database)	
db_subset([], _).
db_subset([H|T], L) :- isA(Y,arm), \+ holds(Y,_), H=armEmpty, 	  \+ member(H,L), db_subset(T, [H|L]).
db_subset([H|T], L) :- isA(Y,arm), 	  holds(Y,X), H=isHolding(X), \+ member(H,L), db_subset(T, [H|L]).
db_subset([H|T], L) :- isA(Y,table),     on(X,Y), H=onTable(X),   \+ member(H,L), db_subset(T, [H|L]).
db_subset([H|T], L) :- isA(Y,block),     on(X,Y), H=on(X,Y), 	  \+ member(H,L), db_subset(T, [H|L]). 
db_subset([H|T], L) :- 				    clear(X), H=clear(X),     \+ member(H,L), db_subset(T, [H|L]). 

% db_proper_subset/1 
% pre-conditions: L is a list
% post-conditions: true if L is a proper subset of the database (there exists a state in the database not in L)
db_proper_subset(L) :- isA(Y,arm), \+ holds(Y,_), H=armEmpty, 	  \+ member(H,L).
db_proper_subset(L) :- isA(Y,arm), 	  holds(Y,X), H=isHolding(X), \+ member(H,L).
db_proper_subset(L) :- isA(Y,table),     on(X,Y), H=onTable(X),   \+ member(H,L).
db_proper_subset(L) :- isA(Y,block),     on(X,Y), H=on(X,Y), 	  \+ member(H,L).
db_proper_subset(L) :- 			 	    clear(X), H=clear(X), 	  \+ member(H,L).


% plan/1 
% pre-conditions: L is a list
% post-conditions: true if the sequence of sentences in list L expresses an executable plan
plan(L) :- state(S), !, plan2(L,S).

% plan/2 
% pre-conditions: the two inputs are lists; the first is a sequence of sentences; the second is a list of current states
% post-conditions: true if the sequence of sentences is an executable plan, given the current states in the second list
plan2([],_).
plan2([H|T],A) :- s(H,[pickup, X,Y]),  do_pickup(X,Y,A,B), plan2(T,B).
plan2([H|T],A) :- s(H,[put,	   X,Y]),     do_put(X,Y,A,B), plan2(T,B).
plan2([H|T],A) :- s(H,[stack,  X,Y]),   do_stack(X,Y,A,B), plan2(T,B).
plan2([H|T],A) :- s(H,[unstack,X,Y]), do_unstack(X,Y,A,B), plan2(T,B).

% do_pickup/4 
% pre-conditions: the first two inputs are atoms; the last two are lists (current states and new states)
% post-conditions: true if "pickup" can be done, given the current states, and C lists the new states 
do_pickup(X,Y,A,C) :- isA(X,block), isA(Y,table),
					  member(clear(X),A), member(onTable(X),A), member(armEmpty,A), 	% Pre-conditions
					  delete(A, [clear(X),onTable(X),armEmpty], B), 					% Delete-effects
					  union(B, [isHolding(X)], C).										% Add-effects

% do_put/4 
% pre-conditions: the first two inputs are atoms; the last two are lists (current states and new states)
% post-conditions: true if "put" can be done, given the current states, and C lists the new states 					  
do_put(X,Y,A,C) :- isA(X,block), isA(Y,table),
				   member(isHolding(X),A), 										 		% Pre-conditions
				   delete(A, [isHolding(X)], B), 										% Delete-effects
			       union(B, [clear(X),armEmpty,onTable(X)], C).	  						% Add-effects

% do_stack/4 
% pre-conditions: the first two inputs are atoms; the last two are lists (current states and new states)
% post-conditions: true if "stack" can be done, given the current states, and C lists the new states 							
do_stack(X,Y,A,C) :- isA(X,block), isA(Y,block),
					 member(isHolding(X),A), member(clear(Y),A), 				 		% Pre-conditions
					 delete(A, [isHolding(X),clear(Y)], B),	 							% Delete-effects
					 union(B, [clear(X),armEmpty,on(X,Y)], C).	  						% Add-effects	

% do_unstack/4 
% pre-conditions: the first two inputs are atoms; the last two are lists (current states and new states)
% post-conditions: true if "unstack" can be done, given the current states, and C lists the new states 			  
do_unstack(X,Y,A,C) :- isA(X,block), isA(Y,block),
					   member(clear(X),A), member(on(X,Y),A), member(armEmpty,A), 		% Pre-conditions
					   delete(A, [clear(X),on(X,Y),armEmpty], B), 						% Delete-effects
					   union(B, [isHolding(X),clear(Y)], C).							% Add-effects			  
							
% -------- predicates from question 3 -------- %
% verb/1
% pre-conditions: X is an atom
% post-conditions: true if X is one of the verbs from the production rules
verb(X) :- member(X, [put, pickup, stack, unstack]).

% det/1
% pre-conditions: X is an atom
% post-conditions: true if X is one of the determinants from the production rules
det(X) :- member(X, [the]).

% adj/2
% pre-conditions: X and C, or S, are atoms
% post-conditions: true if X has the C color / true if X has the S size (to be checked with the given database)
adj(X,C) :- color(X,C).
adj(X,S) :- size(X,S).

% noun/2
% pre-conditions: X and N are atoms
% post-conditions: true if X is of type N (to be checked with the given database)
noun(X,N) :- isA(X,N).

% prep/1
% pre-conditions: X is an atom
% post-conditions: true if X is one of the prepositions from the production rules
prep(X) :- member(X, [on, from]).

% -------- helper predicates that translate the production rules of BlocksGrammar into Prolog rules -------- %
% s/2
% pre-conditions: the two input are lists, and the second is a list of 3 elements
% post-conditions: true if the sequence of words in the list forms a sentence, and X is the sequence of actions, [actionName, objectName1, objectName2]
s(L,X) :- vp(L,X).

% vp/2
% pre-conditions: the two input are lists, and the second is a list of 3 elements
% post-conditions: true if the sequence of words in the list forms a verb phrase, and X is the sequence of actions, [actionName, objectName1, objectName2]
vp([X|T], [X,Y,Z]) :- verb(X), append(B,C,T), np(B,Y), pp(C,Z).

% np/2
% pre-conditions: [A,B] or [A|T] is a list, and X is an atom (an object)
% post-conditions: true if the sequence of words in the list forms a noun phrase, and X is the object referred to by the noun phrase
np([A,B], X) :- det(A), noun(X,B).
np([A|T], X) :- det(A), append(B,[C],T), adjp(B,X), noun(X,C).

% adjp/2
% pre-conditions: [A] or [A|B] is a list, and X is an atom (an object)
% post-conditions: true if the sequence of words in the list forms a adjective phrase, and X is the object referred to by the adjective phrase
adjp([A], X) :- adj(X,A).
adjp([A|B], X) :- adj(X,A), adjp(B,X).

% pp/2
% pre-conditions: [A|B] is a list, and X is an atom (an object)
% post-conditions: true if the sequence of words in the list forms a preposition phrase, and X is the object referred to by the preposition phrase
pp([A|B],X) :- prep(A), np(B,X).
