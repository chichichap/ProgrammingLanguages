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

% refersTo/2
% pre-conditions: L is a list and X is an atom (an object)
% post-conditions: true if the sequence of words in list L is a noun phrase referring to the object X
refersTo(L,X) :- np(L,X).

% means/2
% pre-conditions: the two inputs are lists of lists
% post-conditions: true if the sequence of sentences in list L represents the sequence of actions in list A, 
% 				   where each action is represented as a list of 3 elements, [actionName, objectName1, objectName2].
means([],[]).
means([L|LT], [A|AT]) :- s(L,A), means(LT,AT).
