% verb/2
% pre-conditions: X is an atom, and F can be checked for exact structural match (=)
% post-conditions: true if X is one of the verbs from the production rules, and F is "verb(X)" with X replaced by the verb
verb(X,F) :- member(X, [put, pickup, stack, unstack]), F=verb(X).

% det/2
% pre-conditions: X is an atom, and F can be checked for exact structural match (=)
% post-conditions: true if X is one of the determinants from the production rules, and F is "verb(X)" with X replaced by the determinant
det(X,F) :- member(X, [the]), F=det(X).

% det/2
% pre-conditions: X is an atom, and F can be checked for exact structural match (=)
% post-conditions: true if X is one of the adjectives from the production rules, and F is "verb(X)" with X replaced by the adjective
adj(X,F) :- member(X, [big, small, green, red, yellow, blue]), F=adj(X).

% noun/2
% pre-conditions: X is an atom, and F can be checked for exact structural match (=)
% post-conditions: true if X is one of the nouns from the production rules, and F is "verb(X)" with X replaced by the noun
noun(X,F) :- member(X, [block, table]), F=noun(X).

% prep/2
% pre-conditions: X is an atom, and F can be checked for exact structural match (=)
% post-conditions: true if X is one of the prepositions from the production rules, and F is "verb(X)" with X replaced by the preposition
prep(X,F) :- member(X, [on, from]), F=prep(X).

% -------- helper predicates that translate the production rules of BlocksGrammar into Prolog rules -------- %
% s/2
% pre-conditions: L is a list, and F can be checked for exact structural match (=)
% post-conditions: true if the sequence of words in the list forms a sentence, and F is "sentence(X)" with X replaced by the sentence
s(L,F) :- vp(L,F2), F=sentence(F2).

% vp/2
% pre-conditions: [A|T] is a list, and F can be checked for exact structural match (=)
% post-conditions: true if the sequence of words in the list forms a verb phrase, and F is "verb_phrase(X)" with X replaced by the verb phrase
vp([A|T], F) :- verb(A, F2), append(B,C,T), np(B,F3), pp(C,F4), F = verb_phrase(F2, F3, F4).

% np/2
% pre-conditions: [A|T] is a list, and F can be checked for exact structural match (=)
% post-conditions: true if the sequence of words in the list forms a verb phrase, and F is "verb_phrase(X)" with X replaced by the verb phrase
np([A|[B]], F) :- det(A,F2), noun(B,F3), F=noun_phrase(F2, F3).
np([A|T], F) :- det(A,F2), append(B,[C],T), adjp(B,F3), noun(C,F4), F=noun_phrase(F2, F3, F4).

% adjp/2
% pre-conditions: [A] is a list, or [A|B] is a list, and F can be checked for exact structural match (=)
% post-conditions: true if the sequence of words in the list forms a adjective phrase, and F is "adj_phrase(X)" with X replaced by the adjective phrase
adjp([A], F) :- adj(A,F2), F=adj_phrase(F2).
adjp([A|B], F) :- adj(A,F2), adjp(B,F3), F=adj_phrase(F2, F3).

% pp/2
% pre-conditions: [A|B] is a list, and F can be checked for exact structural match (=)
% post-conditions: true if the sequence of words in the list forms a preposition phrase, and F is "adj_phrase(X)" with X replaced by the preposition phrase
pp([A|B],F) :- prep(A,F2), np(B,F3), F=prep_phrase(F2, F3).

% parseTree/2
% pre-conditions: L is a list, and F can be checked for exact structural match (=)
% post-conditions: true the sequence of words in list L forms a grammatical sentence according to BlocksGrammar, 
% 				    and the term T represents the parse tree for the sentence in L
parseTree(L,T) :- s(L,T).