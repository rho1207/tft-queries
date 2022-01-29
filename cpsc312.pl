% ========================================= JSON SETUP & START =========================================

% JSON
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- dynamic(found/2).


% Make a call to the TFT API 
api(Category, _, _) :-
  setup_call_cleanup(
    http_open('https://api.myjson.com/bins/12fekw', In, []),
    json_read(In, Category),
    close(In)
  ),
    Category=json([champions=X]),
  parse(X).
:- initialization(api(_,_,_)). 


% Parse the JSON file to build KB
parse([H|T]) :-
  H = json([H1,H2,H3,H4,H5|_]),
  =(H1, =(name, Name)),
  =(H2, =(origin, Origin)),
  =(H3, =(class, Class)),
  =(H4, =(cost, Cost)),
  =(H5, =(items, Item)),
    assert(champion(Name)),
    assert(champion_origin(Name,Origin)),
    assert(champion_class(Name,Class)),
    assert(champion_cost(Name,Cost)),
    assert(champion_item(Name,Item)),
  parse(T).
parse([]).


% User Level - Dynamic
start(_) :-
    write("What level are you? [2-9] : "),
    flush_output(current_output),
    readln([Head|_]),
    assert(level(Head)),
    write("For a simple question, input: q(Ans) or complex question: q2(Ans)").
    
:- initialization(start(_)).

% To get the input from a line:

q(Ans) :-
    write("Ask me: "), flush_output(current_output),
    readln(Ln),
    question(Ln,End,Ans),
    member(End,[[],['?'],['.']]).

q2(Ans) :-
    write("Ask me: "), flush_output(current_output),
    readln(Ln),
    question2(Ln,End,Ans,[],C),
    prove(C),
    member(End,[[],['?'],['.']]).    
    
% ================================ NLP based off Dr.Poole's geography.pl ================================

% question(Question,QR,Entity) is true if Query provides an answer about Entity to Question
question([what,is | L0],L1,Entity) :-
    noun_phrase(L0,L1,Entity).
question([what,are | L0],L1,Entity) :-
    noun_phrase(L0,L1,Entity).    
question([what | L0],L2,Entity) :-
    noun_phrase(L0,L1,Entity),
    mp(L1,L2,Entity).


% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(L0,L4,Entity) :-
    det(L0,L1,Entity),
    adjectives(L1,L2,Entity),
    noun(L2,L3,Entity),
    mp(L3,L4,Entity).


% Determiners
det([the | L],L,_).
det([a | L],L,_).
det([an | L],L,_).
det([with | L],L,_).
det(L,L,_).
	

% adjectives(L0,L1,Entity) is true if 
% L0-L1 is a sequence of adjectives that true of Entity
adjectives(L0,L2,Entity) :-
    adj(L0,L1,Entity),
    adjectives(L1,L2,Entity).
adjectives(L,L,_).


% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(L0,L1,Entity) :-
    noun_phrase(L0,L1,Entity).
mp([that|L0],L1,Entity) :-
    noun_phrase(L0,L1,Entity).
mp([with|L0],L1,Entity) :-
    noun_phrase(L0,L1,Entity).  
mp([for|L0],L1,Entity) :-
    noun_phrase(L0,L1,Entity).  
mp(L,L,_).

% ============================================= DICTIONARY =============================================
% adj(L0,L1,Entity) is true if L0-L1 
% is an adjective that is true of Entity
adj([Lang,origin | T],T,Entity) :- champion_origin(Entity,Lang).
adj([Lang,class  | T],T,Entity) :- champion_class(Entity,Lang).
adj([Lang,cost   | T],T,Entity) :- champion_cost(Entity,Lang).


% Nouns 
noun([champion | T],T,X) :- champion(X).
noun([odds | T],T,X) :- probability(_,X).
noun([X | T],T,X) :- champion_origin(_,X).
noun([X | T],T,X) :- champion_class(_,X).
noun([X | T],T,X) :- champion_cost(_,X).



% ============================================= COMPLEX NLP =============================================

% Complex Questions 
question2([what,is | T0],T1,Entity,C0,C1) :-
    noun_phrase2(T0,T1,Entity,C0,C1).
question2([what | T0],T1,Entity,C0,C1) :-
    noun_phrase2(T0,T1,Entity,C0,C1).


% Noun Phrases.
noun_phrase2(T0,T4,Entity,C0,C4) :-
    det2(T0,T1,Entity,C0,C1),
    adjectives2(T1,T2,Entity,C1,C2),
    noun2(T2,T3,Entity,C2,C3),
    mp2(T3,T4,Entity,C3,C4).


% Determiners.
det2([the | T],T,_,C,C).
det2([a | T],T,_,C,C).
det2([as | T],T,_,C,C).
det2(T,T,_,C,C).


% Adjectives.
adjectives2(T,T,_,C,C).
adjectives2(T0,T2,Entity,C0,C2) :-
    adj2(T0,T1,Entity,C0,C1),
    adjectives2(T1,T2,Entity,C1,C2).
	

% Dictionary
adj2([h | T],T,_,[_,C], C).
	

% Nouns
noun2([champion | T],T,Entity,C,[champion(Entity)|C]).
noun2([X | T],T,X,C,C) :- champion(X).
noun2([probability | T],T,X,C,C) :- 
    champion_cost(X,R),
    probability(R,C).


% Modifying Phrase.
mp2(T,T,_,C,C).
mp2(T0,T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).
mp2([that,uses,the|T0],T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).
mp2([has|T0],T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).    
mp2([with|T0],T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).
mp2([has,the|T0],T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).    
mp2([with,the|T0],T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).
mp2([of,getting,a|T0],T1,O1,C0,C1) :-
    noun_phrase2(T0,T1,O1,C0,C1).

    
% Relations
reln2([same,origin,as| T],T,O1,O2,_,[sameOrigin(O1,O2)]).
reln2([same,class,as| T],T,O1,O2,_,[sameClass(O1,O2)]).
reln2([same,cost,as| T],T,O1,O2,_,[sameCost(O1,O2)]).
reln2([more,cost,than| T],T,O1,O2,_,[moreCost(O1,O2)]).
reln2([less,cost,than| T],T,O1,O2,_,[lessCost(O1,O2)]).
reln2([same,item,as| T],T,O1,O2,_,[sameItem(O1,O2)]).


% ask(Q,A) gives answer A to question Q
% ask2(Q,A) gives answer A to question Q
% prove(L) proves all elements of L against the database
ask(Q,A) :-
    question(Q,[],A). 

ask2(Q,A) :-
    question2(Q,[],A,[],C),
    prove(C).

prove([]).
prove([H|T]) :-
    call(H),      % built-in Prolog predicate calls an atom
    prove(T).    


% ========================================== HELPER FUNCTIONS ==========================================

% sameOrigin(X,Y) where X is an name of a champion and Y is the name of a champion that is in the same origin
sameOrigin(X,Y) :-
    champion_origin(X, Z),
    champion_origin(Y, Z),
    X \= Y,
    champion(X),
    champion(Y).    

% sameClass(X,Y) where X is an name of a champion and Y is the name of a champion that is in the same class
sameClass(X,Y) :-
    champion_class(X, Z),
    champion_class(Y, Z),
    X \= Y,
    champion(X),
    champion(Y).        

% sameCost(X,Y) where X is an name of a champion and Y is the name of a champion that costs the same
sameCost(X,Y) :-
    champion_cost(X, Z),
    champion_cost(Y, Z),
    X \= Y,
    champion(X),
    champion(Y).

% lessCost(X,Y) where X is an name of a champion and Y is the name of a champion that costs less
lessCost(X,Y) :-
    champion_cost(X, A),
    champion_cost(Y, B),
    atom_number(A,C),
    atom_number(B,D),
    C < D,
    X \= Y,
    champion(X),
    champion(Y).

% moreCost(X,Y) where X is an name of a champion and Y is the name of a champion that costs more
moreCost(X,Y) :-
    champion_cost(X, A),
    champion_cost(Y, B),
    atom_number(A,C),
    atom_number(B,D),
    C > D,
    champion(X),
    champion(Y).    

% sameItem(X,Y) where X is an name of a champion and Y is the name of a champion that uses the same item
sameItem(X,Y) :-
    champion_item(X, Z),
    champion_item(Y, Z),
    X \= Y,
    champion(X),
    champion(Y).

% probabilty(X,Y) where X is the cost of unit and Y is the probability
probability(X,Y):-
    level(A),
    tierprob(X,A,Y).

% tierprob(C,L,P) where P is the probability of recieving a C cost champion at level L
% used for dynamic KB

% tier 1
tierprob(1,2,100).
tierprob(1,3,70).
tierprob(1,4,50).
tierprob(1,5,35).
tierprob(1,6,25).
tierprob(1,7,20).
tierprob(1,8,15).
tierprob(1,9,10).

% tier 2
tierprob(2,2,0).
tierprob(2,3,25).
tierprob(2,4,35).
tierprob(2,5,35).
tierprob(2,6,35).
tierprob(2,7,30).
tierprob(2,8,20).
tierprob(2,9,15).

% tier 3
tierprob(3,2,0).
tierprob(3,3,5).
tierprob(3,4,15).
tierprob(3,5,25).
tierprob(3,6,30).
tierprob(3,7,33).
tierprob(3,8,35).
tierprob(3,9,30).

% tier 4
tierprob(4,2,0).
tierprob(4,3,0).
tierprob(4,4,0).
tierprob(4,5,5).
tierprob(4,6,10).
tierprob(4,7,15).
tierprob(4,8,24).
tierprob(4,9,30).

% tier 5
tierprob(5,2,0).
tierprob(5,3,0).
tierprob(5,4,0).
tierprob(5,5,0).
tierprob(5,6,0).
tierprob(5,7,2).
tierprob(5,8,6).
tierprob(5,9,15).


% ============================================ Try Queries =============================================
%
% ask([what,are,the,odds], X).    
%
% ask([what,is,a,champion], X).    
% ask([what,is,a,demon,origin,champion],X).
% ask([what,is,a,glacial,origin,champion],X).
% ask([what,is,a,gunslinger,class,champion],X).
% ask([what,is,a,'3',cost,champion],X).
%
% ask2([what,champion,has,the,same,origin,as,lucian], X).
% ask2([what,champion,has,the,same,class,as,veigar], X).
% ask2([what,champion,has,the,same,cost,as,zed], X).
% ask2([what,is,a,champion,with,the,same,origin,as,aatrox], X).
% ask2([what,is,a,champion,with,the,same,class,as,brand], X).
% ask2([what,is,a,champion,with,the,same,cost,as,varus], X).
% ask2([what,is,a,champion,that,uses,the,same,item,as,garen], X).
% ask2([what,is,a,champion,with,more,cost,than,akali], X).
% ask2([what,is,a,champion,with,less,cost,than,varus], X).



% ====================================== Queries that should fail ======================================
%
% ask2([what,is,a,champion,with,the,same,origin,as,yasuo], X).
% - No champions share origins with yasuo
%
% ask2([what,is,a,champion,with,more,cost,than,yasuo], X).
% - Nothing costs more than 5
%
% ask2([what,is,a,champion,with,less,cost,than,garen], X).
% - Nothing costs less than 1