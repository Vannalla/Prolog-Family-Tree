

father(david, holly).
father(david, heather).
father(durkee, brad).
father(durkee, trevor).
father(leverett, durkee).
father(leverett, elmo).
father(brad, charlie).
father(brad, flora).
father(reuben, virginia).
father(reuben, dorothy).

mother(nancy, holly).
mother(nancy, heather).
mother(mary, brad).
mother(mary, trevor).
mother(holly, charlie).
mother(holly, flora).
mother(virginia, durkee).
mother(virginia, elmo).

male(david).
male(durkee).
male(leverett).
male(brad).
male(charlie).
male(trevor).
male(elmo).
male(reuben).

female(nancy).
female(mary).
female(virginia).
female(dorothy).
female(holly).
female(flora).
female(heather).

aunt(Aunt,Kid) :-
    parent(GP,Aunt),
    parent(GP,P),
    parent(P,Kid), 
    P \= Aunt,
    female(Aunt).

fullSister(Sis, Sib) :- 
    parent(P1,Sis),     
    parent(P1,Sib),
    parent(P2,Sis),     
    parent(P2,Sib),
    female(Sis),
    Sis \= Sib,        
    P1 \= P2.   

parent(P,Kid) :- mother(P,Kid).
parent(P,Kid) :- father(P,Kid).

grandfather(Gramps,Kid) :- father(Gramps,Mid), parent(Mid,Kid).

grandparent(Gparent,Kid) :- parent(Gparent,Mid), parent(Mid,Kid).

% The ancestor(Old,Young) relation is true if Old is the ancestor of Young.

ancestor(Old,Young) :- parent(Old,Young).  % 1 generation apart
ancestor(Old,Young) :- grandparent(Old,Young). % 2 generations apart
% ancestor(Old,Young) :- grandparent(Old,Mid), parent(Mid,Young). % 3 gen
ancestor(Old,Young) :- parent(Old,Mid), grandparent(Mid,Young). % alternate

ancestorRec(Old,Young) :- parent(Old,Young).  % Base case
ancestorRec(Old,Young) :- parent(Old,Mid), ancestorRec(Mid,Young).
%ancestorRec(Old,Young) :- ancestorRec(Old,Mid), parent(Mid,Young).

related(P1,P2) :- ancestorRec(P1,P2).
related(P1,P2) :- ancestorRec(P2,P1).
related(P1,P2) :- ancestorRec(A,P1), ancestorRec(A,P2).


ancestorGen(Old,Old,0).  % Base case
ancestorGen(Old,Young,1) :- parent(Old,Young).  % Base case
ancestorGen(Old,Young,N) :- parent(Old,Mid), ancestorGen(Mid,Young,Y), succ(Y,N), Mid \= Young.

generations(P1,P2,N):- ancestorGen(P1,P2,N).
%generations(P1,P2,N):- ancestorGen(P2,P1,N).
%generations(P1,P2,N):- ancestorGen(A,P1,N), ancestorGen(A,P2,N).


digitName([zero],0).
digitName([one],1).
digitName([two],2).
digitName([three],3).
digitName([four],4).
digitName([five],5).
digitName([six],6).
digitName([seven],7).
digitName([eight],8).
digitName([nine],9).

teenName([eleven],11).
teenName([twelve],12).
teenName([thirteen],13).
teenName([fourteen],14).
teenName([fifteen],15).
teenName([sixteen],16).
teenName([seventeen],17).
teenName([eighteen],18).
teenName([nineteen],19).

multName([ten],10).
multName([twenty],20).
multName([thirty],30).
multName([fourty],40).
multName([fifty],50).
multName([sixty],60).
multName([seventy],70).
multName([eighty],80).
multName([ninety],90).

thousandName([one, thousand], 1000).

multPlusDigit([X,Y],Val) :- multName([X],Xval), digitName([Y],Yval), plus(Xval,Yval,Val).

hundredName([X,hundred],Val) :- digitName([X],Xval), Val is Xval*100.

hundredPlusDigit([X,hundred,and,Y],Val) :- digitName([X],Xval), digitName([Y],Yval), TempVal is Xval*100, plus(TempVal,Yval,Val).

hundredPlusTeen([X,hundred,and,Y],Val) :- digitName([X],Xval), teenName([Y],Yval), TempVal is Xval*100, plus(TempVal,Yval,Val).

hundredPlusMult([X,hundred,and,Y],Val) :- digitName([X],Xval), multName([Y],Yval), TempVal is Xval*100, plus(TempVal,Yval,Val).

hundredPlus([X,hundred,and,Y,Z],Val) :- digitName([X],Xval), multName([Y],Yval), digitName([Z],Zval), TempVal is Xval*100, 
    									plus(TempVal,Yval,TempVal2), plus(TempVal2,Zval,Val).

numnames(X,Y) :- digitName(X,Y).
numnames(X,Y) :- teenName(X,Y).
numnames(X,Y) :- multName(X,Y).
%numnames([X,Z], Y) :- multPlusDigit([X,Z],Y).
numnames(X, Y) :- multPlusDigit(X,Y).
numnames(X,Y) :- hundredName(X,Y).
numnames(X,Y) :- hundredPlusDigit(X,Y).
numnames(X,Y) :- hundredPlusTeen(X,Y).
numnames(X,Y) :- hundredPlusMult(X,Y).
numnames(X,Y) :- hundredPlus(X,Y).
numnames(X,Y) :- thousandName(X,Y).









