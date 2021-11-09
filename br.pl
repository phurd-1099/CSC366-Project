%% A propositional logic natural deduction reasoner with origin sets used for belief
%% revision.

%%%%%%%%%%%%%%%%%%%
%%% Origin Sets %%%
%%%%%%%%%%%%%%%%%%%

%% Origin sets are represented as [type, [hyp1, hyp2, ...]] where the hyps provide
%% the justification for believing a fact, and the type is either hyp or der. A hyp
%% type is always associated with a singleton list of hyps (the term itself). A der
%% type is associated with on or more hypotheses from which it is derivable. A term
%% may have one or more origin sets (stored in a list).

% Merge a single pair of OSes. 
merge_os_pair([_, OS1], [_, OS2], [der, Result]) :- 
    append(OS1, OS2, OS3), 
    list_to_set(OS3, Result).

% Base case - only one pair to merge. 
merge_os([OS1], [OS2], [Result]) :- merge_os_pair(OS1, OS2, Result).
% TODO: Multiple OSes to merge.
%merge_os([OS1|Rest1], [OS2|Rest2], Result) :- 

to_der_os([], []).
to_der_os([[_,OS]|Rest], [[der,OS]|PartResult]) :- to_der_os(Rest, PartResult).

% In removing terms you need to remove all of the origin sets with a specific hyp in them
% but a term may have more than one OS, so this removes just the offending ones.
remove_oses_with_hyp(_, [], []).
remove_oses_with_hyp(Term, [[_, OS]|Rest], NewOS) :- 
    member(Term, OS), !, remove_oses_with_hyp(Term, Rest, NewOS).
remove_oses_with_hyp(Term, [[Type, OS]|Rest], [[Type, OS]|NewOS]) :- 
    remove_oses_with_hyp(Term, Rest, NewOS).

%%%%%%%%%%%%%%%%%%%%%%
%%% Knowledge Base %%%
%%%%%%%%%%%%%%%%%%%%%%

%% A knowledge base is a list of the form [[term originset], [term originset], ...]
%% These rules allow building a KB without doing any inference.

assert_hyp(Term, OldKB, [[Term, [[hyp, [Term]]]]|OldKB]).

assert_hyps([Term], OldKB, NewKB) :- assert_hyp(Term, OldKB, NewKB).
assert_hyps([Term|Rest], OldKB, NewKB) :- 
    assert_hyps(Rest, OldKB, PartKB), 
    assert_hyp(Term, PartKB, NewKB).
    
%% Removing a hyp from the KB involves removing everything that has that hyp as one of
%% its origin set members. 

unassert_hyp(_, [], []).
unassert_hyp(Term, [[Term,_]|KBRest], NewKB) :- unassert_hyp(Term, KBRest, NewKB).
unassert_hyp(Term, [[OtherTerm, OS]|KBRest], NewKB) :- remove_oses_with_hyp(Term, OS, NewOS),
    (NewOS = [] -> 
        unassert_hyp(Term, KBRest, NewKB);
        unassert_hyp(Term, KBRest, PartialKB), NewKB = [[OtherTerm, NewOS]|PartialKB]).

% When two terms are in the KB which are identical, merge them.
% - If they have different OSes, merge the OSes.
% - If they have the same OS, do nothing.
merge_term(T, T, T).
merge_term([Term, OS1], [Term, OS2], [Term, NewOS]) :- merge_os(OS1, OS2, NewOS).


merge_kbs(KB1, KB2, Result) :- append_kbs(KB1, KB2, Result).

% Appending KBs is a 2-step process. First, just append them. Then, term-by-term, 
% walk through the resulting KB looking for duplicate terms. Merge them. 

append_kbs(KB1, KB2, Result) :- 
    append(KB1, KB2, KB3), !,
    merge_duplicates(KB3, Result).
    
merge_duplicates([[First, FirstOS]|Rest], [MergeTerm|Result]) :- 
    member([First, OtherOS], Rest),
    merge_term([First, FirstOS], [First, OtherOS], MergeTerm),
    delete(Rest, [First, OtherOS], NewRest),
    merge_duplicates(NewRest, Result).
merge_duplicates([[First, FirstOS]|Rest], [[First, FirstOS]|Result]) :- 
    merge_duplicates(Rest, Result).
merge_duplicates([], []).

    
% Append KB1 to KB2, removing duplicates and merging terms as needed.
%append_kbs([], KB2, KB2).
%append_kbs([[First, FirstOS]|KB1Rest], KB2, [MergeTerm|Rest]) :- 
%    member([First, OldOS], KB2),
%    merge_term([First, OldOS], [First, FirstOS], MergeTerm),
%    delete(KB2,[First, OldOS],NewKB2),
%    append_kbs(KB1Rest, NewKB2, Rest).
%append_kbs([KB1First|KB1Rest], KB2, [KB1First|Rest]) :- 
%    append_kbs(KB1Rest, KB2, Rest).

    
%%%%%%%%%%%%%%%%%%%%%%%
%%% Inference Rules %%%
%%%%%%%%%%%%%%%%%%%%%%%

implication_elimination([if(Ant,Cq), RuleOS], KB, [Cq, CqOS], [if(Ant,Cq), Ant]) :- 
    member([Ant, AntOS], KB),
    merge_os(RuleOS, AntOS, CqOS).
    
conjunction_elimination([and(Conj1, Conj2), RuleOS], [[Conj1, DerOS], [Conj2, DerOS]], [and(Conj1, Conj2)]) :-
    to_der_os(RuleOS, DerOS).

negation_elimination([not(not(Prop)), RuleOS], [Prop, DerOS], [not(not(Prop))]) :- 
    to_der_os(RuleOS, DerOS).

rule_elimination(Rule, KB, NewKB) :- 
    implication_elimination(Rule, KB, NewTerm, Why), 
    \+ member(NewTerm, KB),
    detect_and_resolve_contradiction(NewTerm, KB, KB1),
    append(KB1, [NewTerm], NewKB), 
    showproof(NewTerm, Why), !.
rule_elimination(Rule, KB, NewKB) :- 
    conjunction_elimination(Rule, [NewTerm1, NewTerm2], Why), 
    (\+ member(NewTerm1, KB); \+ member(NewTerm2, KB)),
    detect_and_resolve_contradiction(NewTerm1, KB, KB1),
    detect_and_resolve_contradiction(NewTerm2, KB1, KB2),
    append(KB2, [NewTerm1, NewTerm2], NewKB), 
    showproof(NewTerm1, Why), 
    showproof(NewTerm2, Why), !.
rule_elimination(Rule, KB, NewKB) :-
    negation_elimination(Rule, NewTerm, Why), 
    \+ member(NewTerm, KB),
    detect_and_resolve_contradiction(NewTerm, KB, KB1),
    append(KB1, [NewTerm], NewKB), 
    showproof(NewTerm, Why), !.

is_fact([F,_]) :- F \= xor(_,_), F \= and(_,_), F \= if(_,_), F \= not(not(_)).
is_rule(R) :- \+ is_fact(R).

merge_hyps([],[]).
merge_hyps([[_,HypList]|Rest],Result) :- 
    merge_hyps(Rest, ResultPart),
    append(HypList,ResultPart,Result).

hyp_set(OS1, OS2, HypSet) :- 
    merge_hyps(OS1,OS1Hyps), merge_hyps(OS2,OS2Hyps),
    append(OS1Hyps, OS2Hyps, AllHyps), 
    list_to_set(AllHyps, HypSet).
    
list_hyps([],_).
list_hyps([First|Rest], Counter) :- 
    write(Counter), write(". "), write(First), nl,
    OneMore is Counter + 1,
    list_hyps(Rest, OneMore).
    


detect_and_resolve_contradiction([NewTerm, NewOS], KB, NewKB) :-
    contradicts([NewTerm, NewOS], [OldTerm, OldOS], KB),  !,
    write("Contradiction! Both "), write(NewTerm), write(" and "), write(OldTerm), 
    write(" cannot be true. Select one of the following to unassert: "), nl,
    hyp_set(NewOS, OldOS, HypSet),
    list_hyps(HypSet, 1),
    readln(Num),
    Num = [Entered],
    Lookup is Entered - 1,
    nth0(Lookup, HypSet, ToRemove),
    unassert_hyp(ToRemove, KB, NewKB).
detect_and_resolve_contradiction(_, KB,KB).

contradicts([NewTerm, _], [not(NewTerm), OldOS], KB) :- member([not(NewTerm), OldOS], KB).
contradicts([not(NewTerm), _], [NewTerm, OldOS], KB) :- member([NewTerm, OldOS], KB).

%% The removal of one element from each of the origin sets for a fact is enough to ensure
%% it will not be re-derived. 

    
% If it's in the belief set, figure out who to blame for its belief, remove it, and 
% remove all that follows from it.
disbelieve(Fact, BeliefSet) :- member([Fact, OriginSet], BeliefSet), !.
%% Other case, do nothing.
disbelieve(Fact, BeliefSet).

% Apply each element of the BS to the Term, if the BS element is a rule.
apply_bs_to_new_term(Term, [], [Term]).
apply_bs_to_new_term(Term, [BSFirst|BSRest], NewBSResult) :- 
    is_rule(BSFirst),
    rule_elimination(BSFirst, [Term], NewBS), 
    apply_bs_to_new_term(Term, BSRest, PartBSRest),
    merge_kbs(BSRest, PartBSRest, NewPartBSRest),
    merge_kbs(NewPartBSRest, [BSFirst|NewBS], NewBSResult).
apply_bs_to_new_term(Term, [BSFirst|BSRest], NewBSResult) :- 
    apply_bs_to_new_term(Term, BSRest, NewBSRest),
    merge_kbs([BSFirst], NewBSRest, NewBSResult).

% Believe a term. Try to apply it as a rule to each term in OldBS. 
believe(Term, OldBS, NewBS) :- 
    Term = [_, _],
    apply_bs_to_new_term(Term, OldBS, PartBS),
    rule_elimination(Term, PartBS, NewBS).
believe(Term, OldBS, NewBS) :- 
    Term = [_,_],
    apply_bs_to_new_term(Term, OldBS, NewBS).
believe(Fact, OldBS, FinalBS) :- 
    Term = [Fact, [[hyp, [Fact]]]],
    detect_and_resolve_contradiction(Term, OldBS, NewBS),
    believe(Term, NewBS, FinalBS).

%% From KB, derive everything which follows (the belief set).
% This is combinatorial - try every element of KB alone and with every other element to 
% come up with the full belief set. Do this until we reach a steady state. 

build_belief_set(KB, BeliefSet) :- build_belief_set(KB, [], BeliefSet).
build_belief_set([Term], OldBS, NewBS) :- believe(Term, OldBS, NewBS).
build_belief_set([Term|Rest], OldBS, NewBS) :- 
    believe(Term, OldBS, PartialBS),
    build_belief_set(Rest, PartialBS, NewBS).
    
    
%%%%%%%%%%%%%%%%%
%%% Utilities %%%
%%%%%%%%%%%%%%%%%

joinprint([First,Second|Rest], Text) :- write(First), write(Text), joinprint([Second|Rest], Text).
joinprint([First], _) :- write(First).

showproof([NewTerm,_], Why) :- 
    write("I derived "), 
    write(NewTerm), nl, 
    write("Because I know "),
    joinprint(Why, ' and '), nl.

%%%%%%%%%%%%%%%
%%% Testing %%%
%%%%%%%%%%%%%%%

pretty_print_kb([]).
pretty_print_kb([[Term, OS]|Rest]) :- write(Term), write(" - "), write(OS), nl, pretty_print_kb(Rest).


    
testinfer2 :- 
    assert_hyps([and(a,b), if(b,c)], [], KB), 
    write("---- KB ----"), nl, pretty_print_kb(KB), !,
    build_belief_set(KB, BS),
    write("---- BS ----"), nl , pretty_print_kb(BS), !,
    unassert_hyp(and(a,b), BS, NewBS), 
    write("... Unasserting and(a,b) ..."), nl,
    write("---- BS ----"), nl , pretty_print_kb(NewBS).
   
%% Belief revision in an elevator. 
elevator :-
    assert_hyps([if(onFloor(1), not(onFloor(2))), 
                 if(onFloor(2), not(onFloor(1)))
                 ], [], KB),
    write("---- KB ----"), nl, pretty_print_kb(KB),
    write("... Starting on Floor 1 ..."), nl,
    believe(onFloor(1),KB,NewKB),
    build_belief_set(NewKB, BS),
    write("---- BS ----"),nl , pretty_print_kb(BS),
    write("... Moving to Floor 2 ..."), nl,
    believe(onFloor(2),BS,NextKB),
    build_belief_set(NextKB,FinalBS),
    write("---- BS ----"),nl , pretty_print_kb(FinalBS).
    



