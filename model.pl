%%%%%%%%%%%
%%% NLP %%%
%%%%%%%%%%%

%% Parsing %%

sentence(s(VP)) --> verb_phrase(VP).
sentence(s(VP)) --> verb_phrase(VP), dot.


noun_phrase(np(Adjp, Noun)) --> adj_phrase(Adjp), noun(Noun).
noun_phrase(np(Det,Noun))--> det(Det),noun(Noun).
noun_phrase(np(Noun))--> noun(Noun).
noun_phrase(np(Noun,Conj,NP)) --> noun(Noun),conj(Conj),noun_phrase(NP).

verb_phrase(vp(Propnoun,Verb, NP)) --> propnoun(Propnoun),verb(Verb), noun_phrase(NP).
verb_phrase(vp(Propnoun,Verb, Det,NP)) --> propnoun(Propnoun),verb(Verb),det(Det), noun_phrase(NP).

adj_phrase(adjp(Adj)) --> adj(Adj).
adj_phrase(adjp(Comma,Adjp))-->comma(Comma),adj_phrase(Adjp).
adj_phrase(adjp(Conj,Adjp)) --> conj(Conj),adj_phrase(Adjp).
adj_phrase(adjp(Adj, Adjp)) --> adj(Adj), adj_phrase(Adjp).

conj(c(and))-->[and].
verb(v(want)) --> [want].
det(d(a)) --> [a].
propnoun(pn(i))-->[i].
comma(comma(',')) -->[',']. 

%%adj(a()) --> [].
adj(adj(spicy)) --> [spicy].
adj(a(fastfood)) --> [fastfood].
adj(a(sitdown)) --> [sitdown].
adj(a(delivery)) --> [delivery].
adj(a(cheap)) --> [cheap].
adj(a(average)) --> [average].
adj(a(expensive)) --> [expensive].


%%noun(n()) -->[].
noun(n(food)) -->[food].
noun(n(american)) -->[american].
noun(n(burger)) -->[burger].
noun(n(wings)) -->[wings].
noun(n(steak)) -->[steak].
noun(n(beef)) -->[beef].
noun(n(chineese)) -->[chineese].
noun(n(rice))-->[rice].
noun(n(chicken))-->[chicken].
noun(n(lomein)) -->[lomein].
noun(n(dumplings)) -->[dumplings].
noun(n(japanese)) -->[japanese].
noun(n(sushi)) -->[sushi].
noun(n(seafood)) -->[seafood].
noun(n(pizza)) -->[pizza].
noun(n(pasta)) -->[pasta].
noun(n(tacos)) -->[tacos].
noun(n(burritos)) -->[burritos].


dot --> ['.'].
dot -->[].

%%%%%%%%%%%
%%%Test %%%
%%%%%%%%%%%

testall():-parse(List),category_selection(List,ResultCat,ResultScore),write("Selected: "),write(ResultCat),nl,write("Score: "),write(ResultScore).

%%%%%%%%%%%
%%%Utils%%%
%%%%%%%%%%%

sort_words(List,Nouns,Adjs):-
    List = [],Nouns=[],Adjs=[].
sort_words(List,Nouns,Adjs):-
    List = [Word|Tail],nouns_list(State),member(Word,State),sort_words(Tail,NewNouns,Adjs),Nouns = [Word|NewNouns].
sort_words(List,Nouns,Adjs):-
    List = [Word|Tail],adj_list(State),member(Word,State),sort_words(Tail,Nouns,NewAdjs),Adjs = [Word|NewAdjs].

read_word_list(Ws) :-
    read_line_to_codes(user_input,Cs),
    atom_codes(A,Cs),
    tokenize_atom(A,Ws).


parse(List):-write("Enter what you want to eat"),nl,read_word_list(IN), extract_words(IN,List).

%%%%%%%%%%%%%%%%%
%%Compare Nouns%%
%%%%%%%%%%%%%%%%%
category_selection(List,ResultCat,ResultScore):-features_cats(SampleList),Score = 0,get_cat(List,american,Score,SampleList,ResultCat,ResultScore).

get_cat(_,BestCategory,BestScore,AllCategories,ResultCat,ResultScore):-
    AllCategories=[],ResultScore = BestScore,ResultCat=BestCategory.

get_cat(List,BestCategory,BestScore,AllCategories,ResultCat,ResultScore):-
    Score = 0,
    AllCategories=[Selected|Tail],
    Selected = [TrialCategory,TrialFeatures|_],
    compare_features(List,TrialCategory,TrialFeatures,Score,BestScore,BestCategory,Out,NewBestScore),
    get_cat(List,Out,NewBestScore,Tail,ResultCat,ResultScore).

%%Base case for compare
compare_features(List,TrialCategory,_,Score,BestScore,BestCategory,Out,NewBestScore):- 
    List = [],Score > BestScore, Out = TrialCategory,NewBestScore is Score.
compare_features(List,TrialCategory,_,Score,BestScore,BestCategory,Out,NewBestScore):- 
    List = [],Score is BestScore, Out = BestCategory,NewBestScore is Score.
compare_features(List,TrialCategory,_,Score,BestScore,BestCategory,Out,NewBestScore):- 
    List = [],Score < BestScore, Out = BestCategory, NewBestScore is BestScore.
%%Check the first element of the list and pass in the next
compare_features(List,TrialCategory,TrialFeatures,Score,BestScore,BestCategory,Out,NewBestScore):-
    List = [Feature|Tail],
    member(Feature,TrialFeatures),NewScore is Score + 1,
    compare_features(Tail,TrialCategory,TrialFeatures,NewScore,BestScore,BestCategory,Out,NewBestScore).
compare_features(List,TrialCategory,TrialFeatures,Score,BestScore,BestCategory,Out,NewBestScore):-
    List = [Feature|Tail],
    \+member(Feature,TrialFeatures),NewScore is Score,
    compare_features(Tail,TrialCategory,TrialFeatures,NewScore,BestScore,BestCategory,Out,NewBestScore).
%%%%%%%%%%%%%%%%%
%%%% Extract %%%%
%%%%%%%%%%%%%%%%%

%%Extract a noun list needs to be refined to accept adj's
extract_words(In,List):- sentence(Parse,In,[]),extract(Parse,NP),retrieve([NP],List).

%%Full sentence rules
extract(s(vp(pn(i),v(want), NounPhrase)),NounPhrase).
extract(s(vp(pn(i),v(want),d(a), NounPhrase)),NounPhrase).

%%Noun Phrases rules
extract(np(adjp(Adjp),n(Noun)),Adjp,Noun).
extract(np(adjp(adj(Adj),Adjp),n(Noun)),Adj,Adjp,Noun).

%%Adjp rules
extract(adjp(adj(Adj)),Adj).
extract(adjp(comma(,),adjp(Adjp)),Adjp).
extract(adjp(c(and),adjp(Adjp)),Adjp).

%Adj
extract(adj(Adj),Adj).

%%Get nouns from
extract(np(det(a),n(Noun)),Noun).
extract(np(n(Noun)),Noun).
extract(np(n(Noun),c(and),NounPhrase),Noun,NounPhrase).


%%%%%%%%%%%%%%%%
%Noun Retrieval%
%%%%%%%%%%%%%%%%
%%Base case
retrieve(NounPhrases,Out):- NounPhrases=[],Out=[].
%%noun is by its self
retrieve(NounPhrases,Out):-
   NounPhrases=[Noun|Tail],
   \+extract(Noun,_),\+extract(Noun,_,_),\+extract(Noun,_,_,_),
   retrieve(Tail,NewList),Out=[Noun|NewList].

retrieve(NounPhrases,Out):- 
    NounPhrases=[NP|Tail],extract(NP,NewNp1,NewNp2,NewNp3),
    append([NewNp1],Tail,Temp1),append([NewNp2],Temp1,Temp2),append([NewNp3],Temp2,Temp3),
    retrieve(Temp3,Out).
%%two noun phrases inside an np
retrieve(NounPhrases,Out):-
    NounPhrases=[NP|Tail],extract(NP,NewNp1,NewNp2),append([NewNp1],Tail,Temp1),append([NewNp2],Temp1,Temp2),retrieve(Temp2,Out).
%%one noun phrase inside an np
retrieve(NounPhrases,Out):-
    NounPhrases=[NP|Tail],extract(NP,NewNp),append([NewNp],Tail,Temp1),retrieve(Temp1,Out).


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



%%%%%%%%%%%%%%%%%%%%%%
%%%   Model Code   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%
%%Knowledge base form%
%%%%%%%%%%%%%%%%%%%%%%

%%Features of categories in the form of a list with [Category,[features]]
features_cats([
    [american,[burger,wings,steak,beef,chicken]],
    [chinese, [chicken,dumplings,noodles,rice,beef]],
    [japanese,[sushi,seafood,rice]],
    [italian, [pizza,pasta]],
    [mexican, [tacos,burritos,rice]]
]
).
nouns_list([
    burger,wings,steak,beef,chicken,dumplings,
    noodles,rice,sushi,seafood,pizza,pasta,tacos,
    burritos]).
adj_list([
    spicy,fastfood,sitdown,delivery,cheap,expensive,average
    ]).


american(buffalowildwings).
american(applebees).
american(mcdonalds).
american(rubytuesdays).
american(chickfila).
american(kfc).
american(cheesecakefactory).
american(texasroadhouse).

chinese(pfchangs).
chinese(pandaexpress).
chinese(chowcity).
chinese(kq).

japanese(koto).
japanese(kiyomi).
japanese(snakebomb).
japanese(oceansushi).
japanese(ichiro).

italian(olivegarden).
italian(carrabbas).
italian(bucadibeppo).
italian(pizzahut).
italian(dominos).

mexican(tacobell).
mexican(chipotle).
mexican(fajitagrill).
mexican(aztecha).
mexican(laparrila).

expensive(texasroadhouse).
expensive(cheesecakefactory).
expensive(pfchangs).

normalprice(buffalowildwings).
normalprice(applebees).
normalprice(chickfila).
normalprice(pandaexpress).
normalprice(kiyomi).
normalprice(koto).
normalprice(olivegarden).
normalprice(carrabbas).
normalprice(chipotle).
normalprice(fajitagrill).

cheap(mcdonalds).


test:- assert_hyps([if(category(american),not(category(mexican))),if(category(mexican),not(category(american)))],[],KB).
    