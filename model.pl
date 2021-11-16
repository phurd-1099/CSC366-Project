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
noun(n(chinese)) -->[chinese].
noun(n(rice))-->[rice].
noun(n(chicken))-->[chicken].
noun(n(noodles)) -->[noodles].
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


%%%%%%%%%%%%%%%%%%%%%%
%%%   Model Code   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%
%%Main Loop%%
%%%%%%%%%%%%%

loop():- knowledge_base(KB),parse(List),sort_words(List,Categories,Nouns,Price,Mode),select_res(Categories,Nouns,Price,Mode,KB,Selected),write(Selected).
    

%%%%%%%%%%%%%%%%%%%%%%%
%%Recommendation loop%%
%%%%%%%%%%%%%%%%%%%%%%%

select_res(Categories,Nouns,Price,Mode,KB,Selected):-selectloop(Categories,Nouns,Price,Mode,KB,List,Category),getshortlist(Nouns,KB,List,Options),priceloop(Price,KB,Options,Refined),Refined = [Selected|_].

%%%%%%%%%%%%%%%%%%%%%
%%Initial selection%%
%%%%%%%%%%%%%%%%%%%%%

selectloop(Categories,Nouns,Price,Mode,KB,List,Category):-
    Categories = [],
    category_selection(Nouns,ResultCat,ResultScore),write("Selected Category: "),write(ResultCat),nl,
    findall(Restraunt,member(is(Restraunt,ResultCat),KB),List),Category=ResultCat.

selectloop(Categories,Nouns,Price,Mode,List,Category):-
    \+Categories = [],length(Categories, 1),Categories=[ResultCat|_],write(ResultCat),
    findall(Restraunt,member(is(Restraunt,ResultCat),KB),List),Category=ResultCat.

selectloop(Categories,Nouns,Price,Mode,List,Category):-
    \+Categories = [],\+length(Categories, 1),write("Multiple categories selected please refine to one categories"),false.

%%%%%Get a list of Restraunts that fits the nouns provided%%%%%
getshortlist(Nouns,KB,List,Options):-
    Nouns=[],NewList = [].

getshortlist(Nouns,KB,List,Options):-
    Nouns = [Word|Tail],findall(Next,(member(has(Next,Word),KB),member(Next,List)),Bag),getshortlist(Tail,KB,List,NewOptions),append(NewOptions,Bag,Options).


priceloop(Price,KB,Options,Refined):-
    Price=[],SelectedPrice=normalprice,findall(Restraunt,(member(is(Restraunt,normalprice),KB),member(Restraunt,Options)),Bag),(Bag=[],Refined=Options;\+Bag=[],Refined = Bag).
priceloop(Price,KB,Options,Refined):-
    \+Price=[],length(Price,1),Price=[Selected|Price],findall(Restraunt,(member(is(Restraunt,normalprice),KB),member(Restraunt,Options)),Bag),(Bag=[],Refined=Options;\+Bag=[],Refined = Bag).
priceloop(Price,_,_,_):-
    \+Price=[],\+length(Price,1),write("To many price indicators please refine"),false.



%%%%%%%%%%%
%%%Utils%%%
%%%%%%%%%%%

sort_words(List,Categories,Nouns,Price,Mode):-
    List = [],Nouns=[],Adjs=[].
sort_words(List,Categories,Nouns,Price,Mode):-
    ist = [Word|Tail],categories_list(State),member(Word,State),sort_words(Tail,NewCategories,Nouns,Price,Mode),Categories = [Word|NewCategories].
sort_words(List,Categories,Nouns,Price,Mode):-
    List = [Word|Tail],nouns_list(State),member(Word,State),sort_words(Tail,Categories,NewNouns,Price,Mode),Nouns = [Word|NewNouns].
sort_words(List,Categories,Nouns,Price,Mode):-
    List = [Word|Tail],price_list(State),member(Word,State),sort_words(Tail,Categories,Nouns,NewPrice,Mode),Price = [Word|NewAdjs].
sort_words(List,Categories,Nouns,Price,Mode):-
    List=[Word|Tail],mode_list(State),member(Word,State),sort_words(Tail,Categories,Nouns,Price,NewMode),Mode=[Word|NewMode].

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
extract(a(Adj),Adj).


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


%%%%%%%%%%%%%%%%%%%%%%
%%Knowledge base form%
%%%%%%%%%%%%%%%%%%%%%%

%%Features of categories in the form of a list with [Category,[features]]
features_cats([
    [american,[burger,wings,steak,beef,chicken,american]],
    [chinese, [chicken,dumplings,noodles,rice,beef,chinese]],
    [japanese,[sushi,seafood,rice,japanese]],
    [italian, [pizza,pasta,italian]],
    [mexican, [tacos,burritos,rice,mexican]]
]
).
nouns_list([
    burger,wings,steak,beef,chicken,dumplings,
    noodles,rice,sushi,seafood,pizza,pasta,tacos,
    burritos,spicy]).
price_list([
    cheap,expensive,average
    ]).
mode_list([
    fastfood,sitdown,delivery]).
categories_list([
    american,japanese,chinese,italian,mexican]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Pre extablished KB%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
knowledge_base([
    %%%%%%%%%%%%%%%%%%%%
    %%%% Categories %%%%
    %%%%%%%%%%%%%%%%%%%%

    %%%%American food%%%%
    is(buffalowildwings,american),is(applebees,american),is(mcdonalds,american),is(rubytuesdays,american),is(chickfila,american),is(kfc,american),is(cheesecakefactory,american),is(texasroadhouse,american),
    %%%%%% Chinese %%%%%%
    is(pfchangs,chisese),is(pandaexpress,chinese),is(chowcity,chinese),is(kq,chinese),
    %%%%% Japanese  %%%%%
    is(koto,japanese),is(kiyomi,japanese),is(snakebomb,japanese),is(oceansushi,japanese),is(ichiro,japanese),
    %%%%%  Italian  %%%%%
    is(olivegarden,italian),is(carrabbas,italian),is(bucadibeppo,italian),is(pizzahut,italian),is(dominos,italian),
    %%%%% Mexican %%%%%%
    is(tacobell,mexican),is(chipotle,mexican),is(fajitagrill,mexican),is(aztecha,mexican),is(laparrila,mexican),

    %%%%%%%%%%%%%%%%%%%%
    %%%%%  PRICE  %%%%%%
    %%%%%%%%%%%%%%%%%%%%

    %%%% Expensive %%%%%
    is(texasroadhouse,expensive),is(cheesecakefactory,expensive),is(pfchangs,expensive),
    %%% Normal Price %%%
    is(buffalowildwings,normalprice),is(applebees,normalprice),is(chickfila,normalprice),is(pandaexpress,normalprice),is(kiyomi,normalprice),is(koto,normalprice),is(olivegarden,normalprice),is(chipotle,normalprice),is(carrabbas,normalprice),is(fajitagrill,normalprice),
    %%%%%% Cheap %%%%%%%
    is(mcdonalds,cheap),is(kfc,cheap),
    
    %%%%%%%%%%%%%%%%%%%%
    %%%%% KeyWords %%%%%
    %%%%%%%%%%%%%%%%%%%%

    has(buffalowildwings,wings),has(applebees,burger),has(mcdonalds,burger),has(rubytuesdays,steak),has(chickfila,chicken),has(kfc,chicken),has(texasroadhouse,steak),has(cheesecakefactory,beef),

    has(pfchangs,dumplings),has(pandaexpress,noodles),has(chowcity,rice),has(pfchangs,rice),has(chowcity,noodles),has(kq,beef),has(kq,chicken),

    has(koto,rice),has(kiyomi,sushi),has(kiyomi,seafood),has(snakebomb,rice),has(snakebomb,sushi),has(oceansushi,sushi),has(ichiro,rice),

    has(olivegarden,pasta),has(carrabbas,pasta),has(bucadibeppo,pasta),has(pizzahut,pizza),has(dominos,pizza),

    has(tacobell,tacos),has(tacobell,burritos),has(chipotle,rice),has(chipotle,burritos),has(fajitagrill,burritos),has(aztecha,burritos),has(laparrila,rice)
    
    ]).