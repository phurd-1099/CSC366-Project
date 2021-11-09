%%%%%%%%%%%
%%% NLP %%%
%%%%%%%%%%%

%% Parsing %%

sentence(s(VP)) --> verb_phrase(VP).
sentence(s(VP)) --> verb_phrase(VP), dot.


noun_phrase(np(Adjp, Noun)) --> adj_phrase(Adjp), noun(Noun).
noun_phrase(np(Det,Noun))--> det(Det),noun(Noun).
noun_phrase(np(Noun))--> noun(Noun).

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
adj(a(spicy)) --> [spicy].


%%noun(n()) -->[].
noun(n(food)) -->[food].

dot --> ['.'].
dot -->[].



%%%%%%%%%%%
%%%Utils%%%
%%%%%%%%%%%

read_word_list(Ws) :-
    read_line_to_codes(user_input,Cs),
    atom_codes(A,Cs),
    tokenize_atom(A,Ws).

parse():-read_word_list(IN),sentence(Parse,IN,[]),write(Parse).
