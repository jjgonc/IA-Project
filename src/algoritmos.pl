% Includes
:-include('base-de-conhecimento.pl').


goal(gualtar/green_distribution).


%--------------------------- estratégia de pesquisa informada gulosa tendo em conta CustoDistância ------------
adjacenteD([Nodo|Caminho]/CustoD/_, [ProxNodo,Nodo|Caminho]/NovoCusto/EstD):-
    aresta(Nodo, ProxNodo, PassoCustoD, _),
    \+ member(ProxNodo,Caminho),
    NovoCusto is CustoD + PassoCustoD,
    estima(ProxNodo,EstD,_).

resolve_gulosaD(Nodo,Caminho/CustoD) :- 
        estima(Nodo, EstimaD,_),
        agulosaD([[Nodo]/0/EstimaD], InvCaminho/CustoD/_),
        reverse(InvCaminho, Caminho),
        !.

agulosaD(Caminhos, Caminho) :-
    obtem_melhor_g_D(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

agulosaD(Caminhos, SolucaoCaminho) :-
    obtem_melhor_g_D(Caminhos,MelhorCaminho),
    remove(MehorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaD(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaD(NovoCaminhos,SolucaoCaminho).


obtem_melhor_g_D([Caminho],Caminho) :- !.
obtem_melhor_g_D([Caminho1/CustoD1/Est1, _/CustoD2/Est2|Caminhos], MelhorCaminho) :-
    Est1 =< Est2, !,
    obtem_melhor_g_D([Caminho1/CustoD1/Est1|Caminhos], MelhorCaminho).
obtem_melhor_g_D([_|Caminhos], MelhorCaminho) :- obtem_melhor_g_D(Caminhos, MelhorCaminho).

expande_gulosaD(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, adjacenteD(Caminho,NovoCaminho),ExpCaminhos).


%--------------------------- estratégia de pesquisa informada gulosa tendo em conta CustoTempo ------------

adjacenteT([Nodo|Caminho]/CustoD/_, [ProxNodo,Nodo|Caminho]/NovoCustoD/EstT):-
    aresta(Nodo, ProxNodo, PassoCustoD, PassoCustoT),
    \+ member(ProxNodo,Caminho),
    NovoCustoD is CustoD + PassoCustoD,
    estima(ProxNodo,_,EstT).

resolve_gulosaT(Nodo,Caminho/CustoD) :- 
        estima(Nodo,_, EstimaT),
        agulosaT([[Nodo]/0/EstimaT], InvCaminho/CustoD/_),
        reverse(InvCaminho, Caminho),
        !.

agulosaT(Caminhos, Caminho) :-
    obtem_melhor_g_T(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

agulosaT(Caminhos, SolucaoCaminho) :-
    obtem_melhor_g_T(Caminhos,MelhorCaminho),
    remove(MehorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaT(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaT(NovoCaminhos,SolucaoCaminho).


obtem_melhor_g_T([Caminho],Caminho) :- !.
obtem_melhor_g_T([Caminho1/CustoD1/Est1, _/_/Est2|Caminhos], MelhorCaminho) :-
    Est1 =< Est2, !,
    obtem_melhor_g_T([Caminho1/CustoD1/Est1|Caminhos], MelhorCaminho).
obtem_melhor_g_T([_|Caminhos], MelhorCaminho) :- 
    obtem_melhor_g_T(Caminhos, MelhorCaminho).

expande_gulosaT(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, adjacenteT(Caminho,NovoCaminho),ExpCaminhos).


%--------------------------- estratégia de pesquisa informada estrela tendo em conta CustoDist ------------



adjacente2([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
    aresta(Nodo,ProxNodo,PassoCusto, _),
    not(member(ProxNodo,Caminho)),
    NovoCusto is Custo + PassoCusto,
    estima(ProxNodo,Est,_).



obtem_melhor([Caminho],Caminho) :- !.
obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos],MelhorCaminho) :-
        Est1 + Custo1 =< Est2 + Custo2, !,
        obtem_melhor([Caminho1/Custo1/Est1|Caminhos],MelhorCaminho).

obtem_melhor([_|Caminhos],MelhorCaminho) :-
        obtem_melhor(Caminhos,MelhorCaminho).


expande_aestrela(Caminho,ExpCaminhos) :-
        findall(NovoCaminho, adjacente2(Caminho,NovoCaminho),ExpCaminhos).


aestrela(Caminhos, Caminho) :-
        obtem_melhor(Caminhos, Caminho),
        Caminho = [Nodo|_]/_/_,
        goal(Nodo).

aestrela(Caminhos, SolucaoCaminho) :-
        obtem_melhor(Caminhos, MelhorCaminho),
        remove(MelhorCaminho, Caminhos, OutrosCaminhos),
        expande_aestrela(MelhorCaminho, ExpCaminhos),
        append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
        aestrela(NovoCaminhos, SolucaoCaminho).


resolve_aestrelaD(Nodo, Caminho/Custo) :-
        estima(Nodo, Estima,_),
        aestrela([[Nodo]/0/Estima],InvCaminho/Custo/_),
        reverse(InvCaminho,Caminho),!.


%--------------------------- estratégia de pesquisa informada estrela tendo em conta CustoTempo ------------



adjacente2T([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
    aresta(Nodo,ProxNodo,_,PassoCusto),
    not(member(ProxNodo,Caminho)),
    NovoCusto is Custo + PassoCusto,
    estima(ProxNodo,_,Est).



expande_aestrelaT(Caminho,ExpCaminhos) :-
        findall(NovoCaminho, adjacente2T(Caminho,NovoCaminho),ExpCaminhos).


aestrelaT(Caminhos, Caminho) :-
        obtem_melhor(Caminhos, Caminho),
        Caminho = [Nodo|_]/_/_,
        goal(Nodo).

aestrelaT(Caminhos, SolucaoCaminho) :-
        obtem_melhor(Caminhos, MelhorCaminho),
        remove(MelhorCaminho, Caminhos, OutrosCaminhos),
        expande_aestrelaT(MelhorCaminho, ExpCaminhos),
        append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
        aestrelaT(NovoCaminhos, SolucaoCaminho).


resolve_aestrelaT(Nodo, Caminho/Custo) :-
        estima(Nodo, Estima,_),
        aestrelaT([[Nodo]/0/Estima],InvCaminho/_/_),
        reverse(InvCaminho,Caminho),
        calculaCusto(Caminho,Custo).


%--------------------------- estratégia de pesquisa não informada profundidade ------------

dfs(Nodo, [Nodo|Caminho], C):-
	profundidadeprimeiro(Nodo, [Nodo], Caminho, C).

profundidadeprimeiro(Nodo, _, [], 0):-
	goal(Nodo).

profundidadeprimeiro(Nodo, Historico, [ProxNodo|Caminho], C):-
	adjacente(Nodo, ProxNodo, C1),
	not(member(ProxNodo, Historico)),
	profundidadeprimeiro(ProxNodo, [ProxNodo|Historico], Caminho, C2),
    C is C1 + C2.

adjacente(Nodo, ProxNodo, C) :- aresta(Nodo, ProxNodo, C, _).

%--------------------------- estratégia de pesquisa não informada largura ------------
bfs(Nodo,Caminho,Custo):-
    goal(Final),
    bfs1(Final,[[Nodo]],Caminho),
    calculaCusto(Caminho,Custo).

bfs1(Final,[[Final|XS]|_],Caminho):- reverse([Final|XS],Caminho).

bfs1(Final,[Atuais|Outros],Caminho):-
    Atuais = [Act|_],
    Act \== Final,
    findall([X|Atuais], (aresta(Act,X,C1, _) ,not(member(X,Atuais))),Novos),
    append(Outros,Novos,Todos),
    bfs1(Final,Todos,Caminho).


%--------------------------- estratégia de pesquisa não informada - Busca Iterativa Limitada em Profundidade ------------

resolve_limitada(Nodo,Caminho/Custo) :-
    goal(Final),
    parede(Limite,5,5),
    depthFirstIterativeDeepening(Nodo,Final,0,Limite,Caminho),
    calculaCusto(Caminho,Custo).

depthFirstIterativeDeepening(Final,Final,Profundidade,Limite,[Final]) :- 
    Profundidade<Limite.


depthFirstIterativeDeepening(Nodo,Final,Profundidade,Limite,[Nodo|RestCaminho]) :-
    Profundidade < Limite,
    ProfundidadeAux is Profundidade+1,
    aresta(Nodo,ProxNodo,CustDist,CustTempo),
    depthFirstIterativeDeepening(ProxNodo,Final,ProfundidadeAux,Limite,RestCaminho).

parede(X,X,_).
parede(X,N,Inc) :- 
    NAux is N+Inc,
    parede(X,NAux,Inc).
% ----------------------------- funcoes auxiliares -----------------

% retira n elementos de uma lista 

take(0, _, []) :- !.

take(_,[],[]).

take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).


%calcula custo de um caminho

calculaCusto([X,Y|[]],C1):-
    aresta(X,Y,C1,_).

calculaCusto([X,Y|XS],Custo):-
    aresta(X,Y,C1,_),
    calculaCusto([Y|XS],C2),
    Custo is C1 + C2.


calculaTempo([X,Y|[]],T1):-
	aresta(X,Y,_,T1).

calculaTempo([X,Y|XS],Tempo):-
    aresta(X,Y,_, T1),
    calculaCusto([Y|XS],T2),
    Tempo is T1 + T2.


getVertente(TipoVeiculo, Res) :-
	veiculo(TipoVeiculo, _, _, Res).


apagacabeca([],[]).
apagacabeca([X],[]).
apagacabeca([H|T],T).

remove(X,[X|R],R ).
remove(X,[Y|R],[Y|L]) :-
    X \= Y,
    remove(X, R, L).



inverso(Xs,Ys):-
	inverso(Xs,[],Ys).

inverso([],Xs,Xs).
inverso([X|Xs],Ys,Zs):-
	inverso(Xs,[X|Ys],Zs).

seleciona(E,[E|Xs],Xs).
seleciona(E,[X|Xs],[X|Ys]) :- seleciona(E,Xs,Ys).

nao(Questao) :-
    Questao,
	!,
	fail.
nao(Questao).

membro(X,[X|_]).
membro(X,[_|Xs]):-
	membro(X,Xs).		

escrever([]).
escrever([X|L]):- 
	write(X),
	nl,
	escrever(L).
