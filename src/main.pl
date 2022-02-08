:-include('queries2.pl').

%para correr faz-se:
% swipl main.pl
% menu.
% 10.    -> opcao de exemplo de uma query
% argumentoDaQuery.   -> escrever o argumento da query

menu :- repeat, nl, nl, nl,
	write('________________________________________________ Sistema de Recomendação de Circuitos de Entrega de Encomendas ______________________________________'), nl, nl,
	write(' 1 - Identificar quais os circuitos com maior número de entregas (por volume e peso)'), nl,
	write(' 2 - Comparar circuitos de entrega tendo em conta os indicadores de produtividade'), nl,
	write(' 3 - Escolher o circuito mais rápido (usando o critério da distância)'), nl,
	write(' 4 - Escolher o circuito mais ecológico (usando um critério de tempo)'), nl,
	write(' 5 - Gerar um circuito de entrega com algoritmo à escolha'), nl,
	write(' 6 - Adicionar uma encomenda'), nl,
	write(' 7 - Mostrar encomendas não entregues'), nl,
	write(' 8- Sair.'), nl, nl,
    write('Digite a opção:'), nl,
	read(Choice), runQuery(Choice), menu.




runQuery(1) :- 
	write("Digite o número de circuitos que pretende:"), read(NCircuitos), nl,
	write("1 - Por volume"), nl,
	write("2 - Por peso"), nl,
	read(Opcao),
	query1_Aux(Opcao, NCircuitos, Res), 
	write(Res).

query1_Aux(1, NCircuitos, Res) :- circuitosComMaisVolume(NCircuitos, Res).
query1_Aux(2, NCircuitos, Res) :- circuitosComMaisPeso(NCircuitos, Res).

runQuery(2) :- 
	write("Digite o número de circuitos que prentende: "),
	read(NumCircuitos),
	circuitosComMaiorProdutividade(NumCircuitos, Sol),
	write(Sol).

runQuery(3) :- 
	write("Insira o ano referente à data de inicio: "), read(AnoPrazo), nl,
	write("Insira o mes referente à data de inicio"), read(MesPrazo), nl,
	write("Insira o dia referente à data de inicio"), read(DiaPrazo), nl,
	write("Insira a hora referente à data de inicio"), read(HoraPrazo), nl,
	write("Insira os minutos referentes à data de inicio"), read(MinPrazo), nl,
	write("Digite o ID da encomenda: "),
	read(IdEncomenda),nl,
	escolherCircuitoMaisRapido(data(AnoPrazo, MesPrazo, DiaPrazo)/hora(HoraPrazo, MinPrazo), IdEncomenda).

runQuery(4) :- 
	write("Insira o ano referente à data de inicio: "), read(AnoPrazo), nl,
	write("Insira o mes referente à data de inicio"), read(MesPrazo), nl,
	write("Insira o dia referente à data de inicio"), read(DiaPrazo), nl,
	write("Insira a hora referente à data de inicio"), read(HoraPrazo), nl,
	write("Insira os minutos referentes à data de inicio"), read(MinPrazo), nl,
	write("Digite o ID da encomenda: "),
	read(IdEncomenda),nl,
	escolherCircuitoMaisEcologico(data(AnoPrazo, MesPrazo, DiaPrazo)/hora(HoraPrazo, MinPrazo), IdEncomenda).

runQuery(5) :- 
	write("1 - A Estrela com criterio distancia."), nl,
	write("2 - A Estrela com criterio tempo."), nl,
	write("3 - Gulosa com estima pela distancia."), nl,
	write("4 - Gulosa com estima pelo tempo."), nl,
	write("5 - Depth First Search."), nl,
	write("6 - Breadth First Search"), nl,
	write("7 - Busca Iterativa Limitada em Profundidade"), nl,
	read(NumeroAlg),
	write("Digite o nodo onde pretende levar a encomenda: "),
	read(Nodo),
	escolheAlgoritmo(NumeroAlg, Nodo, Circuito/NovoCusto),
	write(Circuito/NovoCusto).

runQuery(6) :- 
	write("Insira a morada no formato Freguesia/Rua: "), read(Freguesia/Rua), nl,
	write("Insira o ID (único) da encomenda: "), read(IdEncomenda), nl,
	write("Insira o ID do cliente: "), read(IdCliente), nl,
	write("Insira o ano do prazo para a entrega: "), read(AnoPrazo), nl,
	write("Insira o mes do prazo para a entrega: "), read(MesPrazo), nl,
	write("Insira o dia do prazo para a entrega: "), read(DiaPrazo), nl,
	write("Insira a hora do prazo para a entrega: "), read(HoraPrazo), nl,
	write("Insira os minutos do prazo para a entrega: "), read(MinPrazo), nl,
	write("Insira o peso e o volume no formato Peso/Volume: "), read(Peso/Volume), nl,
	write("Insira o preço: "), read(Preco), nl,
	evolucao(encomenda(Freguesia/Rua, IdEncomenda, IdCliente, data(AnoPrazo, MesPrazo, DiaPrazo), hora(HoraPrazo, MinPrazo), Peso/Volume, Preco)), nl,
	write("Encomenda com o ID "), write(IdEncomenda), write(" adicionada com sucesso! Existem agora "),
	contaEncomendas(NEncomendas), write(NEncomendas), write(" encomendas.").

runQuery(7) :- 
	write("Encomendas não entregues:"), nl,
	encomendasPorEntregar(EncomendasPorEntregar),
	write(EncomendasPorEntregar). 

%predicado que termina o programa
runQuery(8) :- 
	writeln("Até breve!"), nl, halt.

%opcao final que só ocorre se nao entrar em nenhum dos casos anteriores
runQuery(_) :- 
	writeln("A opção não é válida!").
