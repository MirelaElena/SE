/*
cod preluat din cartea(bibliografie[1]):
BALCAN Maria Florina, HRISTEA Florentina, 
Aspecte ale Cautarii si Reprezentarii Cunostintelor in Inteligenta Artificiala,
Editura Universitatii din Bucuresti, 2004, 
pg 216
*/

close_all:-current_stream(_,_,S),close(S),fail;true.

curata_bc:-current_predicate(P), abolish(P,[force(true)]), fail;true.


:-use_module(library(lists)).
:-use_module(library(file_systems)).
:-use_module(library(system)).
:-use_module(library(sockets)).

:-op(900,fy,not).
:-dynamic fapt/3.

:-dynamic interogat/1.

:-dynamic scop/1.

:-dynamic interogabil/3.

:-dynamic regula/3.

:-dynamic solutie/4.


tab(N):-N>0,write(' '),N1 is N-1, tab(N1).
tab(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tab(Stream,N):-N>0,write(Stream,' '),N1 is N-1, tab(Stream,N1).
tab(_,0).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not(P):-P,!,fail.

not(_).


scrie_lista([]):-nl.

scrie_lista([H|T]) :-
write(H), tab(1),
scrie_lista(T).

scrie_lista_fara_tab([]):-nl.
scrie_lista_fara_tab([H|T]) :-
write(H), 
scrie_lista_fara_tab(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scrie_lista(Stream,[]):-nl(Stream),flush_output(Stream).

scrie_lista(Stream,[H|T]) :-
write(Stream,H), tab(Stream,1),
scrie_lista(Stream,T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
afiseaza_fapte :- 
	fapt(av(Atr,Val),FC,_) 
	->
		(write('Fapte existente in baza de cunostinte:'),
		nl,nl, write(' (Atribut,valoare) '), nl,nl,
		listeaza_fapte,nl)
	; (nl, write('Nu exista fapte in baza de cunostinte!')).


listeaza_fapte:-  
fapt(av(Atr,Val),FC,_), 
write('('),write(Atr),write(','),
write(Val), write(')'),
write(','), write(' certitudine '),
FC1 is integer(FC),write(FC1),
nl,fail.

listeaza_fapte.


lista_float_int([],[]).

lista_float_int([Regula|Reguli],[Regula1|Reguli1]):-
(Regula \== utiliz,
Regula1 is integer(Regula);
Regula ==utiliz, Regula1=Regula),
lista_float_int(Reguli,Reguli1).


goleste_director([]).
goleste_director([H - _|T]):- 
	atom_concat('output_demonstratii/', H, Aux),
	delete_file(Aux), goleste_director(T).


pornire :-
write("Pornire"), nl,
(directory_exists('output_demonstratii') 
	-> (file_members_of_directory('output_demonstratii', L) 
		,goleste_director(L)	
		)
	; make_directory('output_demonstratii')
),
retractall(interogat(_)),
retractall(fapt(_,_,_)),
repeat,
nl, nl,
write('Introduceti una din urmatoarele optiuni: '),
nl,nl,
write(' (Incarca Consulta Reinitiaza  Afisare_fapte  Cum   Iesire) '),
nl,nl,write('|: '),citeste_linie([H|T]),
executa([H|T]), H == iesire.


executa([incarca]) :- 
incarca,!,nl,
write('Fisierul dorit a fost incarcat'),nl.

executa([consulta]) :- 
scopuri_princ,!.


executa([reinitiaza]) :- retractall(interogat(_)), retractall(fapt(_,_,_)),!.

executa(Stream, [reinitiaza]) :- write(Stream, 'Am reusit sa reinitializez\n'), flush_output(Stream),
						retractall(interogat(_)), retractall(fapt(_,_,_)),!.

executa([afisare_fapte]) :-
afiseaza_fapte,!.


executa([cum|L]) :- cum(L),!.

executa([nu],L):-!.
executa(Stream, [nu], L):-!.

executa([sumar], L):- afis_list_sol(L).
executa(Stream, [sumar], L):- write('SUMAR'), nl, write('Sol:'), write(L), nl, afis_list_sol(Stream, L),write('SUMAR FINAL'), nl.

executa([complet], L):- afis_list_sol_detaliat(L).
executa(Stream, [complet], L):- write('COMPLET'), nl, write('Sol:'), write(L), nl, afis_list_sol_detaliat(Stream, L), write('COMPLET FINAL'), nl.

executa([iesire]):-!.
executa(Stream, [iesire]):-!.

executa([_|_]) :-
write('Comanda incorecta! '),nl.
meniu_secundar(L):- 
		repeat,
		nl, nl,
		write('Reafiseaza?  '),
		nl,nl,
		write(' ( Nu Sumar  Complet ) '),
		nl,nl,write('|: '),citeste_linie([H|T]),
		executa([H|T], L), H == nu.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
meniu_secundar(Stream,L):- 
		write('Am intrat in meniu secundar'),nl,
		write(Stream,'m(Nu#Sumar#Complet)'),
		nl(Stream), flush_output(Stream),
		repeat,
		write('repeat in meniu secundar'),nl,
		citeste_linie(Stream,[H|T]),
		executa(Stream, [H|T], L), H == nu.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End

scopuri_princ :- 
				 scop(Atr),write('Atributul este '),write(Atr),nl, 
				 setof(st(FC,fapt(av(Atr,Val))), 
					X^(determina(Atr), fapt(av(Atr,Val),FC,X), FC>=40),
					L)
					-> (write('Sunt solutii!'),nl, afis_list_sol_detaliat(L), meniu_secundar(L))
					; write('Nu exista solutii!').	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
scopuri_princ(Stream) :-
	scop(Atr),write('Atributul este '),write(Atr),nl, 
	setof(st(FC,fapt(av(Atr,Val))), 
	X^(determina(Stream,Atr), fapt(av(Atr,Val),FC,X), FC>=40),
	L)
	-> (afis_list_sol_detaliat(Stream,L), 
		write('M-am intors in scopuri principale\n'),
		meniu_secundar(Stream,L))
	; write(Stream,'s(Nu exista solutii!)').	
	
scopuri_princ(_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End
determina(Atr) :- realizare_scop(av(Atr,_),_,[scop(Atr)]),!.
determina(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
determina(Stream,Atr) :-
realizare_scop(Stream,av(Atr,_),_,[scop(Atr)]),!.

determina(_,_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End


scrie_solutie_in_fisier(Av, FC):- 
							  Av =.. [_,Atr, Val], 
							  telling(Output_curent), 
							  atom_concat('output_demonstratii/demonstratie_', Val, A1), 
							  atom_concat(A1, '_fc[', A2), 
							  number_chars(FC, Y), 
							  atom_chars(Factor, Y),
							  atom_concat(A2, Factor, A3), 
							  atom_concat(A3, '].txt', Fisier),
							  tell(Fisier), 
							  cum(Av),
							  told, 
							  tell(Output_curent).

scrie_solutie_fcmax_in_fisier(Av, FC):- 
							  Av =.. [_,Atr, Val], 
							  telling(Output_curent), 
							  atom_concat('output_demonstratii/demonstratie_', Val, A1), 
							  atom_concat(A1, '(max_fc=', A2), 
							  number_chars(FC, Y), 
							  atom_chars(Factor, Y),
							  atom_concat(A2, Factor, A3), 
							  atom_concat(A3, ').txt', Fisier),
							  tell(Fisier), 
							  cum(Av),
							  told, 
							  tell(Output_curent).

afis_list_sol_detaliat([H]):- H =.. [_, FC, Fapt],
					   Fapt =.. [_, Av],
					   Av =.. [_,Atr, Val], 
					   scrie_solutie_fcmax_in_fisier(Av, FC),
					   scrie_scop_detaliat(av(Atr, Val), FC), nl.	
						  
afis_list_sol_detaliat([H|T]):- afis_list_sol_detaliat(T), 
					   H =.. [_, FC, Fapt],
					   Fapt =.. [_, Av],
					   Av =.. [_,Atr, Val], 
					   scrie_solutie_in_fisier(Av, FC),
					   scrie_scop_detaliat(av(Atr, Val), FC), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
afis_list_sol_detaliat(Stream,[H]):- 
		H =.. [_, FC, Fapt],
		Fapt =.. [_, Av],
		Av =.. [_,Atr, Val], 
		scrie_solutie_fcmax_in_fisier(Av, FC),
		scrie_scop_detaliat(Stream,av(Atr, Val), FC).
		   
afis_list_sol_detaliat(Stream,[H|T]):- 
		afis_list_sol_detaliat(Stream,T), 
		H =.. [_, FC, Fapt],
		Fapt =.. [_, Av],
		Av =.. [_,Atr, Val], 
		scrie_solutie_in_fisier(Av, FC),
		scrie_scop_detaliat(Stream,av(Atr, Val), FC).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End

afis_list_sol([H]):- H =.. [_, FC, Fapt],
					   Fapt =.. [_, Av],
					   Av =.. [_,Atr, Val], 
					   scrie_scop(av(Atr, Val), FC), nl.

afis_list_sol([H|T]):- afis_list_sol(T), 
					   H =.. [_, FC, Fapt],
					   Fapt =.. [_, Av],
					   Av =.. [_,Atr, Val], 
					   scrie_scop(av(Atr, Val), FC), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
afis_list_sol(Stream, [H]):- H =.. [_, FC, Fapt],
					   Fapt =.. [_, Av],
					   Av =.. [_,Atr, Val], 
					   scrie_scop(Stream, av(Atr, Val), FC), nl(Stream), flush_output(Stream).

afis_list_sol(Stream, [H|T]):- afis_list_sol(Stream, T), 
					   H =.. [_, FC, Fapt],
					   Fapt =.. [_, Av],
					   Av =.. [_,Atr, Val], 
					   scrie_scop(Stream, av(Atr, Val), FC), nl(Stream), flush_output(Stream).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End

afiseaza_scop(Atr) :- nl,
					  fapt(av(Atr, Val), FC,_),
					  FC >= 40,scrie_scop(av(Atr, Val), FC),
					  nl,
					  fail.

afiseaza_scop(_):-nl,nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
afiseaza_scop(Stream, Atr) :-
						nl,fapt(av(Atr,Val),FC,_),
						FC >= 40,format(Stream,"s(~p este ~p cu fc ~p)",[Atr,Val, FC]),
						nl(Stream),flush_output(Stream),fail.

afiseaza_scop(_,_):- write('A terminat'),nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End

scrie_scop(av(Atr,Val),FC) :-
		transformare(av(Atr,Val), X),
		scrie_lista(X),
		write(' '),
		write('factorul de certitudine este '),
		FC1 is integer(FC),write(FC1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
scrie_scop(Stream, av(Atr,Val),FC) :-
		transformare(av(Atr,Val), X),
		FC1 is integer(FC),
		format(Stream,'s(~s)',[Val]), nl(Stream), flush_output(Stream).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End
scrie_scop_detaliat(av(Atr,Val),FC) :-
										transformare(av(Atr,Val), X),
										scrie_lista(X), nl,write(Val),
										solutie(Val, _, Descriere, _),
										write(Descriere), nl,
										write(' '),
										write('factorul de certitudine este '),
										FC1 is integer(FC),write(FC1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scrie_scop_detaliat(Stream,av(Atr,Val),FC) :-
		transformare(av(Atr,Val), X), 
		solutie(Val, Img, Descriere, Proprietati), 
		Proprietati = [av(Anotimp, SolAnotimp), av(Buget, SolBuget)],
		atom_concat(SolAnotimp, ', ', PropsAux),
		atom_concat(PropsAux, SolBuget, Props),
		FC1 is integer(FC),
		format(Stream,'s(~s#~d#~s#~s#~s)',[Val,FC1, Img, Descriere,Props]), 
		nl(Stream),
		flush_output(Stream).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


realizare_scop(not Scop,Not_FC,Istorie) :-
realizare_scop(Scop,FC,Istorie),
Not_FC is - FC, !.

realizare_scop(av(Atr, _),FC, _) :- fapt(av(Atr, nu_conteaza),FC,_), !.

realizare_scop(Scop,FC,_) :-
fapt(Scop,FC,_), !.

realizare_scop(Scop,FC,Istorie) :-
pot_interoga(Scop,Istorie),
!,realizare_scop(Scop,FC,Istorie).

realizare_scop(Scop,FC_curent,Istorie) :-
fg(Scop,FC_curent,Istorie).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
realizare_scop(Stream,not Scop,Not_FC,Istorie) :-
													realizare_scop(Stream,Scop,FC,Istorie),
													Not_FC is - FC, !.

realizare_scop(_,Scop,FC,_) :-
								fapt(Scop,FC,_), !.

realizare_scop(Stream,Scop,FC,Istorie) :-
											pot_interoga(Stream,Scop,Istorie),
											!,realizare_scop(Stream,Scop,FC,Istorie).

realizare_scop(Stream,Scop,FC_curent,Istorie) :-
fg(Stream,Scop,FC_curent,Istorie).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        
fg(Scop,FC_curent,Istorie) :-
regula(N, premise(Lista), concluzie(Scop,FC)),
demonstreaza(N,Lista,FC_premise,Istorie),
ajusteaza(FC,FC_premise,FC_nou),
actualizeaza(Scop,FC_nou,FC_curent,N),
FC_curent == 100,!.

fg(Scop,FC,_) :- fapt(Scop,FC,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
fg(Stream,Scop,FC_curent,Istorie) :-
regula(N, premise(Lista), concluzie(Scop,FC)),
demonstreaza(Stream,N,Lista,FC_premise,Istorie),
ajusteaza(FC,FC_premise,FC_nou),
actualizeaza(Scop,FC_nou,FC_curent,N),
FC_curent == 100,!.

fg(_,Scop,FC,_) :- fapt(Scop,FC,_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End

pot_interoga(av(Atr,_),Istorie) :-
not interogat(av(Atr,_)),
interogabil(Atr,Optiuni,Mesaj),
interogheaza(Atr,Mesaj,Optiuni,Istorie),nl,
asserta( interogat(av(Atr,_)) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start
pot_interoga(Stream,av(Atr,_),Istorie) :-
										not interogat(av(Atr,_)),
										interogabil(Atr,Optiuni,Mesaj),
										write('Optiunile dinainte de interogheaza sunt '),
										write(Optiuni),nl,nl,
										interogheaza(Stream,Atr,Mesaj,Optiuni,Istorie),nl,
										asserta( interogat(av(Atr,_)) ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End

cum([]) :- write('Scop? '),nl,
write('|:'), citeste_linie(Linie),nl,
transformare(Scop,Linie), cum(Scop).

cum(L) :- 
transformare(Scop,L),nl, cum(Scop).

cum(not Scop) :- 
fapt(Scop,FC,Reguli), 
lista_float_int(Reguli,Reguli1),
FC < -40,transformare(not Scop,PG),
append(PG,[' ',a,fost,derivat,cu, ajutorul, 'regulilor: '|Reguli1],LL),
scrie_lista(LL),nl,afis_reguli(Reguli),fail.

cum(Scop):- 
	write('Scop:'), write(Scop), nl,
	Scop = av(Atr, Val), 
	((fapt(av(Atr, nu_conteaza), FC, Reguli), Scop1 = av(Atr, nu_conteaza))
	;
	(fapt(av(Atr, Val), FC, Reguli), Scop1 = Scop)),
	lista_float_int(Reguli,Reguli1), 
	FC > 40,transformare(Scop1,PG), 
	append(PG,[' ', a,fost,derivat,cu, ajutorul, 'regulilor: '|Reguli1],LL),
	scrie_lista(LL),nl,afis_reguli(Reguli), 
	fail.

cum(_).


afis_reguli([]).

afis_reguli([N|X]) :- 
	afis_regula(N),
	premisele(N), nl,
	afis_reguli(X).

afis_regula(N) :-
	regula(N, premise(Lista_premise),
	concluzie(Scop,FC)),NN is integer(N),
	scrie_lista_fara_tab(['Id_regula@',NN]), 
	scrie_lista(['Premise:']),
	scrie_lista_premise(Lista_premise),
	transformare3(Scop,Scop_tr),
	append(['   '],Scop_tr,L1),
	FC1 is integer(FC), 
	append(L1, ['/', '/', 'FC', '=', FC1], L2), 
	scrie_lista_fara_tab(L2),nl.

	
scrie_lista_premise([]).

scrie_lista_premise([H|T]) :-
	transformare2(H,H_tr),
	tab(5), write('[#] '), scrie_lista_fara_tab(H_tr),
	scrie_lista_premise(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scrie_lista_premise(_,[]).

scrie_lista_premise(Stream,[H|T]) :-
	transformare(H,H_tr),
	tab(Stream,5), write(Stream,'[#] '),scrie_lista(Stream,H_tr),
	scrie_lista_premise(Stream,T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transformare(av(A,da),[A]) :- !.
transformare(not av(A,da), [not,A]) :- !.
transformare(av(A,nu),[not,A]) :- !.
transformare(av(A,V),[A,este,V]).
	
%pt afisarea regulilor	
transformare2(av(A,da),[A]) :- !.
transformare2(not av(A,da), [not, '[', A, ']']) :- !.
transformare2(not av(A,da),[not, '[', A, ']']) :- !.
transformare2(av(A,V),[A,'<', '-',V]).

transformare3(av(A,da), [A, '=', 't']).
transformare3(not av(A, da), [A, '=', 'f']).
transformare3(av(A,V), [A, '=',V]).


premisele(N) :-
	regula(N, premise(Lista_premise), _),
	!, cum_premise(Lista_premise).

        
cum_premise([]).

cum_premise([Scop|X]) :-
	cum(Scop),
	cum_premise(X).

interogheaza(Atr,Mesaj,[da,nu],Istorie) :-
	!,write(Mesaj),nl, write('( da nu nu_stiu nu_conteaza)'), nl,
	de_la_utiliz(X,Istorie,[da,nu, nu_stiu,nu_conteaza]),
	det_val_fc(X,Val,FC),
	asserta( fapt(av(Atr,Val),FC,[utiliz]) ).

interogheaza(Atr,Mesaj,Optiuni,Istorie) :-
	write(Mesaj),nl,
	citeste_opt(VLista,Optiuni,Istorie),
	assert_fapt(Atr,VLista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%            AICI        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     
interogheaza(Stream,Atr,Mesaj,[da,nu],Istorie) :-
	!,write(Stream,i(Mesaj)),nl(Stream),flush_output(Stream),
	write('\n Intrebare atr boolean\n'),
	citeste_opt(Stream,X,[da,nu],Istorie),
	det_val_fc(X,Val,FC),
	asserta( fapt(av(Atr,Val),FC,[utiliz]) ).
	
interogheaza(Stream,Atr,Mesaj,Optiuni,Istorie) :-
	write('\n Intrebare atr val multiple\n'),
	write(Stream,i(Mesaj)),nl(Stream),flush_output(Stream),
	citeste_opt(Stream,VLista,Optiuni,Istorie),
	assert_fapt(Atr,VLista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
citeste_opt(X,Optiuni,Istorie) :-
	append(Optiuni,[nu_stiu, nu_conteaza],Opt2),
	append(['('],Opt2,Opt1),
	append(Opt1,[')'],Opt),
	scrie_lista(Opt),
	de_la_utiliz(X,Istorie,Opt2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
citeste_opt(Stream,X,Optiuni,Istorie) :-
	append(Optiuni,[nu_stiu, nu_conteaza],Opt2),
	append(['('],Opt2,Opt1),
	append(Opt1,[')'],Opt),
		scrie_lista(Stream,Opt),
	de_la_utiliz(Stream,X,Istorie,Opt2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
de_la_utiliz(X,Istorie,Lista_opt) :-
	repeat,write(': '),citeste_linie(X),
	proceseaza_raspuns(X,Istorie,Lista_opt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
de_la_utiliz(Stream,X,Istorie,Lista_opt) :-
	repeat,write('astept raspuns\n'),citeste_linie(Stream,X),format('Am citit ~p din optiunile ~p\n',[X,Lista_opt]),
	proceseaza_raspuns(X,Istorie,Lista_opt), write('gata de la utiliz\n').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proceseaza_raspuns([de_ce],Istorie,_) :- nl,afis_istorie(Istorie),!,fail.

proceseaza_raspuns([X],_,Lista_opt):-
	member(X,Lista_opt).

proceseaza_raspuns([X,fc,FC],_,Lista_opt):-
	member(X,Lista_opt),float(FC).
	



assert_fapt(Atr,[Val,fc,FC]) :-
!,asserta( fapt(av(Atr,Val),FC,[utiliz]) ).

assert_fapt(Atr,[Val]) :-
asserta( fapt(av(Atr,Val),100,[utiliz])).


det_val_fc([nu],da,-100).

det_val_fc([nu,FC],da,NFC) :- NFC is -FC.

det_val_fc([nu,fc,FC],da,NFC) :- NFC is -FC.

det_val_fc([Val,FC],Val,FC).

det_val_fc([Val,fc,FC],Val,FC).

det_val_fc([Val],Val,100).

        
afis_istorie([]) :- nl.

afis_istorie([scop(X)|T]) :-
scrie_lista([scop,X]),!,
afis_istorie(T).

afis_istorie([N|T]) :-
afis_regula(N),!,afis_istorie(T).


demonstreaza(N,ListaPremise,Val_finala,Istorie) :-
dem(ListaPremise,100,Val_finala,[N|Istorie]),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
demonstreaza(Stream,N,ListaPremise,Val_finala,Istorie) :-
dem(Stream,ListaPremise,100,Val_finala,[N|Istorie]),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dem([],Val_finala,Val_finala,_).

dem([H|T],Val_actuala,Val_finala,Istorie) :-
realizare_scop(H,FC,Istorie),
Val_interm is min(Val_actuala,FC),
Val_interm >= 40,
dem(T,Val_interm,Val_finala,Istorie).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dem(_,[],Val_finala,Val_finala,_).

dem(Stream,[H|T],Val_actuala,Val_finala,Istorie) :-
realizare_scop(Stream,H,FC,Istorie),
Val_interm is min(Val_actuala,FC),
Val_interm >= 20,
dem(Stream,T,Val_interm,Val_finala,Istorie).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
actualizeaza(Scop,FC_nou,FC,RegulaN) :-
fapt(Scop,FC_vechi,_),
combina(FC_nou,FC_vechi,FC),
retract( fapt(Scop,FC_vechi,Reguli_vechi) ),
asserta( fapt(Scop,FC,[RegulaN | Reguli_vechi]) ),!.

actualizeaza(Scop,FC,FC,RegulaN) :-
asserta( fapt(Scop,FC,[RegulaN]) ).


ajusteaza(FC1,FC2,FC) :-
X is FC1 * FC2 / 100,
FC is round(X).

combina(FC1,FC2,FC) :-
FC1 >= 0,FC2 >= 0,
X is FC2*(100 - FC1)/100 + FC1,
FC is round(X).

combina(FC1,FC2,FC) :-
FC1 < 0,FC2 < 0,
X is - ( -FC1 -FC2 * (100 + FC1)/100),
FC is round(X).

combina(FC1,FC2,FC) :-
(FC1 < 0; FC2 < 0),
(FC1 > 0; FC2 > 0),
FCM1 is abs(FC1),FCM2 is abs(FC2),
MFC is min(FCM1,FCM2),
X is 100 * (FC1 + FC2) / (100 - MFC),
FC is round(X).


incarca :- 
	write('Introduceti numele fisierului care doriti sa fie incarcate regulile: '),nl, write('|:'),read(F),
	file_exists(F),!,incarca(F),
	write('Introduceti numele fisierului care doriti sa fie incarcate solutiile: '),nl, write('|:'),read(S),
	file_exists(F),!,incarca_sol(S).

incarca:-write('Nume incorect de fisier! '),nl,fail.

incarca(F) :-
retractall(interogat(_)),retractall(fapt(_,_,_)),
retractall(scop(_)),retractall(interogabil(_,_,_)),
retractall(regula(_,_,_)),
see(F),incarca_reguli,seen,!.

incarca_sol(F) :-
retractall(solutii(_,_,_,_)),
see(F),incarca_solutii,seen,!.

incarca_reguli :- 
repeat,citeste_propozitie(L), 
proceseaza(L),L == [end_of_file],nl.

incarca_solutii :- 
repeat,citeste_propozitie(L),
proceseaza(L),L == [end_of_file],nl.
 
proceseaza([end_of_file]):-!.

proceseaza(L) :-
trad(R,L,[]),write(R),nl,nl,
assertz(R), !.

trad(regula(N, premise(Premise), concluzie(Atunci,F))) --> identificator(N),
															premise(Premise),
															atunci(Atunci, F).

trad(scop(X)) --> ['[','scop',X, ']'].


trad(solutie(Denumire,Poza,Descriere,Proprietati)) --> denumire(Denumire),
													   poza(Poza),
													   descriere_sol(Descriere),
													   lista_proprietati(Proprietati).
/*
afis_sol:- solutie(Denumire,Poza,Descriere,Proprietati), write(Denumire),nl,
														write(Poza),nl,
														write(Descriere),nl,
														write(Proprietati),nl,nl, fail ; true.
														*/


trad(interogabil(Atr,M,P)) --> 
['[', Atr], afiseaza(Atr,P), lista_optiuni(M).


trad('Eroare la parsare'-L, L, _).

lista_proprietati([av(Atribut,Valoare)]) -->  [Atribut,Valoare,'[',descriere,']'].
lista_proprietati([av(Atribut,Valoare)|T]) --> [Atribut,Valoare,','], lista_proprietati(T).

lista_optiuni(M) --> lista_de_optiuni(M).

lista_de_optiuni([Element]) -->  [Element,']'].
lista_de_optiuni([Element|T]) --> [Element], lista_de_optiuni(T).


afiseaza(_,P) -->  [P].

afiseaza(P,P) -->  [].

denumire(Denumire) --> ['[',descriere,']',denumire,Denumire].

poza(Poza) --> [';',poza,Poza].

descriere_sol(Descriere) --> [';',descriere_sol,Descriere,';',proprietati].

identificator(N) --> [id_regula,N].


premise(Premise) --> [premise,':'], lista_premise(Premise).


lista_premise([Daca]) --> [ '[' , ']' ], propoz(Daca).

lista_premise([Prima|Celalalte]) --> [ '[' , ']' ], propoz(Prima),lista_premise(Celalalte).


atunci(Atunci,FC) --> propoz(Atunci),[fc],[FC].

atunci(Atunci,100) --> propoz(Atunci).


propoz(not av(Atr,da)) --> [not,'[',Atr,']'].

propoz(av(Atr,Val)) --> [Atr,'-',Val].

propoz(av(Atr,da)) --> [Atr,t]. %pt concluzie booleana

propoz(av(Atr,nu)) --> [Atr,f]. %pt concluzie booleana

propoz(av(Atr,da)) --> [Atr].

propoz(av(Atr,Val)) --> [Atr,Val]. %pt atunci!



citeste_linie([Cuv|Lista_cuv]) :-
get_code(Car),
citeste_cuvant(Car, Cuv, Car1), 
rest_cuvinte_linie(Car1, Lista_cuv).
 
      
% -1 este codul ASCII pt EOF

rest_cuvinte_linie(-1, []):-!.
    
rest_cuvinte_linie(Car,[]) :-(Car==13;Car==10), !.

rest_cuvinte_linie(Car,[Cuv1|Lista_cuv]) :-
citeste_cuvant(Car,Cuv1,Car1),      
rest_cuvinte_linie(Car1,Lista_cuv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%stream%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
citeste_linie(Stream,[Cuv|Lista_cuv]) :-
get_code(Stream,Car),
citeste_cuvant(Stream,Car, Cuv, Car1), write('Cuv = '),write(Cuv),nl,
rest_cuvinte_linie(Stream,Car1, Lista_cuv).
 
      
% -1 este codul ASCII pt EOF

rest_cuvinte_linie(_,-1, []):-!.
    
rest_cuvinte_linie(_,Car,[]) :-(Car==13;Car==10), !.

rest_cuvinte_linie(Stream,Car,[Cuv1|Lista_cuv]) :-
citeste_cuvant(Stream,Car,Cuv1,Car1),      
rest_cuvinte_linie(Stream,Car1,Lista_cuv).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

citeste_propozitie([Cuv|Lista_cuv]) :-
get_code(Car),citeste_cuvant(Car, Cuv, Car1),
rest_cuvinte_propozitie(Car1, Lista_cuv).
    
rest_cuvinte_propozitie(-1, []):-!.
    
rest_cuvinte_propozitie(Car,[]) :-Car==46, !.

rest_cuvinte_propozitie(Car,[Cuv1|Lista_cuv]) :-
citeste_cuvant(Car,Cuv1,Car1),      
rest_cuvinte_propozitie(Car1,Lista_cuv).


citeste_cuvant(-1,end_of_file,-1):-!.

citeste_cuvant(Caracter,Cuvant,Caracter1) :-   
caracter_cuvant(Caracter),!, 
name(Cuvant, [Caracter]),get_code(Caracter1).

citeste_cuvant(Caracter, Numar, Caracter1) :-
caracter_numar(Caracter),!,
citeste_tot_numarul(Caracter, Numar, Caracter1).
 

citeste_tot_numarul(Caracter,Numar,Caracter1):-
determina_lista(Lista1,Caracter1),
append([Caracter],Lista1,Lista),
transforma_lista_numar(Lista,Numar).


determina_lista(Lista,Caracter1):- 
get_code(Caracter), 
(caracter_numar(Caracter),
determina_lista(Lista1,Caracter1),
append([Caracter],Lista1,Lista); 
\+(caracter_numar(Caracter)),
Lista=[],Caracter1=Caracter).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rest_cuvinte_propozitie(_,-1, []):-!.
    
rest_cuvinte_propozitie(_,Car,[]) :-Car==46, !.

rest_cuvinte_propozitie(Stream,Car,[Cuv1|Lista_cuv]) :-
citeste_cuvant(Stream,Car,Cuv1,Car1),      
rest_cuvinte_propozitie(Stream,Car1,Lista_cuv).


citeste_cuvant(_,-1,end_of_file,-1):-!.

citeste_cuvant(Stream,Caracter,Cuvant,Caracter1) :-   
caracter_cuvant(Caracter),!, 
name(Cuvant, [Caracter]),get_code(Stream,Caracter1).

citeste_cuvant(Stream,Caracter, Numar, Caracter1) :-
caracter_numar(Caracter),!,
citeste_tot_numarul(Stream,Caracter, Numar, Caracter1).
 

citeste_tot_numarul(Stream,Caracter,Numar,Caracter1):-
determina_lista(Stream,Lista1,Caracter1),
append([Caracter],Lista1,Lista),
transforma_lista_numar(Lista,Numar).


determina_lista(Stream,Lista,Caracter1):-
get_code(Stream,Caracter), 
(caracter_numar(Caracter),
determina_lista(Stream,Lista1,Caracter1),
append([Caracter],Lista1,Lista); 
\+(caracter_numar(Caracter)),
Lista=[],Caracter1=Caracter).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transforma_lista_numar([],0).

transforma_lista_numar([H|T],N):-
transforma_lista_numar(T,NN), 
lungime(T,L), Aux is exp(10,L),
HH is H-48,N is HH*Aux+NN.


lungime([],0).

lungime([_|T],L):-
lungime(T,L1),
L is L1+1.


% 39 este codul ASCII pt '  '

citeste_cuvant(Caracter,Cuvant,Caracter1) :-
Caracter==39,!,
pana_la_urmatorul_apostrof(Lista_caractere),
L=[Caracter|Lista_caractere],
name(Cuvant, L),get_code(Caracter1).
        

pana_la_urmatorul_apostrof(Lista_caractere):- 
get_code(Caracter),
(Caracter == 39,Lista_caractere=[Caracter];
Caracter\==39,
pana_la_urmatorul_apostrof(Lista_caractere1),
Lista_caractere=[Caracter|Lista_caractere1]).


citeste_cuvant(Caracter,Cuvant,Caracter1) :-          
caractere_in_interiorul_unui_cuvant(Caracter),!,              
((Caracter>64,Caracter<91),!,
Caracter_modificat is Caracter+32;
Caracter_modificat is Caracter),                              
citeste_intreg_cuvantul(Caractere,Caracter1),
name(Cuvant,[Caracter_modificat|Caractere]).
        

citeste_intreg_cuvantul(Lista_Caractere,Caracter1) :-
get_code(Caracter),
(caractere_in_interiorul_unui_cuvant(Caracter),
((Caracter>64,Caracter<91),!, 
Caracter_modificat is Caracter+32;
Caracter_modificat is Caracter),
citeste_intreg_cuvantul(Lista_Caractere1, Caracter1),
Lista_Caractere=[Caracter_modificat|Lista_Caractere1]; \+(caractere_in_interiorul_unui_cuvant(Caracter)),
Lista_Caractere=[], Caracter1=Caracter).


citeste_cuvant(_,Cuvant,Caracter1) :-  
get_code(Caracter),       
citeste_cuvant(Caracter,Cuvant,Caracter1).
 

caracter_cuvant(C):-member(C,[44,59,58,63,33,46,41,40,91,93]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
citeste_cuvant(Stream,Caracter,Cuvant,Caracter1) :-
		Caracter==39,!,
		pana_la_urmatorul_apostrof(Stream,Lista_caractere),
		L=[Caracter|Lista_caractere],
		name(Cuvant, L),get_code(Stream,Caracter1).
				
		
		pana_la_urmatorul_apostrof(Stream,Lista_caractere):-
		get_code(Stream,Caracter),
		(Caracter == 39,Lista_caractere=[Caracter];
		Caracter\==39,
		pana_la_urmatorul_apostrof(Stream,Lista_caractere1),
		Lista_caractere=[Caracter|Lista_caractere1]).
		
		
		citeste_cuvant(Stream,Caracter,Cuvant,Caracter1) :-          
		caractere_in_interiorul_unui_cuvant(Caracter),!,              
		((Caracter>64,Caracter<91),!,
		Caracter_modificat is Caracter+32;
		Caracter_modificat is Caracter),                              
		citeste_intreg_cuvantul(Stream,Caractere,Caracter1),
		name(Cuvant,[Caracter_modificat|Caractere]).
				
		
		citeste_intreg_cuvantul(Stream,Lista_Caractere,Caracter1) :-
		get_code(Stream,Caracter),
		(caractere_in_interiorul_unui_cuvant(Caracter),
		((Caracter>64,Caracter<91),!, 
		Caracter_modificat is Caracter+32;
		Caracter_modificat is Caracter),
		citeste_intreg_cuvantul(Stream,Lista_Caractere1, Caracter1),
		Lista_Caractere=[Caracter_modificat|Lista_Caractere1]; \+(caractere_in_interiorul_unui_cuvant(Caracter)),
		Lista_Caractere=[], Caracter1=Caracter).
		
		
		citeste_cuvant(Stream,_,Cuvant,Caracter1) :-                
		get_code(Stream,Caracter),       
		citeste_cuvant(Stream,Caracter,Cuvant,Caracter1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% am specificat codurile ASCII pentru , ; : ? ! . ) ( [ ] # |   , 35, 124

caractere_in_interiorul_unui_cuvant(C):-
C>64,C<91;C>47,C<58;
C==45;C==95;C>96,C<123.

caracter_numar(C):-C<58,C>=48.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONEXIUNE INTERFATA
inceput:-format('Salutare\n',[]),	flush_output,
				prolog_flag(argv, [PortSocket|_]), %preiau numarul portului, dat ca argument cu -a
												   %portul este atom, nu constanta numerica, asa ca trebuie sa il convertim la numar
				atom_chars(PortSocket,LCifre),
				number_chars(Port,LCifre),%transforma lista de cifre in numarul din 
				socket_client_open(localhost: Port, Stream, [type(text)]),
				proceseaza_text_primit(Stream,0).

				
proceseaza_text_primit(Stream,C):-
				write(inainte_de_citire),nl,
				read(Stream,CevaCitit),
				write(dupa_citire),nl,
				write(CevaCitit),nl,
				proceseaza_termen_citit(Stream,CevaCitit,C).
				
proceseaza_termen_citit(Stream,director(D),C):- %pentru a seta directorul curent
				format(Stream,'Locatia curenta de lucru s-a deplasat la adresa ~p.',[D]),
				format('Locatia curenta de lucru s-a deplasat la adresa ~p',[D]),
				X=current_directory(_,D),
				write(X),
				call(X),
				nl(Stream),
				flush_output(Stream),
				C1 is C+1,
				proceseaza_text_primit(Stream,C1).				
				
				
proceseaza_termen_citit(Stream, incarca(X),C):-
				write(Stream,'Se incearca incarcarea fisierului\n'),
				flush_output(Stream),
				incarca(X),
				C1 is C+1,
				proceseaza_text_primit(Stream,C1).
				
proceseaza_termen_citit(Stream, incarca_sol(X),C):-
				write(Stream,'Se incearca incarcarea fisierului\n'),
				flush_output(Stream),
				incarca_sol(X),
				C1 is C+1,
				proceseaza_text_primit(Stream,C1).
				
proceseaza_termen_citit(Stream, comanda(consulta),C):-
				write(Stream,'Se incepe consultarea'), nl(Stream), flush_output(Stream),
				flush_output(Stream),
				scopuri_princ(Stream),
				C1 is C+1,
				proceseaza_text_primit(Stream,C1).
				
proceseaza_termen_citit(Stream, comanda(reinitiaza),C):-
				write(Stream, 'Se incepe reinitializarea'), nl(Stream), flush_output(Stream),
				executa(Stream, [reinitiaza]), write('2'),
				C1 is C+1,
				proceseaza_text_primit(Stream,C1).		
						
				
proceseaza_termen_citit(Stream, X, _):- % cand vrem sa-i spunem "Pa"
				(X == end_of_file ; X == exit),
				write(gata),nl,
				close(Stream).

proceseaza_termen_citit(Stream, Altceva,C):-
				write(Stream,'nu inteleg ce vrei sa spui: '),write(Stream,Altceva),nl(Stream),
				flush_output(Stream),
				C1 is C+1,
				proceseaza_text_primit(Stream,C1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END -> CONEXIUNE INTERFATA
