autokolcsonzo(X,megoldas(X,_,_,_,_)).
autokolcsonzo(X,megoldas(_,X,_,_,_)).
autokolcsonzo(X,megoldas(_,_,X,_,_)).
autokolcsonzo(X,megoldas(_,_,_,X,_)).
autokolcsonzo(X,megoldas(_,_,_,_,X)).

grafikus(h(X,_,_,_),X).
megbizo(h(_,X,_,_),X).
kolcsonzo(h(_,_,X,_),X).
nap(h(_,_,_,X),X).

nemfurgon(kamion).
nemfurgon(szemelyauto).
nemfurgon(teherauto).
nemfurgon(mikrobusz).

nemteherauto(kamion).
nemteherauto(szemelyauto).
nemteherauto(furgon).
nemteherauto(mikrobusz).

nemgedeon(gergely).
nemgedeon(geza).
nemgedeon(greta).
nemgedeon(gusztav).

nemgizella(gabor).
nemgizella(gaspar).
nemgizella(gabriella).
nemgizella(gerda).

nemgeza(gedeon).
nemgeza(greta).
nemgeza(gergely).
nemgeza(gusztav).

ferfi(gabor).
ferfi(gaspar).
no(gabriella).
no(gerda).
no(gizella).

kamionosvszemelyautomegbizas(kamion).
kamionosvszemelyautomegbizas(szemelyauto).

furgonvkamion(kamion).
furgonvkamion(furgon).



azonosnemuno(A,B):-
    no(A),no(B).

azonosnemuferfi(A,B):-
	ferfi(A),ferfi(B).

azonosnemu(A,B):-
    azonosnemuferfi(A,B).

azonosnemu(A,B):-
    azonosnemuno(A,B).

hetfo(hetfo).
kedd(kedd).
szerda(szerda).
csutortok(csutortok).
pentek(pentek).

nemcsütörtök(hetfo).
nemcsütörtök(kedd).
nemcsütörtök(szerda).
nemcsütörtök(pentek).

haromnappalelobbvagykesobb(kedd,pentek).
haromnappalelobbvagykesobb(pentek,kedd).
haromnappalelobbvagykesobb(hetfo,csutortok).
haromnappalelobbvagykesobb(csutortok,hetfo).

egynappalelobb(A,B):-
    hetfo(A),kedd(B).

egynappalelobb(A,B):-
    kedd(A),szerda(B).

egynappalelobb(A,B):-
    szerda(A),csutortok(B).

egynappalelobb(A,B):-
    csutortok(A),pentek(B).


egymasutaninapok(hetfo,kedd,szerda).
egymasutaninapok(kedd,szerda,csutortok).
egymasutaninapok(szerda,csutortok,pentek).

megbizok(geza).
megbizok(greta).
megbizok(gergely).
megbizok(gusztav).
megbizok(gedeon).

kolcsonzok(furgon).
kolcsonzok(kamion).
kolcsonzok(mikrobusz).
kolcsonzok(szemelyauto).
kolcsonzok(teherauto).

mikrobuszvteherauto(teherauto).
mikrobuszvteherauto(mikrobusz).

napok(hetfo).
napok(kedd).
napok(szerda).
napok(csutortok).
napok(pentek).


egymasutanidays(X,Y,napok(X,Y,_,_,_)).
egymasutanidays(X,Y,napok(_,X,Y,_,_)).
egymasutanidays(X,Y,napok(_,_,X,Y,_)).
egymasutanidays(X,Y,napok(_,_,_,X,Y)).

harmasdays(X,Y,Z,napok(X,Y,Z,_,_)).
harmasdays(X,Y,Z,napok(_,X,Y,Z,_)).
harmasdays(X,Y,Z,napok(_,_,X,Y,Z)).
harmasdays(X,Y,Z,napok(_,_,_,X,Y)).



megold(P) :-
    autokolcsonzo(A, P), grafikus(A, gabor),
	autokolcsonzo(B, P), grafikus(B, gabriella),
	autokolcsonzo(C, P), grafikus(C, gerda),
	autokolcsonzo(D, P), grafikus(D, gaspar),
    autokolcsonzo(E, P), grafikus(E, gizella),
	P = megoldas(A, B, C, D, E),
    
    autokolcsonzo(F, P), megbizo(F, geza),
	autokolcsonzo(G, P), megbizo(G, greta),
	autokolcsonzo(H, P), megbizo(H, gergely),
	autokolcsonzo(I, P), megbizo(I, gusztav),
    autokolcsonzo(J, P), megbizo(J, gedeon),
    
    autokolcsonzo(K, P), kolcsonzo(K, teherauto),
    autokolcsonzo(L, P), kolcsonzo(L, szemelyauto),
	autokolcsonzo(M, P), kolcsonzo(M, furgon),
	autokolcsonzo(N, P), kolcsonzo(N, kamion),
    autokolcsonzo(O, P), kolcsonzo(O, mikrobusz),
    
    autokolcsonzo(Q, P), nap(Q, csutortok),
    autokolcsonzo(R, P), nap(R, szerda),
	autokolcsonzo(S, P), nap(S, kedd),
	autokolcsonzo(T, P), nap(T, pentek),
    autokolcsonzo(U, P), nap(U, hetfo),
    WW = napok(Q,R,S,T,U),


    %1 Gábor (nem ő kapta a Gedeontól a feladatot)a teherautó-kölcsönzéssel foglalkozó cég megbízását egy a
	%személyautó-kölcsönző megrendelése után, és egy nappal egy masik megrendelés előtt kapta, 
	%ami szintén nem gedeoné volt.

	A = K,
	A \= j,  
    nap(A,AN),
    nap(L,LN),
    autokolcsonzo(W,P), nap(W,WN),W \= j,
    egymasutaninapok(LN,AN,WN),

    
	%2 szabály Az a grafikus akit a furgon-kölcsönző bízott meg, vagy három nappal előbb, vagy három nappal később kapta	
	%a feladatot mint az egyik kollégája, aki nem a gézától kapta a megbízást/Hetfo-csutortok/csutortok-hetfo/kedd-pentek/pentek-kedd/
    %
    
 	nap(M,MN), 
	autokolcsonzo(EE,Megbizas), nemgeza(Emegbizo),megbizo(E,Emegbizo),nap(EE,EEN),haromnappalelobbvagykesobb(MN,EEN),
	

	%3	szabaly Gizella és a Gréta által megbízott művész: aki egy nappal a fugronosok előtt kapta a munkát, és a kamionos-lógót vagy
 	% a személyaitó-kölcsönző megbízását teljesítő tervező, valamilyen sorrendben /csutortok-hetfo/kedd-pentek/pentek-kedd/
   
    E \= M,  
    G \= O,
    G \= M,
    G \= K,
    autokolcsonzo(FF,Megbizas),nemteherauto(FFK),mikrobuszvteherauto(FFK),kolcsonzo(FF,FFK),egynappalelobb(FFN,DN),nap(FF,FFN),
    autokolcsonzo(GG,Megbizas),kamionosvszemelyautomegbizas(GGK),kolcsonzo(GG,GGK),
	
	%4 szabaly Gizella vagy három nappal korábban vagy három nappal később kapta a megbízást, mint az a grafikus aki ugyanolyan
	%nemu mint aki a kamionos megbízást kapta 
	
    E \= N, nap(E,EN),
	autokolcsonzo(II,Megbizas),nap(II,IIN),grafikus(II,IIG), haromnappalelobbvagykesobb(IIN,EN),
	grafikus(N,NG),kolcsonzo(N,kamion),azonosnemu(NG,IIG),

	%5 Az 5 grafikus közül három. Gizella ; akit a furgonkölcsönző álltal megbízott meg és aki csütörtökökön kapta meg
    % Gizella a grafikus ő nem furgonos és nem csütörtökön kapta meg 
    
 	 E \= Q, E \= M,
 	 M \= E, M \= Q,
	 Q \= E, Q \= M,


	%6 AZ 5 grafikus közül 3 egymás után kapta meg a megbízast.
	% A személyautó kölcsönző megbízás azaz gréta 
    % A teherautó kölcsönzést kapó	
    % És Gaspar 2 nappal  kesobb lett megbízva mint ahogy gréta megbízott valakit 
	L = G,
    nap(K,KN),
    nap(L,LN),
    nap(D,DN),
    egymasutaninapok(LN,KN,DN),

    

	%7 Gerda a gergely altal kepviselt furgon, vagy a kamion-kölcsönző megbízást kapta
	C = H, 
   	C \= K,
    C \= L,
    C \= O,
   	H \= K, 
    H \= L, 
    H \= O.




