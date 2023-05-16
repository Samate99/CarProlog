megold(P) :-
    autokolcsonzo(A, P), grafikus(A, gabor),
	autokolcsonzo(B, P), grafikus(B, gabriella),
	autokolcsonzo(C, P), grafikus(C, gerda),
	autokolcsonzo(D, P), grafikus(D, gaspar),
    autokolcsonzo(E, P), grafikus(E, gizella),
	P = megoldas(A, B, C, D, E),
    NW = nevek(A, B, C, D, E),
    
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
    
    autokolcsonzo(Q, P), nap(Q, hetfo),
    autokolcsonzo(R, P), nap(R, kedd),
	autokolcsonzo(S, P), nap(S, szerda),
	autokolcsonzo(T, P), nap(T, csutortok),
    autokolcsonzo(U, P), nap(U, pentek),
    WW = napok(Q,R,S,T,U),


    %1 Gábor (nem ő kapta a Gedeontól a feladatot)a teherautó-kölcsönzéssel foglalkozó cég megbízását egy a
	%személyautó-kölcsönző megrendelése után, és egy nappal egy masik megrendelés előtt kapta, 
	%ami szintén nem gedeoné volt.

	A = K,
	A \= J,  
    autokolcsonzo(W,P), W \= J,
   	egymasutanidays(L,K,WW),
    egymasutanidays(K,W,WW),

    
	%2 szabály Az a grafikus akit a furgon-kölcsönző bízott meg, vagy három nappal előbb, vagy három nappal később kapta	
	%a feladatot mint az egyik kollégája, aki nem a gézától kapta a megbízást/Hetfo-csutortok/csutortok-hetfo/kedd-pentek/pentek-kedd/
    %
    
	autokolcsonzo(EE,P), EE	\=  F,haromnapos(M,EE,WW),
	

	%3	szabaly Gizella és a Gréta által megbízott művész: aki egy nappal a fugronosok előtt kapta a munkát, és a kamionos-lógót vagy
 	% a személyaitó-kölcsönző megbízását teljesítő tervező, valamilyen sorrendben /csutortok-hetfo/kedd-pentek/pentek-kedd/
   
    E \= M,  
    G \= O,
    G \= M,
    G \= K,
    egymasutanidays(E,M,WW),
    
%	autokolcsonzo(FF,P),nemteherauto(FFK),mikrobuszvteherauto(FFK),kolcsonzo(FF,FFK),egynappalelobb(FFN,DN),nap(FF,FFN),
%    autokolcsonzo(GG,P),kamionosvszemelyautomegbizas(GGK),kolcsonzo(GG,GGK),
%    
	%4 szabaly Gizella vagy három nappal korábban vagy három nappal később kapta a megbízást, mint az a grafikus aki ugyanolyan
	%nemu mint aki a kamionos megbízást kapta 
	
    E \= N,
    
    autokolcsonzo(NE,P),neme(N,NE,NW),haromnapos(E,NE,WW),
    

	%5 Az 5 grafikus közül három. Gizella ; akit a furgonkölcsönző álltal megbízott meg és aki csütörtökökön kapta meg
    % Gizella a grafikus ő nem furgonos és nem csütörtökön kapta meg 
    
 	 E \= T, E \= M,
 	 M \= E, M \= T,
	 T \= E, T \= M,


	%6 AZ 5 grafikus közül 3 egymás után kapta meg a megbízast.
	% A személyautó kölcsönző megbízás azaz gréta 
    % A teherautó kölcsönzést kapó	
    % És Gaspar 2 nappal  kesobb lett megbízva mint ahogy gréta megbízott valakit 
	L = G,
	harmasdays(L,K,D,WW),

    
	%7 Gerda a gergely altal kepviselt furgon, vagy a kamion-kölcsönző megbízást kapta
	C = H, 
   C \= K,
  	C \= L,
   C \= O,
 	H \= K, 
   H \= L, 
   H \= O.


neme(X,Y,nevek(X,_,Y,_,_)).
neme(X,Y,nevek(X,_,_,Y,_)).
neme(X,Y,nevek(_,_,X,Y,_)).
neme(X,Y,nevek(_,X,_,_,Y)).
neme(X,Y,nevek(Y,_,X,_,_)).
neme(X,Y,nevek(Y,_,_,X,_)).
neme(X,Y,nevek(_,_,Y,X,_)).
neme(X,Y,nevek(_,Y,_,_,X)).


harmasdays(X,Y,Z,napok(X,Y,Z,_,_)).
harmasdays(X,Y,Z,napok(_,X,Y,Z,_)).
harmasdays(X,Y,Z,napok(_,_,X,Y,Z)).

haromnapos(X,Y,napok(X,_,_,Y,_)).
haromnapos(X,Y,napok(_,Y,_,_,X)).
haromnapos(X,Y,napok(Y,_,_,X,_)).
haromnapos(X,Y,napok(_,X,_,_,Y)).


egymasutanidays(X,Y,napok(X,Y,_,_,_)).
egymasutanidays(X,Y,napok(_,X,Y,_,_)).
egymasutanidays(X,Y,napok(_,_,X,Y,_)).
egymasutanidays(X,Y,napok(_,_,_,X,Y)).


autokolcsonzo(X,megoldas(X,_,_,_,_)).
autokolcsonzo(X,megoldas(_,X,_,_,_)).
autokolcsonzo(X,megoldas(_,_,X,_,_)).
autokolcsonzo(X,megoldas(_,_,_,X,_)).
autokolcsonzo(X,megoldas(_,_,_,_,X)).

grafikus(h(X,_,_,_),X).
megbizo(h(_,X,_,_),X).
kolcsonzo(h(_,_,X,_),X).
nap(h(_,_,_,X),X).





