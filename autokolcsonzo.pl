% autokolcsonzo prolog.

grafikus(kiadottMegbizas(X,_,_,_),X).
megbizo(kiadottMegbizas(_,X,_,_),X).
kolcsonzo(kiadottMegbizas(_,_,X,_),X).
nap(kiadottMegbizas(_,_,_,X),X).

autokolcsonzo(X,lista(X,_,_,_,_)).
autokolcsonzo(X,lista(_,X,_,_,_)).
autokolcsonzo(X,lista(_,_,X,_,_)).
autokolcsonzo(X,lista(_,_,_,X,_)).
autokolcsonzo(X,lista(_,_,_,_,X)).

nemfurgon(kamion).
nemfurgon(szemelyauto).
nemfurgon(teherauto).
nemfurgon(mikrobusz).

nemgedeon(gergely).
nemgedeon(geza).
nemgedeon(greta).
nemgedeon(gusztav).

nemgizella(gabor).
nemgizella(gaspar).
nemgizella(gabriella).
nemgizella(gerda).

nemgeza(gedeon).
nemgeza(gréta).
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

solve(Megbizas):-
    Megbizas=lista(
              kiadottMegbizas(gabor,MEGBIZO1,_,NAP1),
              kiadottMegbizas(gabriella,MEGBIZO2,_,NAP2),
              kiadottMegbizas(gerda,MEGBIZO3,_,NAP3),
              kiadottMegbizas(gaspar,MEGBIZO4,_,NAP4),
              kiadottMegbizas(gizella,MEGBIZO5,_,NAP5)
              ),
   	autokolcsonzo(MA,Megbizas),grafikus(MA,gabor),
    autokolcsonzo(MB,Megbizas),grafikus(MB,gabriella),
    autokolcsonzo(MC,Megbizas),grafikus(MC,gerda),
    autokolcsonzo(MD,Megbizas),grafikus(MD,gaspar),
    autokolcsonzo(ME,Megbizas),grafikus(ME,gizella),
    /*
    kolcsonzok(KOLCSONZO1),
    kolcsonzok(KOLCSONZO2),
    kolcsonzok(KOLCSONZO3),
    kolcsonzok(KOLCSONZO4),
    kolcsonzok(KOLCSONZO5),
    KOLCSONZO1 \= KOLCSONZO2,
    KOLCSONZO1 \= KOLCSONZO3,
    KOLCSONZO1 \= KOLCSONZO4,
    KOLCSONZO1 \= KOLCSONZO5,
    KOLCSONZO2 \= KOLCSONZO3,
    KOLCSONZO2 \= KOLCSONZO4,
    KOLCSONZO2 \= KOLCSONZO5,
    KOLCSONZO3 \= KOLCSONZO4,
    KOLCSONZO3 \= KOLCSONZO5,
    KOLCSONZO4 \= KOLCSONZO5,
    */
    megbizok(MEGBIZO1),
    megbizok(MEGBIZO2),
    megbizok(MEGBIZO3),
    megbizok(MEGBIZO4),
    megbizok(MEGBIZO5),  
    MEGBIZO1 \= MEGBIZO2,
    MEGBIZO1 \= MEGBIZO3,
    MEGBIZO1 \= MEGBIZO4,
    MEGBIZO1 \= MEGBIZO5,
    MEGBIZO2 \= MEGBIZO3,
    MEGBIZO2 \= MEGBIZO4,
    MEGBIZO2 \= MEGBIZO5,
    MEGBIZO3 \= MEGBIZO4,
    MEGBIZO3 \= MEGBIZO5,
    MEGBIZO4 \= MEGBIZO5,  
    napok(NAP1),
    napok(NAP2),
    napok(NAP3),
    napok(NAP4),
    napok(NAP5),
    NAP1 \= NAP2,
    NAP1 \= NAP3,   
    NAP1 \= NAP4,  
    NAP1 \= NAP5,   
    NAP2 \= NAP3,   
    NAP2 \= NAP4,   
    NAP2 \= NAP5, 
    NAP3 \= NAP4,
    NAP3 \= NAP5,
    NAP4 \= NAP5,
    			
    %1 Gábor (nem ő kapta a Gedeontól a feladatot)a teherautó-kölcsönzéssel foglalkozó cég megbízását egy a
	%személyautó-kölcsönző megrendelése után, és egy nappal egy masik megrendelés előtt kapta, 
	%ami szintén nem gedeoné volt.
    % Gáboré a teherautó kölcsönzés/A teherautó kölcsönző megbízó nem Gedeon/Kedd-Szerda-Csütörtök/
    % ismeretlek a A személyautó kölcsönző grafikus/ ismeretlen a megbízó/ Hétfő-kedd-szerda/
    % Ismeretlen a grafikus és a munka is/ Nem gedeon a megbízó / Szerda-csütörtök-péntek
    
	autokolcsonzo(A,Megbizas), grafikus(A,gabor),nemgedeon(Amegbizo),kolcsonzo(A,teherauto),megbizo(A,Amegbizo),nap(A,AN),
    autokolcsonzo(B,Megbizas), kolcsonzo(B,szemelyauto), nap(B,BN),
    autokolcsonzo(C,Megbizas), nap(C,CN),nemgedeon(Cmegbizo), megbizo(C,Cmegbizo),
    egymasutaninapok(BN,AN,CN),
    
    
	%2 szabály Az a grafikus akit a furgon-kölcsönző bízott meg, vagy három nappal előbb, vagy három nappal később kapta	
	%a feladatot mint az egyik kollégája, aki nem a gézától kapta a megbízást/Hetfo-csutortok/csutortok-hetfo/kedd-pentek/pentek-kedd/
    %
    
	autokolcsonzo(D, Megbizas), kolcsonzo(D,furgon), nap(D,DN), 
	autokolcsonzo(E,Megbizas), nemgeza(Emegbizo),megbizo(E,Emegbizo),nap(E,EN),haromnappalelobbvagykesobb(DN,EN),
    
    
	%3	szabaly Gizella és a Gréta által megbízott művész: aki egy nappal a fugronosok előtt kapta a munkát, és a kamionos-lógót vagy
 	% a személyaitó-kölcsönző megbízását teljesítő tervező, valamilyen sorrendben /csutortok-hetfo/kedd-pentek/pentek-kedd/
    % Gizella 

    
    autokolcsonzo(F,Megbizas),grafikus(F,gizella), 
    autokolcsonzo(G,Megbizas),megbizo(G,greta),
    autokolcsonzo(FF,Megbizas),mikrobuszvteherauto(FFK),kolcsonzo(FF,FFK),egynappalelobb(FFN,DN),nap(FF,FFN),
    autokolcsonzo(GG,Megbizas),kamionosvszemelyautomegbizas(GGK),kolcsonzo(GG,GGK),nemcsütörtök(GGN),nap(GG,GGN),GGN \= FFN, GGN \= DN, 
    
    %4 szabaly Gizella vagy három nappal korábban vagy három nappal később kapta a megbízást, mint az a grafikus aki ugyanolyan
	%nemu mint aki a kamionos megbízást kapta 
    
	autokolcsonzo(H,Megbizas),grafikus(H,gizella),nap(H,HN),
	autokolcsonzo(I,Megbizas),nap(I,IN),grafikus(I,IG), haromnappalelobbvagykesobb(IN,HN),
	autokolcsonzo(J,Megbizas),grafikus(J,JG),kolcsonzo(J,kamion),azonosnemu(JG,IG),
	
    
    %5 Az 5 grafikus közül három. Gizella ; akit a furgonkölcsönző álltal megbízott meg és aki csütörtökökön kapta meg
    % Gizella a grafikus ő nem furgonos és nem csütörtökön kapta meg 
    % A furgonos nem Gizella es nem csütörtökön kapta meg /Hetfon-kedden-penteken lehet)
    % Aki csütörtökön kapta meg az nem a furgonos sőt nem is gizi
    
	autokolcsonzo(K,Megbizas),grafikus(K,gizella),nemcsütörtök(KN),nap(K,KN), nemfurgon(KK), kolcsonzo(K,KK),
	autokolcsonzo(L,Megbizas),kolcsonzo(L,furgon),nemcsütörtök(LN),nap(L,LN),nemgizella(LG),grafikus(L,LG),
	autokolcsonzo(M,Megbizas),nap(M,csutortok),nemgizella(MG),grafikus(M,MG),nemfurgon(MK),kolcsonzo(M,MK),
	
    
    %6 AZ 5 grafikus közül 3 egymás után kapta meg a megbízast.
	% A személyautó kölcsönző megbízás azaz gréta 
    % A teherautó kölcsönzést kapó	
    % És Gaspar 2 nappal  kesobb lett megbízva mint ahogy gréta megbízott valakit 
	
    autokolcsonzo(N,Megbizas),kolcsonzo(N,szemelyauto),nap(N,NN),megbizo(N,greta),	
	autokolcsonzo(O,Megbizas),kolcsonzo(O,teherauto),nap(O,ON),
	autokolcsonzo(P,Megbizas),nap(P,PN),grafikus(P,gaspar),egymasutaninapok(NN,ON,PN),
	
    
    %7 Gerda a gergely altal kepviselt furgon, vagy a kamion-kölcsönző megbízást kapta
    % Gerda vagy megkapta a kamionos vagy a furgonos melo
    % A furgonos megbízó Gergely
	
    autokolcsonzo(R,Megbizas),grafikus(R,gerda),furgonvkamion(Rkolcs) ,kolcsonzo(R,Rkolcs),megbizo(R,gergely).





