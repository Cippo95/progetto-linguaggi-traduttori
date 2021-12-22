--Progetto Linguaggi E Traduttori - Filippo Landi
--Parser di un sottolinguaggio del linguaggio C
--Riconosco operazioni con int niente array

--importo questa libreria per semplificarmi la vita nel riconoscimento di int
import Data.Char

--IMPLEMENTAZIONE PDA PER BILANCIAMENTO PARENTESI, REGOLA S
--comando per iniziare il parsing
parseString = codice ""
--se trovo graffa aperta mi aspetto graffa chiusa, la andro' ad inserire nello stack poi
expClose c = '}'
--fine stringa, stack nullo quindi True (bilancio corretto)
codice s "" = (null s,"")
--algoritmo generale, trovo parentesi o non parentesi (saranno dichiar. o assegn.)
codice s (c:r)
    | c == ' ' = codice s r
    | c == '{'                     = codice (expClose c:s) r
    | elem c lettera               = codice' s (c:r)
    | c == '}' && (not.null) s     = codice'' s (c:r)
    | c == ';'                     = codice s r
--nel caso non graffe allora faccio parsing dichiarazioni e assegnamenti
--uso ident per riconoscimento int, in caso non si voglia bisogna sostituire 'ident' con 'istruzione'
codice' s x
    | fst(ident x)  = codice s (snd(ident x))
--nel caso chiudo una parentesi ma ne ho altre da chiudere continuo 
codice'' s (c:r)
    | c == head s = codice (tail s) r
    
--RESTO GRAMMATICA, RICONOSCIMENTO INT E RESTANTI REGOLE
--codice per riconoscere int ma come detto in codice' si puo' togliere
--solo qui uso funzioni da Data.Char per semplificarmi la vita
break' x = break isSpace x
--elimino spazi iniziali
ident x = ident' (dropWhile isSpace x)
--controllo se ho int all'inizio, non la tengo però come parola riservata
ident' (x:xs)
    --se primo termine indica int continuo su secondo termine
    | fst (break' (x:xs)) == "int" = istruzione $ (dropWhile isSpace (snd (break' (x:xs))))
    | elem x lettera = istruzione (x:xs)
    
--REGOLE DELLA GRAMMATICA
--regola <istruzione>
istruzione (x:xs)
    | elem x lettera = link fineistruzioneDichiarazioneAssegnamento (identificatore(x:xs))
--regola <fineistruzione-dichiarazione-assegnamento>
fineistruzioneDichiarazioneAssegnamento (x:xs)
    | x == ';' = (True, xs)
    | x == ',' = istruzione xs
    | x == '=' = espressioneMultiassegnamento xs
    | x == ' ' = fineistruzioneDichiarazioneAssegnamento xs
--regole <lettera> e <cifra>
lettera = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
cifra = "1234567890"
--insieme delle operazioni
op = "+*-/"
--regole <identificatore> e <lettera-cifra>
identificatore (x:[]) = (elem x lettera, "")
identificatore (x:xs)
    | elem x lettera && not(elem (head xs) lettera || elem (head xs) cifra) = (True, xs)
    | elem x lettera = letteraCifra xs
letteraCifra (x:[]) = ((elem x lettera || elem x cifra), "")  
letteraCifra (x:xs)
    | (elem x lettera || elem x cifra) && not(elem (head xs) lettera || elem (head xs) cifra)  = (True, xs)
    | (elem x lettera || elem x cifra) = letteraCifra xs
--regola <numero>
numero (x:[]) = (elem x cifra, "")
numero (x:xs)
    | elem x cifra && not(elem (head xs) cifra) = (True, xs)
    | elem x cifra = numero xs
--funzione link mi serve per legare le produzioni
link y i
    | fst i = y(snd i)
    | otherwise =  i
--regola <espressione-multiassegnamento>
espressioneMultiassegnamento (x:xs)
    | elem x "+-" = espressione xs
    | elem x cifra = link postespressione (numero(x:xs))
    | elem x lettera = link assegnamentoChiamatafunzionePostespressione (identificatore(x:xs))
    | x == '(' = link postespressione (espressionepAssegnamentop(xs))
    | x == ' ' = espressioneMultiassegnamento xs
--regola <espressione>
espressione (x:xs)
    | elem x lettera = link chiamatafunzionePostespressione (identificatore(x:xs))
    | elem x cifra = link postespressione (numero(x:xs))
    | x == '(' = link postespressione (espressionepAssegnamentop(xs))
    | x == ' ' = espressione xs
--regola <parametrifunzione>
parametrifunzione (x:xs)
    | x == ')' = (True, xs)
    | x == ' ' = parametrifunzione xs
    | otherwise = espressionepAssegnamentop (x:xs)
--regola <espressionep-assegnamentop>
espressionepAssegnamentop (x:xs)
    | elem x "+-" = espressionep xs
    | elem x cifra = link postespressionep (numero(x:xs))
    | elem x lettera = link assegnamentopChiamatafunzionepPostespressionep (identificatore(x:xs))
    | x == '(' = link postespressionep (espressionepAssegnamentop(xs))
    | x == ' ' = espressionepAssegnamentop xs
--regola <espressionep>   
espressionep (x:xs)
    | elem x lettera = link chiamatafunzionepPostespressionep (identificatore(x:xs))
    | elem x cifra = link postespressionep (numero(x:xs))
    | x == '(' = link postespressionep (espressionepAssegnamentop(xs))
    | x == ' ' = espressionep xs
--regola <assegnamento-chiamatafunzione-postespressione> 
assegnamentoChiamatafunzionePostespressione (x:xs)
    | x =='=' = espressioneMultiassegnamento(xs)
    | x == ' ' = assegnamentoChiamatafunzionePostespressione xs
    | otherwise =  chiamatafunzionePostespressione(x:xs)
--regola <assegnamentop-chiamatafunzionep-postespressionep>
assegnamentopChiamatafunzionepPostespressionep (x:xs)
    | x =='=' = espressionepAssegnamentop(xs)
    | x == ' ' = assegnamentopChiamatafunzionepPostespressionep xs
    | otherwise =  chiamatafunzionepPostespressionep(x:xs)
--regola <postespressione>
postespressione (x:xs)
    | elem x op = segno xs
    | x==';' = (True, xs)
    | x==',' = istruzione(xs)
    | x == ' ' = postespressione xs
-- <postespressionep>
postespressionep (x:xs)
    | elem x op = segnop xs
    | x==')' = (True, xs)
    | x==',' = espressionepAssegnamentop(xs)
    | x == ' ' = postespressionep xs
--regola <chiamatafunzione-postespressione>
chiamatafunzionePostespressione (x:xs)
    | x =='(' = link postespressione (parametrifunzione(xs))
    | x == ' ' = chiamatafunzionePostespressione xs
    | otherwise = postespressione(x:xs)
--regola <chiamatafunzionep-postespressionep>
chiamatafunzionepPostespressionep (x:xs)
    | x =='(' = link postespressionep (parametrifunzione(xs))
    | x == ' ' = chiamatafunzionepPostespressionep xs
    | otherwise = postespressionep(x:xs)
--regola <segno>
segno (x:xs)
    | elem x "+-" = espressione xs
    | x == ' ' = segno xs
    | otherwise = espressione (x:xs)
--regola <segno>
segnop (x:xs)
    | elem x "+-" = espressionep xs
    | x == ' ' = segnop xs
    | otherwise = espressionep (x:xs)


