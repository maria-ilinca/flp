-- REPL 
-- RPL : read, print, loop
-- Read: citim sirul de caractere pe care vrem sa il interpretam ca o comanda
-- Print: reprezentam sirul de caractere intr-o structura interna (Parsare) si afisam rezultatul la STDOUT
-- Loop: dupa ce am afisat rez, afisam iar linia de comanda
-- miniHaskell> :q
-- sau 
-- miniHaskell> :l file.hs
-- Not implemented

-- miniHaskell>2
-- 2 

-- miniHaskell> let x:=1 in x
-- let x:=1 in x

-- Pentru a impl acest interpretor avem nevoie de urm fisiere:
-- Exp.hs : def tipurile de date 
--     var - pt variabile
--     newtype Var = Var {getVar :: String}
--     si ComplexExp care defineste care sunt expresiie pe care le putem reprezenta
--     - variabile
--     - numere naturale
--     - lambda-functii
--     - aplicari de functii
--     - let
--     - letrec
--     - liste

-- Lab2.hs : rez din lab 2
-- Main.hs : trebuie sa permita urm:
-- - afiseaza linia de comanda miniHaskell>
-- - citeste comanda
-- - parseaza comanda
-- - daca era quit -> iese din linia de comanda
-- - daca era load -> afiseaza ca nu e implementata
-- - altfel -> incearca sa parseze expr citita
-- - daca nu s a putut parsa -> eroare de parsare
-- - altfel => reprez interna a expresiei

-- Parsing.hs - def cum parsam expr citite, utilizand combinatorii de parsare din Lab2.hs
-- Printing.hs - def cum arata afisarea expr
-- REPLCommand.hs - def cum parsam comenzile

module REPLCommand where
    