-- lab - coresp curry howard
-- scop - implementarea sistemului de deductie naturala a logicii propozitionale in haskell
-- caracterizam un sistem logic prin:
-- 1. sintaxa - demonstratia, teorema
-- 2. semantica - adevar, validitate, satisfiabilitate
-- in cazul logicii propozitionale, cele 2 notiuni sunt echivalente. daca o formula este adevarata, atunci este si demonstrabila - rezultatul este dat de teorema de completitudine 

-- Notatii
-- sintaxa: fi este o gama teorema (cu ip de deductie gama) |- fi
-- semantica: |= fi este o tautologie gama |= fi (fi este o gama tautologie)

-- semantica: tabele de adevar, este costisitoare dpdv computational
-- sintaxa: sistemul de deductie hilbert

-- (A1) fi -> (psi -> fi)
-- (A2) (fi -> (psi -> chi)) -> ((fi -> psi) -> (fi -> chi))
-- (A3) (¬psi -> ¬fi) -> (fi -> psi)

-- regula de deductie in logica propoz - modus ponens
--        fi, fi -> psi 
--  (MP) ----------------
--         psi
-- in continuare, in loc sa lucram in sist Hilbert, vom lucra in sist natural de deductie
-- este un sist deductiv, construit doar din reguli de deductie pentru toti conectorii logici (negatia, conjunctia si echivalenta)
-- este un sist echivalent cu Hilbert, dar mai usor de folosit


-- Sistemul de deductie naturala pentru logica propozitionala
-- este un sist expresi, pt care avem reguli de deductie pt toti conectorii logici
-- reg de deductie = reg de introducere + reg de eliminare

-- Conjunctia
-- Reg de introducere pentru conjunctie ne spune ca daca avem 2 teoreme fi si psi, atunci avem si fi ^ psi teorema
-- reg de elim sunt proiectiile pt fiecare dintre cele 2 conjunctii
-- daca avem fi ^ psi, atunci avem si fi si avem si psi

-- Implicatia:
-- reg de introducere: daca avem o teorema psi, atunci avem si fi -> psi teorema

-- reg de eliminare(modus ponens): daca avem fi -> psi si avem fi, atunci avem si psi

-- Disjunctia:
-- reg de introducere: un adevar in disj cu orice este adevar
-- in cazul reg de elim, daca stim ca fi v psi este dem, nu stim care dintre ele este adevarat, dar stim ca cel putin unul este adevarat
--elim se face pe cazuri
-- daca avem fi v psi si avem fi, atunci avem si chi
-- daca avem fi v psi si avem psi, atunci avem si chi
-- in acest caz, indif de care dintre cele 2 este adevarat, avem chi

-- Negatia:
-- notam falsul cu _|_, respectiv adevarul cu T
-- reg de introducere: daca avem _|_, atunci avem si fi(0 implica orice)
-- natural, se poate construi principiul RAA (red la absurd)

-- Echivalenta:
-- Regulile se deduc automat, din faptul ca echivalenta se construieste din implicatii si o conjunctie
-- fi <-> psi = (fi -> psi) ^ (psi -> fi)
-- reg elim: daca avem fi <-> psi, atunci avem si fi -> psi (elim stanga) si avem si psi -> fi (elim dreapta)

-- exemple de dem in deductie naturala
-- dem ca urm secventi sunt valizi
-- 1. p ^ q , r ^ s |- p ^ s
-- trebuie sa dem p ^ s, pornind de la ip de deductie p ^ q si r ^ s
-- echival se putea scrie |- (p ^ q) ^ (r ^ s) -> p ^ s
-- in logica propoz exista teorema deductiei care spune ca |- (fi ^ psi) -> fi

-- in impl folosim forma cu implicatia
exercise :: And p q -> And r s -> And p s
exercise = undefined
-- dem este una pe linii, ca in sist hilbert. pe primele linii, se scriu ip de deductie, iar pe ultima linie se scrie concluzia
-- (1) p ^ q ipoteza
-- (2) r ^ s ipoteza
-- (3) p (^elim)(1)
-- (4) s (^elim)(2)
-- (5) p ^ s (^intro)(3)(4)
-- am dem ca secventul este valid

-- 2. p ^ (q v s) |- (p ^ q) v (p ^ s)
-- demonstratie:
-- (1) p ^ (q v s) ipoteza
-- (2) p (^elim)(1)
-- (3) q v s (^elim)(1)
-- (4) q asumptie = ipoteza temporara
-- (5) p ^ q (^intro)(2)(4)
-- (6) (p ^ q) v (p ^ s) (v intro)(5) 
-- (7) s asumptie 
-- (8) p ^ s (^intro)(2)(7)
-- (9) (p ^ q) v (p ^ s) (v intro)(8)
-- (10) (p ^ q) v (p ^ s) (v elim)(3)(4-6)(7-9)
-- am demonstrat ca secventul este valid

-- impl coresp Curry Howard in haskell
-- avem urm echivalente:
-- dem vor fi chiar programele
-- implicatia e chiar fct de tipul (->) 
-- pentru restul tipurilor:
-- False = Void
-- True = unit
-- And (^) = produs
-- Or (v) = suma
-- Not (¬) = def prim implicatie si False (¬fi = fi -> False)
-- Iff (<->) = and si implicatie

data False --empty type, void
data True = True -- unit type

data And a b = And {proj1 :: a, proj :: b} 

data Or a b = Left a 
     | Right b

type Not a = a -> False
type Iff a b = And (a -> b) (b -> a)

-- specif reg de elim si introducere pt aceste tipuri

trueIntro :: True
trueIntro = True

falseElim :: False -> b
falseElim x = case x of 

implElim :: (a -> b) -> a -> b
implElim f  = f

implIntro :: (a -> b) -> a -> b
implIntro f = f

andIntro :: a -> b -> And a b
andIntro  = And

andElimLeft :: And a b -> a
andElimLeft = proj1

andElimRight :: And a b -> b
andElimRight = proj2

orIntroLeft :: a -> Or a b
orIntroLeft = Left

orIntroRight :: b -> Or a b
orIntroRight = Right

orElim :: (a -> c) -> (b -> c) -> Or a b -> c
orElim fac fbc or = case or of
    Left a -> fac c
    Right b -> fbc c

notIntro :: (forall p. a -> p) -> Not a
notIntro f = f

notElim :: p -> Not p -> c
notElim p np = falseElim(np p)

iffIntro :: (a -> b) -> (b -> a) -> Iff a b
iffIntro fab fba = andIntro fab fba

iffElimLeft :: a -> Iff a b -> b
iffElimLeft a iff = andElimRight iff a

iffElimRight :: b -> Iff a b -> a
iffElimRight b iff = (andElimLeft iff) b

deMorgan1 :: And (Not a) (Not b) -> Not (Or a b)
deMorgan1 = undefined

deMorgan2 :: Not (Or a b) -> And (Not a) (Not b)
deMorgan2 = undefined

deMorgan3 :: Or (Not a) (Not b) -> Not (And a b)
deMorgan3 = undefined

-- in continuare, def un sist de axiome pt un fragment de logica instit a logicii propozitionale.
-- Logica institutionista este logica in care nu tin princ tertului exclus, adica nu avem o valoare de adevar pentru orice formula
-- avem 10 axiome si vrem sa implem dem pt ele in sist de deductie naturala

ax1 :: a -> b -> a
ax1 = implIntro (\a -> implIntro (\b -> a))

ax2 :: (a ->b) -> (a -> (b -> c)) -> (a -> c)
ax2 = implIntro (\f ->      -- f = a -> b
       implIntro (\g ->      -- g = a -> (b -> c)
       implIntro (\a ->      -- a = a
        implElim (implElim g a) (implElim f a))))
 
-- lucram la ax2 |- (a -> b) -> (a -> (b -> c)) -> (a -> c)
-- dem:
-- (1) a -> b ipoteza
-- (2) a -> (b -> c) ipoteza
-- (3) a ipoteza
-- (4) b (^elim)(1)(3)
-- (5) b -> c (^elim)(2)(3)
-- (6) c (^elim)(4)(5)

ax3 :: (a -> b) -> And a b 
ax3 = implIntro (\a -> implIntro (\b -> andIntro a b))

ax4 :: And a b -> a
ax4 = implIntro (\ab -> andElimLeft ab)

-- tema: de terminat ax5-10 din lab Lab07
-- de dem ca daca in logica tine princ tertului exclus, atunci este dem elim dublei neg
-- (p  v ¬p) |- ¬¬p -> p
-- de dem reg lui Morgan (1,2,3)
-- trimitem tema pe mail bogdan.macovei.fmi@gmail.com
-- daca mai avem teme trecute, de trimis pana la sf saptamanii viitoare
