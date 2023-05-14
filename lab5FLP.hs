import qualified Data.Bool as Bool
import qualified GHC.Natural as Natural
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either 
import qualified Data.List as List


-- codari Church
-- vom reprezenta tipurile bool, nat, maybe, either, list si pair in haskell folosind codare Church ca aplicatie a expresivitatii lambda-calculului

-- bool
-- true λxy.x
-- false λxy.y

-- T tf -> beta t 
-- F tf -> beta f

-- if b tf =  λbtf.btf

-- Avand definit constructoriide baza ai tipului si conditionalul if, cum putem def op logice?

-- and λb1b1.if b1b2 F
-- or  λb1b1.if b1 T b2
-- not λb.if b F T

-- impl in haskell

class BoolClass a where 
 
  false :: a
  true :: a
  bool :: Bool -> a -> a -> a
    -- in impl gandim: F = λft.f, T = λft.t
    -- iar fctia bool este o fct care primeste: act pt false, act pentru true si un bool de evaluat si returneaza act-ul corespunzator
    -- putem instantia clasa de sus pe tipul primitiv Bool importat din Data.Bool 
    -- obs: in toate impl pe care le vom face , nu vom folosi tipuri primitive, ci doar tipuri definite de noi
    -- astfel, tipul de baza va fi importat ca Bool.Bool, pt ca nu se afla deja in environment

instance BoolClass Bool.Bool where
  false = Bool.False
  true = Bool.True
  bool = Bool.Bool

-- vrem sa definim o functie if-then-else, care sa primeasca argum in ordinea lor naturala: booleanul de evaluat, cond pt true(then), cond pt false(else)
ite :: BoolClass b => b -> a -> a -> a
ite b t e = bool e t b

-- avand def ac functii, vom defini:
(&&) :: BoolClass b => b -> b -> b
(&&) b1 b2 = ite b1 b2 false

(||) :: BoolClass b => b -> b -> b
(||) b1 b2 = ite b1 true b2

not :: BoolClass b => b -> b
not b = ite b false true

-- tipul nat 
-- pentru a reprezenta mult nr nat in lambda-calcul, vom folosi codarea Church
-- pentru fiecare n natural, vom avea o functie λfx.f^n x 

-- 0 λfx.x
-- 1 λfx.fx
-- 2 λfx.f(fx)

-- putem reprezenta functia succesor utilizand combinatorul din lambda-calcul Succ
-- Succ = λnfx.f(nfx)
-- Succ 3 = λfx.f(nfx) 3  =>beta red  => λfx.f(3fx) =>beta red => λfx.f(f(f(fx))) => 4

-- pentru a repr mult nr nat in lambda calcul avem nevoie de urm defintii: 
-- elem initial al mult = zero
-- functie de succesor => constructia iterativa a tipului succ
-- functie care permite aplicarea unei functii generice de un numar finit de ori, iter

-- propr iteratiei sunt:
-- iter f i zero = i
-- iter f i (succ n) = f(iter f i n)

-- Definim NatClass in haskell

class NatClass n where
    zero :: n
    succ :: n -> n
    iter :: (a -> a) -> a -> n -> a
-- instantiem clasa pentru tipul Natural, pe care il vom accesa din Natural.Natural si nu avem acces la op elementare, pentru + vom folosi Natural.plusNatural, in loc de - vom folosi Natural.minusNatural

instance NatClass Natural.Natural where
    zero = 0
    succ = Natural.plusNatural n 1
    iter f i 0 = i
    iter f i n = f(iter f i (Natural.minusNatural n 1))

-- toate numerele naturale se construiesc inductiv aplicand succesorul pe zero

one :: NatClass n => n
one = Main.succ zero

-- obs: tipul bool definit anterior, va fi exportat ca CBool (def lab din BoolClass.hs)
 
newtype Cbool = Cbool {getCBool :: forall a. a -> a -> a}

-- vrem sa definim fctii pentru a lucra cu multimea numerelor naturale in haskell
isZero :: NatClass n => n -> Cbool
isZero = iter (const false) true nat

add :: NatClass n => n -> n -> n
add a b = iter Main.succ x y 

-- definiti functia de inmultire si exponentul 

mul :: NatClass n => n -> n -> n
mul = iter (add x) zero y

exp :: NatClass n => n -> n -> n
exp = iter (mul x) one y

-- tipul maybe 
-- implementare in haskell

class MaybeClass m where
    nothing :: m a
    just :: a -> m a
    maybe :: b -> (a -> b) -> m a -> b

-- comportamentul fctiei maybe este:
-- primeste o val def
-- primeste o fctie din a in b
-- primeste un maybe a
-- daca maybe a este nothing, atunci se returneaza val default, altfel se aplica fctia asupra lui a

instance MaybeClass Maybe.Maybe where
    nothing = Maybe.Nothing
    just = Maybe.Just
    maybe = Maybe.maybe

-- vrem sa implementam functia fromMaybe care sa primeasca o valoare default si o val MaybeClass 
-- daca valoarea MaybeClass este nothing, atunci se returneaza val default, altfel se returneaza val din interiorul lui Just


fromMaybe :: (MaybeClass m) => a -> m a -> a
fromMaybe a ma = Main.maybe a id ma

isNothing :: (MaybeClass m, BoolClass b) => m a -> b
isNothing ma = Main.maybe true (const false) ma

isJust :: (MaybeClass m, BoolClass b) => m a -> b
isJust ma = Main.not (isNothing ma)

-- tema 
-- de finalizat implementarea pentru mult numerelor naturale - predecesor, scaderi, op de comparatii (lt, gt, lte, gte)
-- de impl codarile Church pt tipurile either, list si pair

-- subiect: Lab05, Grupa, Nume

class PairClass p where
  pair :: a -> b -> p a b
  first :: p a b -> a
  second :: p a b -> b
 
instance PairClass (,) where
  pair = (,)
  first = fst
  second = snd
 
-- Vrem sa definim
swap :: PairClass p => p a b -> p b a
swap p = pair (second p) (first p)
 
-- Vrem sa definim
curry :: PairClass p => (p a b -> c) -> a -> b -> c
curry f a b = f (pair a b)
 
-- Vrem sa definim
uncurry :: PairClass p => (a -> b -> c) -> p a b -> c
uncurry f p = f (first p) (second p)
 
-- Vrem sa definim
fst3 :: PairClass p => p a (p b c) -> a
fst3 p = first p
 
-- Vrem sa definim
snd3 :: PairClass p => p a (p b c) -> b
snd3 p = first (second p)
 
-- Vrem sa defini
thd3 :: PairClass p => p a (p b c) -> c
thd3 p = second (second p)
 
-- Vrem sa definim
mapPair :: PairClass p => (a -> b) -> p a a -> p b b
mapPair f p = pair (f (first p)) (f (second p))
 
-- Vrem sa definim
zipPair :: PairClass p => p a b -> p c d -> p (p a c) (p b d)
zipPair p1 p2 = pair (pair (first p1) (first p2)) (pair (second p1) (second p2))
 
-- Vrem sa definim
unzipPair :: PairClass p => p (p a b) (p c d) -> p (p a c) (p b d)
unzipPair p = pair (pair (first (first p)) (first (second p))) (pair (second (first p)) (second (second p)))
 
class EitherClass e where
  left :: a -> e a b
  right :: b -> e a b
  either :: (a -> c) -> (b -> c) -> e a b -> c
 
instance EitherClass Either.Either where
  left = Either.Left
  right = Either.Right
  either = Either.either
 
-- Vrem sa definim
isLeft :: (EitherClass e, BoolClass b) => e a b -> b
isLeft e = Main.either (const true) (const false) e
 
-- Vrem sa definim
isRight :: (EitherClass e, BoolClass b) => e a b -> b
isRight e = Main.not (isLeft e)
 
 
class ListClass l where
  nil :: l a
  cons :: a -> l a -> l a
  head :: l a -> a
  tail :: l a -> l a
  null :: l a -> Cbool
  null = iter (const false) true
 
instance ListClass [] where
  nil = []
  cons = (:)
  head = Prelude.head
  tail = Prelude.tail
 
-- Vrem sa definim
mapList :: ListClass l => (a -> b) -> l a -> l b
mapList f l = iter (\x -> cons (f x)) nil l
 
-- Vrem sa definim
filterList :: ListClass l => (a -> Cbool) -> l a -> l a
filterList f l = iter (\x -> if f x then cons x else id) nil l
 
-- Vrem sa definim
foldrList :: ListClass l => (a -> b -> b) -> b -> l a -> b
foldrList f b l = iter (\x -> f x) b l
 
-- Vrem sa definim
foldlList :: ListClass l => (b -> a -> b) -> b -> l a -> b
foldlList f b l = iter (\x -> f x) b (reverseList l)
 
-- Vrem sa definim
zipList :: ListClass l => l a -> l b -> l (p a b)
zipList l1 l2 = iter (\x -> cons (pair x)) nil (zip l1 l2)
 
-- Vrem sa definim
unzipList :: ListClass l => l (p a b) -> p (l a) (l b)
unzipList l = pair (mapList (first) l) (mapList (second) l)
 
-- Vrem sa definim
get :: ListClass l => l a -> Natural.Natural -> Maybe a
get l n = iter (\x -> if equal n zero then just x else id) nothing l
 
-- Vrem sa definim
set :: ListClass l => l a -> Natural.Natural -> a -> l a
set l n a = iter (\x -> if equal n zero then cons a else cons x) nil l
 
-- Vrem sa definim
insert :: ListClass l => l a -> Natural.Natural -> a -> l a
insert l n a = iter (\x -> if equal n zero then cons a . cons x else cons x) nil l
 
-- Vrem sa definim
remove :: ListClass l => l a -> Natural.Natural -> l a
remove l n = iter (\x -> if equal n zero then id else cons x) nil l
