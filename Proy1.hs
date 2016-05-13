
--Definicion del tipo de dato Term

data Term =  Verdadero | Falso | Var Char | Or Term Term 
            | And Term Term | Implica Term Term | Neg Term 
            | Equiv Term Term | NegEquiv Term Term  

--Definicion de Variables y Constantes
true :: Term
true = Verdadero

false :: Term
false = Falso

a :: Term
a = Var 'a'

b :: Term
b = Var 'b'

c :: Term
c = Var 'c'

d :: Term
d = Var 'd'

e :: Term
e = Var 'e'

f :: Term
f = Var 'f'

g :: Term
g = Var 'g'

h :: Term
h = Var 'h'

i :: Term
i = Var 'i'

j :: Term
j = Var 'j'

k :: Term
k = Var 'k'

l :: Term
l = Var 'l'

m :: Term
m = Var 'm'

n :: Term
n = Var 'n'

o :: Term
o = Var 'o'

p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

s :: Term
s = Var 's'

t :: Term
t = Var 't'

u :: Term
u = Var 'u'

v :: Term
v = Var 'v'

w :: Term
w = Var 'w'

x :: Term
x = Var 'x'

y:: Term
y = Var 'y'

z :: Term
z = Var 'z'

-- Deficion de operadores
infixl 4 \/  
(\/) :: Term -> Term -> Term
(\/) t1 t2 = Or t1 t2

infixl 4 /\ 
(/\) :: Term -> Term -> Term
(/\) t1 t2 = And t1 t2

infixr 3 ==>
(==>):: Term -> Term -> Term
(==>) t1 t2 = Implica t1 t2

-- No se le coloca precedecia, por defecto es la maxima
neg :: Term -> Term
neg t = Neg t

infixl 2 <==>
(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = Equiv t1 t2

infixl 2 !<==>
(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = NegEquiv t1 t2

-- Definicion tipo ecuacion y del operador igualdad
data Equation = Equal Term Term

infixl 1 ===
(===) :: Term -> Term -> Equation
(===) t1 t2 =  Equal t1 t2

-- Para poder imprimir los terminos
instance Show Term where
    show (Var a) =  [a]
    show (Verdadero) = "true"
    show (Falso) = "false"
    show (Or a b) = "(" ++ show a ++ " \\/ " ++ show b ++ ")"
    show (And a b) = "(" ++  show a ++ " /\\ " ++ show b ++ ")"
    show (Implica a b) = "(" ++ show a ++ " ==> " ++ show b ++ ")"
    show (Neg a) = "~" ++ show a
    show (Equiv a b) = "(" ++ show a ++ " <==> " ++ show b ++ ")"
    show (NegEquiv a b) = "(" ++ show a ++ " !<==> " ++ show b ++ ")"

-- Definicion tipo de dato Sust
class Sust t
    casoBase :: Char -> t -> Term

type Sust1 = (Term, Term)
type DobSust = (Term, Sust1, Term)
type TripSust = (Term, Term, Sust1, Term, Term)

instance Sust Sust1 where
	casoBase a t = ()

instance Sust DobSust where
	casoBase a t = ()

instance Sust Sust1 where
	casoBase a t = ()

infixl 1 =:
(=:) :: Term -> Term -> Sust1
(=:) t1 t2 = (t1, t2)

sustitucion :: Term -> Sust t-> Term
sustitucion (Var a) s = casoBase a s

intantiate :: Equation -> Sust -> Equation
intantiate (Equal t1 t2) s = Equal (sustitucion t1 s) (sustitucion t2 s)

leibniz :: Equation -> Term -> Char -> Equation
leibniz (Equal t1 t2) tE z = Equal (sustitucion tE s1) (sustitucion tE s2) 
    where 
        s1 = (t1,Var z)
        s2 = (t2,Var z)

infer :: Float -> Sust -> Char -> Term -> Equation
infer n s z tE = leibniz (intantiate (prop n) s) tE z

