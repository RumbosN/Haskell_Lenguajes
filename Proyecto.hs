{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Proyecto where

-- ##############################################################
-- #############  Definicion del tipo de dato Term  #############
-- ##############################################################
data Term =  Verdadero | Falso | Var Char | Or Term Term 
            | And Term Term | Implica Term Term | Neg Term 
            | Equiv Term Term | NegEquiv Term Term  
-- ##############################################################




-- ##############################################################
-- ###########  Definicion de Variables y Constantes  ###########
-- ##############################################################
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
-- ##############################################################




-- ##############################################################
-- ##################  Definicion operadores  ###################
-- ##############################################################
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
-- ##############################################################




-- ##############################################################
-- #####  Definicion tipo ecuacion y del operador igualdad  #####
-- ##############################################################
data Equation = Equal Term Term

infixl 1 ===
(===) :: Term -> Term -> Equation
(===) t1 t2 =  Equal t1 t2
-- ##############################################################




-- ##############################################################
-- ################  Definicion varibles dummy  #################
-- ##############################################################
data Ignore = Ignorame

with :: Ignore
with = Ignorame

using :: Ignore
using = Ignorame

lambda :: Ignore
lambda = Ignorame
-- ##############################################################


-- ##############################################################
-- ######  Para poder imprimir los terminos y la Ecuacion  ######
-- ##############################################################
instance Show Term where
    show (Var a) =  [a]
    show (Verdadero) = "true"
    show (Falso) = "false"
    
    -- El mostrar bien parentizado el OR
    show (Or (Var a) (Var b)) = [a] ++ " \\/ " ++ [b]
    show (Or (Var a) Verdadero) = [a] ++ " \\/ " ++ "true"
    show (Or (Var a) Falso) = [a] ++ " \\/ " ++ "false"
    show (Or Verdadero (Var a)) = "true" ++ " \\/ " ++ [a]
    show (Or Falso (Var a)) = "false" ++ " \\/ " ++ [a]
    show (Or Verdadero Verdadero) = "true" ++ " \\/ " ++ "true"
    show (Or Falso Falso) = "false" ++ " \\/ " ++ "false"
    show (Or Verdadero Falso) = "true" ++ " \\/ " ++ "false"
    show (Or Falso Verdadero) = "false" ++ " \\/ " ++ "true"
    show (Or t Verdadero) = show t ++ " \\/ " ++ "true"
    show (Or t Falso) = show t ++ " \\/ " ++ "false"
    show (Or Verdadero t) = "true" ++ " \\/ " ++ show t
    show (Or Falso t) = "false" ++ " \\/ " ++ show t
    show (Or (Var a) t) = [a] ++ " \\/ (" ++ show t ++ ")"
    show (Or t (Var b)) = "(" ++ show t ++ ") \\/ " ++ [b]
    show (Or t1 t2) = "(" ++ show t1 ++ ") \\/ (" ++ show t2 ++ ")"
    
    -- El mostrar bien parentizado el AND
    show (And (Var a) (Var b)) = [a] ++ " /\\ " ++ [b]
    show (And (Var a) Verdadero) = [a] ++ " /\\ " ++ "true"
    show (And (Var a) Falso) = [a] ++ " /\\ " ++ "false"
    show (And Verdadero (Var a)) = "true" ++ " /\\ " ++ [a]
    show (And Falso (Var a)) = "false" ++ " /\\ " ++ [a]
    show (And Verdadero Verdadero) = "true" ++ " /\\ " ++ "true"
    show (And Falso Falso) = "false" ++ " /\\ " ++ "false"
    show (And Verdadero Falso) = "true" ++ " /\\ " ++ "false"
    show (And Falso Verdadero) = "false" ++ " /\\ " ++ "true"
    show (And t Verdadero) = show t ++ " /\\ " ++ "true"
    show (And t Falso) = show t ++ " /\\ " ++ "false"
    show (And Verdadero t) = "true" ++ " /\\ " ++ show t
    show (And Falso t) = "false" ++ " /\\ " ++ show t
    show (And (Var a) t) = [a] ++ " /\\ (" ++ show t ++ ")"
    show (And t (Var b)) = "(" ++ show t ++ ") \\/ " ++ [b]
    show (And t1 t2) = "(" ++ show t1 ++ ") /\\ (" ++ show t2 ++ ")"
    
    -- El mostrar bien parentizado el Implica
    show (Implica (Var a) (Var b)) = [a] ++ " ==> " ++ [b]
    show (Implica (Var a) Verdadero) = [a] ++ " ==> " ++ "true"
    show (Implica (Var a) Falso) = [a] ++ " ==> " ++ "false"
    show (Implica Verdadero (Var a)) = "true" ++ " ==> " ++ [a]
    show (Implica Falso (Var a)) = "false" ++ " ==> " ++ [a]
    show (Implica Verdadero Verdadero) = "true" ++ " ==> " ++ "true"
    show (Implica Falso Falso) = "false" ++ " ==> " ++ "false"
    show (Implica Verdadero Falso) = "true" ++ " ==> " ++ "false"
    show (Implica Falso Verdadero) = "false" ++ " ==> " ++ "true"
    show (Implica t Verdadero) = show t ++ " ==> " ++ "true"
    show (Implica t Falso) = show t ++ " ==> " ++ "false"
    show (Implica Verdadero t) = "true" ++ " ==> " ++ show t
    show (Implica Falso t) = "false" ++ " ==> " ++ show t
    show (Implica (Var a) t) = [a] ++ " ==> (" ++ show t ++ ")"
    show (Implica t (Var b)) = "(" ++ show t ++ ") ==> " ++ [b]
    show (Implica t1 t2) = "(" ++ show t1 ++ ") ==> (" ++ show t2 ++ ")"
    
    show (Neg a) = "~" ++ show a
    
    -- El mostrar bien parentizado el Equiv
    show (Equiv (Var a) (Var b)) = [a] ++ " <==> " ++ [b]
    show (Equiv (Var a) Verdadero) = [a] ++ " <==> " ++ "true"
    show (Equiv (Var a) Falso) = [a] ++ " <==> " ++ "false"
    show (Equiv Verdadero (Var a)) = "true" ++ " <==> " ++ [a]
    show (Equiv Falso (Var a)) = "false" ++ " <==> " ++ [a]
    show (Equiv Verdadero Verdadero) = "true" ++ " <==> " ++ "true"
    show (Equiv Falso Falso) = "false" ++ " <==> " ++ "false"
    show (Equiv Verdadero Falso) = "true" ++ " <==> " ++ "false"
    show (Equiv Falso Verdadero) = "false" ++ " <==> " ++ "true"
    show (Equiv t Verdadero) = show t ++ " <==> " ++ "true"
    show (Equiv t Falso) = show t ++ " <==> " ++ "false"
    show (Equiv Verdadero t) = "true" ++ " <==> " ++ show t
    show (Equiv Falso t) = "false" ++ " <==> " ++ show t
    show (Equiv (Var a) t) = [a] ++ " <==> (" ++ show t ++ ")"
    show (Equiv t (Var b)) = "(" ++ show t ++ ") <==> " ++ [b]
    show (Equiv t1 t2) = "(" ++ show t1 ++ ") <==> (" ++ show t2 ++ ")"
    
    -- El mostrar bien parentizado el Equiv
    show (NegEquiv (Var a) (Var b)) = [a] ++ " !<==> " ++ [b]
    show (NegEquiv (Var a) Verdadero) = [a] ++ " !<==> " ++ "true"
    show (NegEquiv (Var a) Falso) = [a] ++ " !<==> " ++ "false"
    show (NegEquiv Verdadero (Var a)) = "true" ++ " !<==> " ++ [a]
    show (NegEquiv Falso (Var a)) = "false" ++ " !<==> " ++ [a]
    show (NegEquiv Verdadero Verdadero) = "true" ++ " !<==> " ++ "true"
    show (NegEquiv Falso Falso) = "false" ++ " !<==> " ++ "false"
    show (NegEquiv Verdadero Falso) = "true" ++ " !<==> " ++ "false"
    show (NegEquiv Falso Verdadero) = "false" ++ " !<==> " ++ "true"
    show (NegEquiv t Verdadero) = show t ++ " !<==> " ++ "true"
    show (NegEquiv t Falso) = show t ++ " !<==> " ++ "false"
    show (NegEquiv Verdadero t) = "true" ++ " !<==> " ++ show t
    show (NegEquiv Falso t) = "false" ++ " !<==> " ++ show t
    show (NegEquiv (Var a) t) = [a] ++ " !<==> (" ++ show t ++ ")"
    show (NegEquiv t (Var b)) = "(" ++ show t ++ ") !<==> " ++ [b]
    show (NegEquiv t1 t2) = "(" ++ show t1 ++ ") !<==> (" ++ show t2 ++ ")"
    

instance Show Equation where
    show (Equal a b) = show a ++ "==" ++ show b
-- ##############################################################




-- ##############################################################
-- #####  Para poder aquiparar los terminos y la Ecuacion  ######
-- ##############################################################
instance Eq Term where
    (==) (Var a) (Var b) = a == b
    (==) (Verdadero) (Verdadero) = True
    (==) (Falso) (Falso) = True
    (==) (Or a b) (Or x y) = a == x && b == y
    (==) (And a b) (And x y) = a == x && b == y
    (==) (Implica a b) (Implica x y) = a == x && b == y
    (==) (Neg a) (Neg b) = a == b
    (==) (Equiv a b) (Equiv x y) = a == x && b == y
    (==) (NegEquiv a b) (NegEquiv x y) = a == x && b == y
    (==) _ _ = False
-- ##############################################################




-- ##############################################################
-- ###############  Definicion tipo de dato Sust  ###############
-- ##############################################################
type Sust1 = (Term, Term)
type DobSust = (Term, Sust1, Term)
type TripSust = (Term, Term, Sust1, Term, Term)

class Sust t where
    casoBase :: Char -> t -> Term
    
infixl 1 =:
(=:) :: Term -> Term -> Sust1
(=:) t1 t2 = (t1, t2)


instance Show Sust1 where
    show (t1, t2) =  "(" ++ show t1 ++ " =: " ++ show t2 ++ ")"
    
instance Show DobSust where
    show (t1, (t2,t3), t4) =  "(" ++ show t1 ++ "," ++ show t2 ++" =: " ++ show t3 ++ "," ++ show t4 ++ ")"
    
instance Show TripSust where
    show (t1, t2, (t3, t4), t5, t6) =  "(" ++ show t1 ++ "," ++ show t2 ++ "," ++ show t3 ++ " =: " ++ show t4 ++ "," ++ show t5 ++ "," ++ show t6 ++ ")"
-- ##############################################################




-- ##############################################################
-- ##################  Funcion Sustitucion  #####################
-- ##############################################################
instance Sust Sust1 where
	casoBase a (t1 , (Var p)) = if a == p then t1 else (Var a)
    -- Que pasa cuando el segundo termino da la tupla no es un Var?

instance Sust DobSust where
	casoBase a (t1, (t2, (Var p)), (Var q))
	    | a == p = t2
	    | a == q = t1
	    | otherwise = (Var a)
    -- que pasa cuando p es igual a q. Debe dar error?

instance Sust TripSust where
	casoBase a (t1, t2, (t3, (Var p)), (Var q), (Var r))
	    | a == p = t3
	    | a == q = t2
	    | a == r = t1
	    | otherwise = (Var a)
	    
sustitucion :: (Sust t) => Term -> t-> Term
sustitucion (Verdadero) _ = Verdadero
sustitucion (Falso) _ = Falso
sustitucion (Or t1 t2) s = Or (sustitucion t1 s) (sustitucion t2 s)
sustitucion (And t1 t2) s = And (sustitucion t1 s) (sustitucion t2 s)
sustitucion (Implica t1 t2) s = Implica (sustitucion t1 s) (sustitucion t2 s)
sustitucion (Neg t) s = Neg (sustitucion t s)
sustitucion (Equiv t1 t2) s = Equiv (sustitucion t1 s) (sustitucion t2 s)
sustitucion (NegEquiv t1 t2) s = NegEquiv (sustitucion t1 s) (sustitucion t2 s)
sustitucion (Var a) s = casoBase a s
-- ##############################################################




-- ##############################################################
-- #######  Funcion Instanciacion, Leibniz, Infer y Step  #######
-- ##############################################################
intantiate :: (Sust t) => Equation -> t -> Equation
intantiate (Equal t1 t2) s = Equal (sustitucion t1 s) (sustitucion t2 s)

leibniz :: Equation -> Term -> Char -> Equation
leibniz (Equal t1 t2) tE z = Equal (sustitucion tE s1) (sustitucion tE s2) 
    where 
        s1 = (t1,Var z)
        s2 = (t2,Var z)

infer :: (Sust t) => Float -> t -> Char -> Term -> Equation
infer n s z tE = leibniz (intantiate (prop n) s) tE z

step :: (Sust t) => Term -> Float -> t -> Char -> Term -> Term
step t1 n s z tE
    | t1 == td = ti
    | t1 == ti = td
    | otherwise = error "No se encuentra esta expresion en la ecuacion"
    where
       (Equal ti td) = infer n s z tE
-- ##############################################################



-- ##############################################################
-- #####################  Funciones IO  ######################
-- ##############################################################
statement :: (Sust t, Show t) => Float -> Ignore -> t -> Ignore -> Ignore -> Term -> Term -> Term -> IO Term
statement n _ s _ _ (Var z) tE t0 = return("=== <statement " ++ show n ++ " with " ++ show s ++ " using lambda " ++ [z] ++ " (" ++ show tE ++ ")>\n")
                                               >>= \str1 -> (return (step t0 n s z tE) >>= \term1 -> (putStrLn(str1 ++ show(term1)) >>= \ignore -> (return term1)))
                                               
proof :: Equation -> IO Term
proof teorema@(Equal t1 t2) = return("prooving "++ show teorema ++ "\n\n" ++ show t1) >>= \str1 -> (putStrLn str1 >>= \ignore -> (return t1))

--Esta funcion debe imprimir un mensaje de exito o fracaso
done :: Equation -> Term -> IO ()
done (Equal t1 t2) t3
    | t2 == t3 = return("\nproof successful") >>= \str -> (putStrLn str)
    | t2 /= t3 = return("\nproof unsuccessful") >>= \str -> (putStrLn str)

-- ##############################################################

prop :: Float -> Equation 
prop num
    | num == 3.1  = (p <==> q) <==> r === p <==> (q <==> r)
    | num == 3.2  = (p <==> q) === (q <==> p)
    | num == 3.3  = p <==> p === true
    | num == 3.4  = p === p <==> true
    | num == 3.5  = (p <==> q) <==> q === p
    | otherwise = error "The statement doesn't exists"