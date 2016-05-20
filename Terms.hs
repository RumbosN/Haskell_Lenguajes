module Terms where

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
    show (Or (Neg t1) (Var a)) = show (Neg t1) ++ " \\/ " ++ [a]
    show (Or (Var a) (Neg t2)) = [a] ++ " \\/ " ++ show (Neg t2)
    show (Or (Neg t1) Verdadero) = show (Neg t1) ++ " \\/ true"
    show (Or Verdadero (Neg t2)) = "true \\/ " ++ show (Neg t2)
    show (Or (Neg t1) Falso) = show (Neg t1) ++ " \\/ false"
    show (Or Falso (Neg t2)) = "false \\/ " ++ show (Neg t2)
    show (Or (Neg t1) (Neg t2)) = show (Neg t1) ++ " \\/ " ++ show (Neg t2)
    show (Or (Neg t1) t2) = show (Neg t1) ++ " \\/ (" ++ show t2 ++ ")"
    show (Or t1 (Neg t2)) = "(" ++ show t1 ++ ") \\/ " ++ show (Neg t2)
    show (Or t Verdadero) = "(" ++ show t ++ ") \\/ " ++ "true"
    show (Or t Falso) = "(" ++ show t ++ ") \\/ " ++ "false"
    show (Or Verdadero t) = "true" ++ " \\/ (" ++ show t ++ ")"
    show (Or Falso t) = "false" ++ " \\/ (" ++ show t ++ ")"
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
    show (And (Neg t1) (Var a)) = show (Neg t1) ++ " /\\ " ++ [a]
    show (And (Var a) (Neg t2)) = [a] ++ " /\\ " ++ show (Neg t2)
    show (And (Neg t1) Verdadero) = show (Neg t1) ++ " /\\ true"
    show (And Verdadero (Neg t2)) = "true /\\ " ++ show (Neg t2)
    show (And (Neg t1) Falso) = show (Neg t1) ++ " /\\ false"
    show (And Falso (Neg t2)) = "false /\\ " ++ show (Neg t2)
    show (And (Neg t1) (Neg t2)) = show (Neg t1) ++ " /\\ " ++ show (Neg t2)
    show (And (Neg t1) t2) = show (Neg t1) ++ " /\\ (" ++ show t2 ++ ")"
    show (And t1 (Neg t2)) = "(" ++ show t1 ++ ") /\\ " ++ show (Neg t2)    
    show (And t Verdadero) = "(" ++ show t ++ ") /\\ " ++ "true"
    show (And t Falso) = "(" ++ show t ++ ") /\\ " ++ "false"
    show (And Verdadero t) = "true" ++ " /\\ (" ++ show t ++ ")"
    show (And Falso t) = "false" ++ " /\\ (" ++ show t ++ ")"
    show (And (Var a) t) = [a] ++ " /\\ (" ++ show t ++ ")"
    show (And t (Var b)) = "(" ++ show t ++ ") /\\ " ++ [b]
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
    show (Implica (Neg t1) (Var a)) = show (Neg t1) ++ " ==> " ++ [a]
    show (Implica (Var a) (Neg t2)) = [a] ++ " ==> " ++ show (Neg t2)
    show (Implica (Neg t1) Verdadero) = show (Neg t1) ++ " ==> true"
    show (Implica Verdadero (Neg t2)) = "true ==> " ++ show (Neg t2)
    show (Implica (Neg t1) Falso) = show (Neg t1) ++ " ==> false"
    show (Implica Falso (Neg t2)) = "false ==> " ++ show (Neg t2)
    show (Implica (Neg t1) (Neg t2)) = show (Neg t1) ++ " ==> " ++ show (Neg t2)
    show (Implica (Neg t1) t2) = show (Neg t1) ++ " ==> (" ++ show t2 ++ ")"
    show (Implica t1 (Neg t2)) = "(" ++ show t1 ++ ") ==> " ++ show (Neg t2)
    show (Implica t Verdadero) = "(" ++ show t ++ ") ==> " ++ "true"
    show (Implica t Falso) ="(" ++  show t ++ ") ==> " ++ "false"
    show (Implica Verdadero t) = "true" ++ " ==> (" ++ show t ++ ")"
    show (Implica Falso t) = "false" ++ " ==> (" ++ show t ++ ")"
    show (Implica (Var a) t) = [a] ++ " ==> (" ++ show t ++ ")"
    show (Implica t (Var b)) = "(" ++ show t ++ ") ==> " ++ [b]
    show (Implica t1 t2) = "(" ++ show t1 ++ ") ==> (" ++ show t2 ++ ")"
    
    -- El mostrar bien parentizado la negacion
    show (Neg (Var z)) = "~" ++ [z]
    show (Neg Verdadero) = "~" ++ "true"
    show (Neg Falso) = "~" ++ "false"
    show (Neg t) = "~(" ++ show t ++ ")"  

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
    show (Equiv (Neg t1) (Var a)) = show (Neg t1) ++ " <==> " ++ [a]
    show (Equiv (Var a) (Neg t2)) = [a] ++ " <==> " ++ show (Neg t2)
    show (Equiv (Neg t1) Verdadero) = show (Neg t1) ++ " <==> true"
    show (Equiv Verdadero (Neg t2)) = "true <==> " ++ show (Neg t2)
    show (Equiv (Neg t1) Falso) = show (Neg t1) ++ " <==> false"
    show (Equiv Falso (Neg t2)) = "false <==> " ++ show (Neg t2)
    show (Equiv (Neg t1) (Neg t2)) = show (Neg t1) ++ " <==> " ++ show (Neg t2)
    show (Equiv (Neg t1) t2) = show (Neg t1) ++ " <==> (" ++ show t2 ++ ")"
    show (Equiv t1 (Neg t2)) = "(" ++ show t1 ++ ") <==> " ++ show (Neg t2)  
    show (Equiv t Verdadero) = "(" ++ show t ++ ") <==> " ++ "true"
    show (Equiv t Falso) = "(" ++ show t ++ ") <==> " ++ "false"
    show (Equiv Verdadero t) = "true" ++ " <==> (" ++ show t ++ ")"
    show (Equiv Falso t) = "false" ++ " <==> (" ++ show t ++ ")"
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
    show (NegEquiv (Neg t1) (Var a)) = show (Neg t1) ++ " !<==> " ++ [a]
    show (NegEquiv (Var a) (Neg t2)) = [a] ++ " !<==> " ++ show (Neg t2)
    show (NegEquiv (Neg t1) Verdadero) = show (Neg t1) ++ " !<==> true"
    show (NegEquiv Verdadero (Neg t2)) = "true !<==> " ++ show (Neg t2)
    show (NegEquiv (Neg t1) Falso) = show (Neg t1) ++ " !<==> false"
    show (NegEquiv Falso (Neg t2)) = "false !<==> " ++ show (Neg t2)
    show (NegEquiv (Neg t1) (Neg t2)) = show (Neg t1) ++ " !<==> " ++ show (Neg t2)
    show (NegEquiv (Neg t1) t2) = show (Neg t1) ++ " !<==> (" ++ show t2 ++ ")"
    show (NegEquiv t1 (Neg t2)) = "(" ++ show t1 ++ ") !<==> " ++ show (Neg t2)    
    show (NegEquiv t Verdadero) = "(" ++ show t ++ ") !<==> " ++ "true"
    show (NegEquiv t Falso) = "(" ++ show t ++ ") !<==> " ++ "false"
    show (NegEquiv Verdadero t) = "true" ++ " !<==> (" ++ show t ++ ")"
    show (NegEquiv Falso t) = "false" ++ " !<==> (" ++ show t ++ ")"
    show (NegEquiv (Var a) t) = [a] ++ " !<==> (" ++ show t ++ ")"
    show (NegEquiv t (Var b)) = "(" ++ show t ++ ") !<==> " ++ [b]
    show (NegEquiv t1 t2) = "(" ++ show t1 ++ ") !<==> (" ++ show t2 ++ ")"
    

instance Show Equation where
    show (Equal a b) = show a ++ " === " ++ show b
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




