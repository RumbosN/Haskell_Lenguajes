-- {-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Functions where

import Terms
import Theorems

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

myShow :: a -> String
myShow a = show a

instance {-# OVERLAPPING  #-} Show Sust1 where
    show (t1, t2) =  "(" ++ show t1 ++ " =: " ++ show t2 ++ ")"
    
instance {-# OVERLAPPING  #-} Show DobSust where
    show (t1, (t2,t3), t4) =  "(" ++ show t1 ++ "," ++ show t2 ++" =: " ++ 
                               show t3 ++ "," ++ show t4 ++ ")"
    
instance {-# OVERLAPPING  #-} Show TripSust where
    show (t1, t2, (t3, t4), t5, t6) =  "(" ++ show t1 ++ "," ++ show t2 ++ "," ++ 
                                       show t3 ++ " =: " ++ show t4 ++ "," ++ show t5 ++
                                       "," ++ show t6 ++ ")"
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
statement :: (Sust t, Show t) => Float -> Ignore -> t -> Ignore -> 
                                 Ignore -> Term -> Term -> Term -> IO Term
statement n _ s _ _ (Var z) tE t0 = return("=== <statement " ++ show n ++ " with " ++ 
                                           show s ++ " using lambda " ++ [z] ++ " (" ++ 
                                           show tE ++ ")>\n")
                                    >>= \str1 -> (return (step t0 n s z tE) 
                                    >>= \term1 -> (putStrLn(str1 ++ show(term1)) 
                                    >>= \ignore -> (return term1)))
                                               
proof :: Equation -> IO Term
proof teorema@(Equal t1 t2) = return("prooving "++ show teorema ++ "\n\n" ++ show t1) 
                              >>= \str1 -> (putStrLn str1 >>= \ignore -> (return t1))

done :: Equation -> Term -> IO ()
done (Equal t1 t2) t3
    | t2 == t3 = return("\nproof successful") >>= \str -> (putStrLn str)
    | t2 /= t3 = return("\nproof unsuccessful") >>= \str -> (putStrLn str)

-- ##############################################################