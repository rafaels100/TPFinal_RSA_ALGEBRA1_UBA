module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits


-- (1)
{-
Variante del algoritmo de Euclides extendido tomado de la guía de álgebra.
La función recibe dos enteros a y b cuyo máximo común divisor se quiere calcular.
Devuelve un par cuyo primer elemento es un número entero (el máximo común divisor entre a y b) y cuyo segundo elemento es otro par con los coeficientes s y t de la combinación entera mediante la que se expresa el MCD.
-}
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt a b | b > a = mcdExt b a
mcdExt a 0 = (a, (1, 0))
mcdExt a b = (d, (t, s - t * k))
          where (k, r) = (div a b, mod a b)
                (d, (s, t)) = mcdExt b r


-- ##################################################################################

-- (2)

-- genera la lista de los números primos menores a n
criba :: Integer -> Set Integer
criba n = cribaAux n 2


-- genera una lista de números primos menores a n y mayores o iguales a k
cribaAux :: Integer -> Integer -> Set Integer
cribaAux n k | k < n && esPrimo k = (k : cribaAux n (k+1))
           | k < n = cribaAux n (k+1)
           | otherwise = []


{-
Determina si un número entero n es primo.
Si n es igual a 2, es primo y devuelve True.
Si n es mayor a 2, resuelve la ecuación de congruencia 2^(n - 1) congruente a X (módulo n). Si X = 1, el número es primo. En caso contrario, no lo es.
Esta función se basa en el test del pequeño teorema de Fermat. Si bien este test no es infalible, resulta, para los propósitos de este problema, una aproximación aceptable.
Utiliza como función auxiliar mcdExp, que permite resolver eficientemente estas ecuaciones.
-} 


esPrimo :: Integer -> Bool
esPrimo n | n == 2 = True
          | n > 2 = (modExp 2 (n-1) n) == 1
          | otherwise = undefined


-- ##################################################################################

-- (3) Dado un número n, busca un número coprimo con aquél. 
-- Es un caso particular de la función buscarCoprimo donde el segundo parámetro es (n - 2)
coprimoCon:: Integer -> Integer
coprimoCon n = buscarCoprimo n (n-2)


{-
Dado un entero n, la función usa el parámetro m para encontrar un coprimo de n.
Este parámetro m determina el intervalo en los enteros dentro del cual se buscan los coprimos de n.
Por ejemplo si n = 10 y m = 10, la función busca el primer coprimo en el intervalo m = 10; m = 1; lo que devuelve 9.
Si, en cambio, n = 10 y m = 8, la función busca el primer coprimo en el intervalo m = 8; m = 1; lo que devuelve 7.
Para esto se utiliza la función esCoprimo incializada en n m. Si ambos son coprimos, devuelve m.
En caso contrario, reduce el valor de m y vuelve a evaluar si m-1 es coprimo con n
-}
buscarCoprimo :: Integer -> Integer -> Integer
buscarCoprimo n m | esCoprimo n m = m
                  | otherwise = buscarCoprimo n (m-1)


{-
Determina si dos enteros m y n son coprimos evaluando si el divisor común mayor de ambos es 1.
Se utiliza la función divisorComúnMayor para evaluar esta condición.
-}

esCoprimo :: Integer -> Integer -> Bool
esCoprimo n m | m == n - 1 = False
              | otherwise = fst (mcdExt n m) == 1


-- ##################################################################################



-- ##################################################################################

{-
Dados dos números enteros a y b, calcula el inverso multiplicativo de a módulo b
Para eso busca el mcd entre a y b. La función mcdExt devuelve un par cuyo segundo elemento es otro par. 
Se extrae el segundo elemento de ese segundo elemento y se devuelve el resto entre ese número y b
-}
inversoMultiplicativo :: Integer -> Integer -> Integer
inversoMultiplicativo a b = mod (snd (snd (mcdExt a b))) b


-- ##################################################################################



-- ##################################################################################


-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1