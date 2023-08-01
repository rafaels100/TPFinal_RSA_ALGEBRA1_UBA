module RSA where
import Tipos
import Aritmetica

{-
Recibe dos números primos p y q.
Genera: 
  - un número n = p * q
  - un número m = (p - 1) * (q - 1)
  - un número e que tiene que ser coprimo con el número m  = (p - 1) * (q - 1)
  - un número d que es el inverso multiplicativo de e módulo m

Devuelve un triplo de enteros (e, d, n) a partir de la cual se obtienen la clave pública (e, n) y la clave privada (d, n)
-}
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p * q
        m = (p - 1) * (q - 1)
        e = coprimoCon m
        d = inversoMultiplicativo e m


-- ##################################################################################



-- ##################################################################################
{- 
Codificar primer elemento de una lista
Toma un par de números que representan la clave pública (e, n) y una lista de números que representan el mensaje que se quiere enviar.
Toma el primer elemento de la lista head (l), lo eleva a e (el primer número de la clave pública) y devuelve el resto entre el resultado de esa operación y n (el segundo elemento de la clave pública)
-}
codificar :: (Integer, Integer) -> [Integer] -> Integer
codificar puk l = modExp (head l) (fst puk) (snd puk)

{-
Codificar toda una lista de enteros.
Usando como auxiliar la función anterior, codificar, toma los elementos de la lista uno a uno y los codifica todos.
-}
codificarLista :: (Integer, Integer) -> [Integer] -> [Integer]
codificarLista puk [] = []
codificarLista puk l = codificar puk l : codificarLista puk (tail l)


{-
Toma un par de números e y n de la clave pública y una lista de caracteres que representan el mensaje que se quiere codificar.
Utiliza la función auxiliar codificarLista donde el primer parámetro (puk) es el par que representa la clave pública y el segundo parámetro es el resultado de convertir los caracteres del mensaje en números enteros utilizando la función aEnteros.
Devuelve una lista de enteros que representa el mensaje encriptado.
-}
codificador :: (Integer, Integer) -> [Char] -> [Integer]
codificador puk s = codificarLista puk (aEnteros s)


-- #############################################################



-- #############################################################

{- Decodificar primer elemento de una lista
Recibe un par de enteros que representan la clave privada prk, (d, n, y una lista de enteros que representan el mensaje que se quiere decodificar.
Toma el primer elemento de la lista (head l) y lo eleva al primer elemento del par de la clave privada (d). Luego toma el segundo elemento de la clave privada (n) y devuelve el resto de dividir esos dos números.
-}
decodificar :: (Integer, Integer) -> [Integer] -> Integer
decodificar prk l = modExp (head l) (fst prk) (snd prk)

{-
Decodificar toda una lista de enteros
Recibe un par de enteros que representan la clave privada y una lista que representa el mensaje a desencriptar.
Utiliza la función auxiliar decodificar, que devuelve decodificado el primer elemento de la lista y vuelve a llamar a decodificarLista para el resto de la lista.
Devuelve una lista de enteros que representan el mensaje desencriptado.
-}
decodificarLista :: (Integer, Integer) -> [Integer] -> [Integer]
decodificarLista prk [] = []
decodificarLista prk l = decodificar prk l : decodificarLista prk (tail l)


{-
El ultimo paso es pasar esa lista de enteros desencriptados a caracteres utilizando la función aChars
Recibe el par que representa la clave privada prk, (d, n), y la lista de enteros desencriptada por decodificarLista y convierte ese resultado en caracteres.
Devuelve esa lista de caracteres que representan el mensaje original.
-}

decodificador :: (Integer, Integer) -> [Integer] -> [Char]
decodificador prk l = aChars (decodificarLista prk l)