--Proyecto de Lenguajes de Programacion:
--Analizador Lexico
--Autores: José Antonnio Vélez Gómez
--	   Carlos Ramirez
import System.IO
import System.Random 
import System.IO.Unsafe
import Control.Monad

main::IO()
main = do
		putStrLn "\t\t\t*****************************"
		putStrLn "\t\t\t     ANALIZADOR LEXICO"
		putStrLn "\t\t\t  José Antonio Vélez Gómez"
		putStrLn "\t\t\t       Carlos Ramirez"
		putStrLn "\t\t\t*****************************"
	
		putStrLn "\n\t\t\t\tArchivo Codec.c Abierto"
		codigo <- openFile "code.c" ReadMode
		mainloop codigo
		hClose codigo

--Funcion que toma la una lista de caracteres y hace los TOKENS
tokensCaracter :: [Char]->String->String
tokensCaracter (x:xs) cadena = if length (x:xs) == 1
						then concatenaLista (splitCadena cadena x)
						else tokensCaracter xs (concatenaLista (splitCadena cadena x))

--Funcion que concatena una Lista		
concatenaLista :: [String]->String
concatenaLista (x:xs) = if length(x:xs) == 1
					then x
					else x++concatenaLista(xs)

--Funcion que parte una cadena en slpit
splitCadena :: String -> Char -> [String]
splitCadena str delim = let (start, end) = break (== delim) str
		    in start : if null end then [] 
					else splitCadena (tail end) delim

--Funcion que lee Linea a Linea y se ejecuta todas las funciones
mainloop inh = do
			ineof <- hIsEOF inh
			if ineof
				then return ()
				else do
					inpStr <- hGetLine inh
					lista <- openFile "caracter.c" ReadMode
					caracter <- hGetContents lista
					let	palabra = tokensCaracter caracter inpStr
						listaLinea = splitCadena palabra ' '
					listaPalabras <- openFile "palabraReservadas.c" ReadMode
					palReser <- hGetLine listaPalabras

					handleDelimitadores <- openFile "delim.c" ReadMode
					delimitadores <- hGetLine handleDelimitadores
					let	listaDelimitadores = splitCadena delimitadores ' '
					putStrLn $ "L: "++show(listaLinea)
					let pal = splitCadena palReser ' '
					identificadorPalabras pal listaLinea pal--palabraReservada listaLinea palabraReservada
					mainloop inh 

--Funcion que te permite indentificar las palabras reservadas
identificadorPalabras ::[String] -> [String] -> [String] -> IO()
identificadorPalabras lO l1 l2 = do
				if (head l1) == (head l2)
					then if(length(tail l2) == 0)
						then if (length(tail l1) == 0)
							then	putStrLn $ "Identificador"++show(head l2)
							else do putStrLn $ "Identificador"++show(head l2)
								identificadorPalabras lO (tail l1) lO
						else if	(length (tail l1) == 0)
							then do putStrLn $ "Identificador"++show(head l2)
								identificadorPalabras lO l1 (tail l2)
							else do	putStrLn $ "Identificador"++show(head l2)
								identificadorPalabras lO l1 (tail l2)
					else if(length (tail l2) == 0)
						then if(length(tail l1) /= 0) 
							then  identificadorPalabras lO (tail l1) lO
							else return ()
						else identificadorPalabras lO l1 (tail l2)


funcion5 :: String -> [Char] -> [String]
funcion5 _ [] = []
funcion5 s l2 = do 
			let lista = splitCadena s (head l2)
			if lista == []
				then funcion5 s (tail l2)
				else do	[[head l2]]++funcion5 s (tail l2)
