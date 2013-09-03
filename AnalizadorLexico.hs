--Proyecto de Lenguajes de Programacion:
--Analizador Lexico
--Autores: José Antonnio Vélez Gómez
--	   Carlos Ramirez
--	   Keyla Figueroa

import Data.Char
import Data.List
import System.IO
import Data.List.Split

index1 :: String -> String -> Int
index1 substring string
	| (null substring || null string) == True = length string
	| (isSubstring substring string) == False = length string
index1 substring string = index2 substring string ((length string) - (length substring))

index2 :: String -> String -> Int -> Int
index2 substring string index
	| (isSubstring substring (init string)) == False = index
index2 substring string index = index2 substring (init string) (index - 1)

isSubstring:: String -> String -> Bool
isSubstring substring string = isInfixOf substring string

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

main::IO()
main = do
		putStrLn "\t\t\t*****************************"
		putStrLn "\t\t\t     ANALIZADOR LEXICO"
		putStrLn "\t\t\t  José Antonio Vélez Gómez"
		putStrLn "\t\t\t       Carlos Ramirez"
		putStrLn "\t\t\t       Keyla Figueroa"
		putStrLn "\t\t\t*****************************"
	
		putStrLn "\n\t\t\t\tArchivo Codec.c Abierto"
		codigo <- openFile "code.c" ReadMode
		menuPrincipal codigo
		hClose codigo

--Funcion que lee Linea a Linea y se ejecuta todas las funciones
menuPrincipal manejable = do
			ineof <- hIsEOF manejable
			if ineof
				then return ()
				else do
					inpStr <- hGetLine manejable
					lista <- openFile "caracter.c" ReadMode
					caracter <- hGetContents lista
					let	palabra = tokensCaracter caracter inpStr
						listaLinea = splitOn " " palabra
					listaPalabras <- openFile "palabraReservadas.c" ReadMode
					palReser <- hGetLine listaPalabras

					handleDelimitadores <- openFile "delim.c" ReadMode
					delimitadores <- hGetLine handleDelimitadores
					let	listaDelimitadores = splitOn " " delimitadores
					let listLine = separaCaracter listaLinea listaDelimitadores []
					let listLine2 = borrarEspacios listLine []
					putStrLn $ "Lista2:        "++show(listLine2)
					let pal = splitOn " " palReser
					let listaTupla = leerTuplas pal []
					identificadorPalabras listaTupla listLine2 listaTupla
					menuPrincipal manejable 
					
--Borra elementos vacios de la lista de cada linea leida del archivo
borrarEspacios :: [String] -> [String] -> [String]
borrarEspacios [] [] = []
borrarEspacios lista listaNueva = do 
					if (length (tail lista)== 0)
						then if (length(head lista) > 0)
							then listaNueva ++ [head lista]
							else listaNueva
						else if (length(head lista) > 0)
							then borrarEspacios (tail lista) (listaNueva ++ [head lista])
							else borrarEspacios (tail lista) listaNueva
							
--Recibe una lista de String de las palabras reservadas del archivo
--y devuelve la lista con sus lexemas y tokens separados
leerTuplas :: [String] -> [[String]] -> [[String]]
leerTuplas [] [[]]= [[]]
leerTuplas list listResult = do
					if (length (tail list)== 0)
						then listResult ++ [splitOn "jk" (head list) ]
						else leerTuplas (tail list) (listResult ++ [splitOn "jk" (head list)])

-- Recibe un String y una lista de delimitadores verifica si en ese String hay caracteres especiales 
-- y devuelve una lista de String con los delimitadores separados 
buscarCaracter :: String -> [String] -> [String] -> [String]
buscarCaracter _ [] [] = []
buscarCaracter cadena listaChar listaNueva = do
										let in1 = (index1 (head listaChar) cadena) -1
										let in2 = (index1 (head listaChar) cadena) + 1-- listaChar>1
										if (isInfixOf (head listaChar) cadena) == False
												then if (length(tail listaChar)== 0)
												 		then [cadena] ++ listaNueva
												 		else buscarCaracter cadena (tail listaChar) listaNueva 
												else if (length(tail listaChar)== 0)
														then [[cadena !! x | x<-[0..in1]++[in2..length(cadena)-1]]]++listaNueva++[head listaChar]
														else do buscarCaracter [cadena !! x | x<-[0..in1]++[in2..length(cadena)-1]] listaChar (listaNueva++[head listaChar])

-- Funcion que recibe la lista String a la que se aplico split ´ ´ a la linea del archivo
-- tambien recibe la lista de delimitadores y retorna una lista nueva
separaCaracter :: [String] ->  [String] -> [String] -> [String]
separaCaracter [] [] [] = []
separaCaracter listL listD l = if(length (tail listL)== 0) == False
					then if(length(buscarCaracter (head listL) listD []) > 1) 
							then separaCaracter (tail listL) listD  ( l++(buscarCaracter (head listL) listD []) )
							else [head listL] ++ (separaCaracter (tail listL) listD l) 
				 	else 
				 		(buscarCaracter (head listL) listD l) 			 	 


--Identifica las diferentes Palabras Reservadas y los diferentes Operadores
identificadorPalabras :: [[String]]->[String]->[[String]]->IO()
identificadorPalabras (list1:listTail) list2 (listAux:listTailAux) = do
												if (length (tail list2) == 0)
													then if( length(listTail) == 0 )
															then if(elem (head list2) list1)
																	then putStrLn $ show(tail list1)++" "++show(head list1)
																	else putStrLn $ "Identificador: "++show(head list2)
															else if (elem (head list2) list1)
																	then putStrLn $ show(tail list1)++" "++show(head list2)
																	else identificadorPalabras listTail list2 (listAux:listTailAux)  --return ()
													else if( length(listTail) == 0 )
															then if(elem (head list2) list1)
																	then do putStrLn $ show(tail list1)++" "++show(head list1)
																		identificadorPalabras (listAux:listTailAux) (tail list2) (listAux:listTailAux)
																	else identificadorPalabras (listAux:listTailAux) (tail list2) (listAux:listTailAux) --putStrLn $ "Identificador: "++show(head list2)
															else if (elem (head list2) list1)
																	then do putStrLn $ show(tail list1)++" "++show(head list2)
																		identificadorPalabras (listAux:listTailAux) (tail list2) (listAux:listTailAux)
																	else identificadorPalabras listTail list2 (listAux:listTailAux) --return ()n