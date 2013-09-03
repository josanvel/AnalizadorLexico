--Proyecto de Lenguajes de Programacion:
--Analizador Lexico
--Autores: José Antonnio Vélez Gómez
--	   Carlos Ramirez
--	   Keyla Figueroa

import Data.Char
import Data.Char(toUpper)
import Data.List
import System.IO
import Data.List.Split

comments :: String -> String
comments  string = do 	
					let 	finalPoint = (index1 "/*" string)
					let 	initialPoint = (index1 "*/" string) + 2	
					if((isSubstring "/*" string) && (isSubstring "*/" string)) == False 
						then string
						else comments[string !! x | x<-[0..finalPoint - 1]++[initialPoint..(length string) - 1]]

cabecera :: String -> String
cabecera  string = do 	
					let 	finalPoint = (index1 "#" string)
					let 	initialPoint = (index1 ">" string) + 2	
					if((isSubstring "#" string) && (isSubstring ">" string)) == False 
						then string
						else cabecera[string !! x | x<-[0..finalPoint - 1]++[initialPoint..(length string) - 1]]

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
		sinComentarios "code.c"
		codigo <- readFile "codeComentario.c"

		menuPrincipal (words codigo)
		--hClose codigo

--Funcion que lee Linea a Linea y se ejecuta todas las funciones
menuPrincipal [] = return()
menuPrincipal manejable = do
					funcion manejable
		
--sinComentarios:: String -> String 
sinComentarios arch = do
						codigo <- readFile arch
						let rComments = comments (codigo)
						let rCabecera = cabecera (rComments)
						writeFile "codeComentario.c" rCabecera

funcion:: [String] -> IO()
funcion (x:xs) = do
					listaPalabras <- openFile "palabraReservadas.c" ReadMode
					palReser <- hGetLine listaPalabras
					let listaSplitArchivo = split (oneOf "(,;<>{[+-?=!#:\\\"%&*)}]") x
					let listaN1 =borrarEspacios listaSplitArchivo []
					let pal = splitOn " " palReser
					let listaTupla = leerTuplas pal []
					identificadorPalabras listaTupla listaN1 listaTupla
					if (length(xs)==0)
						then return()
						else funcion xs


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
																	else identificadorPalabras listTail list2 (listAux:listTailAux) 
													else if( length(listTail) == 0 )
															then if(elem (head list2) list1)
																	then do putStrLn $ show(tail list1)++" "++show(head list1)
																		identificadorPalabras (listAux:listTailAux) (tail list2) (listAux:listTailAux)
																	else do putStrLn $ "Identificador: "++show(head list2)
																		identificadorPalabras (listAux:listTailAux) (tail list2) (listAux:listTailAux) 
															else if (elem (head list2) list1)
																	then do putStrLn $ show(tail list1)++" "++show(head list2)
																		identificadorPalabras (listAux:listTailAux) (tail list2) (listAux:listTailAux)
																	else identificadorPalabras listTail list2 (listAux:listTailAux) 