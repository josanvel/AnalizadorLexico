--Proyecto de Lenguajes de Programacion:
--Analizador Lexico
--Autores: José Antonnio Vélez Gómez
--	   Carlos Ramirez
--	   Keyla Figueroa

import Data.Char
import Data.List
import System.IO
import Data.List.Split

--Elimina los comentarios
comentarios :: String -> String
comentarios  cadena = do 	
					let 	puntoFinal = (indice1 "/*" cadena)
					let 	puntoInicial = (indice1 "*/" cadena) + 2	
					if((isInfixOf "/*" cadena) && (isInfixOf "*/" cadena)) == False 
						then cadena
						else comentarios [cadena !! x | x<-[0..puntoFinal - 1]++[puntoInicial..(length cadena) - 1]]

--Elimina las cabecera
cabecera :: String -> String
cabecera  cadena = do 	
					let 	puntoFinal = (indice1 "#" cadena)
					let 	puntoInicial = (indice1 ">" cadena) + 2	
					if((isInfixOf "#" cadena) && (isInfixOf ">" cadena)) == False 
						then cadena
						else cabecera [cadena !! x | x<-[0..puntoFinal - 1]++[puntoInicial..(length cadena) - 1]]

--Funcion que obtiene la posicion del caracter en otra cadena
indice1 :: String -> String -> Int
indice1 subCadena cadena = do
							if (null subCadena || null cadena) == True
								then length cadena
								else if (isInfixOf subCadena cadena) == False
										then length cadena
										else indice2 subCadena cadena ((length cadena) - (length subCadena))

indice2 :: String -> String -> Int -> Int
indice2 subCadena cadena indice = do
								if (isInfixOf subCadena (init cadena)) == False
									then indice
									else indice2 subCadena (init cadena) (indice - 1)

main::IO()
main = do
		putStrLn "\t\t\t*****************************"
		putStrLn "\t\t\t     ANALIZADOR LEXICO"
		putStrLn "\t\t\t  José Antonio Vélez Gómez"
		putStrLn "\t\t\t       Carlos Ramirez"
		putStrLn "\t\t\t       Keyla Figueroa"
		putStrLn "\t\t\t*****************************"
	
		putStrLn "\n\t\t\t\tArchivo Codec.c Abierto"
		eliminaComentariosCabeceras "code.c"
		codigoNuevo <- readFile "codeComentario.c"
		menuPrincipal (words codigoNuevo)
		putStrLn "Analizador Lexico Guardado en analizador.txt"

--Funcion Principal
menuPrincipal [] = return()
menuPrincipal manejable = do
					analizador manejable

--Funcion que lee el archivo y elimina los comentarios de lineas
--y los comentarios multilineas y las cabeceras		
eliminaComentariosCabeceras ruta = do
						codigoC <- readFile ruta
						let rComments = comentarios (codigoC)
						let rCabecera = cabecera (rComments)
						writeFile "codeComentario.c" rCabecera

--Funcion que toma las palabras reservadas y separa los tokens y lexemas del archivo
analizador:: [String] -> IO()
analizador (x:xs) = do
					listaPalabras <- openFile "palabraReservadas.c" ReadMode
					palReser <- hGetLine listaPalabras
					let listaSplitArchivo = split (oneOf "()<>{}[]+-*/=!?#:%&,;\\\"") x
					let listaN1 =borrarEspacios listaSplitArchivo []
					let pal = splitOn " " palReser
					let listaTupla = leerTuplas pal []
					identificadorPalabras listaTupla listaN1 listaTupla
					if (length(xs)==0)
						then return()
						else analizador xs

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
leerTuplas lista listaResultante = do
					if (length (tail lista)== 0)
						then listaResultante ++ [splitOn "jk" (head lista) ]
						else leerTuplas (tail lista) (listaResultante ++ [splitOn "jk" (head lista)])

--Identifica las diferentes Palabras Reservadas y los diferentes Operadores
identificadorPalabras :: [[String]]->[String]->[[String]]->IO()
identificadorPalabras (list1:listTail) list2 (listAux:listTailAux) = do
												if (length (tail list2) == 0)
													then if( length(listTail) == 0 )
															then if(elem (head list2) list1)
																	then do appendFile "analizador.txt" (show(tail list1)++"  =>  "++show(head list1)++ "\n")
																		--putStrLn $ show(tail list1)++" "++show(head list1)
																	else do appendFile "analizador.txt" ("Identificador   =>   "++show(head list2)++ "\n")
																		--putStrLn $ "Identificador: "++show(head list2)
															else if (elem (head list2) list1)
																	then do appendFile "analizador.txt" (show(tail list1)++"  =>  "++show(head list2)++ "\n")
																		--putStrLn $ show(tail list1)++" "++show(head list2)
																	else identificadorPalabras listTail list2 (listAux:listTailAux) 
													else if( length(listTail) == 0 )
															then if(elem (head list2) list1)
																	then do appendFile "analizador.txt" (show(tail list1)++"  =>  "++show(head list1)++ "\n")
																		--putStrLn $ show(tail list1)++" "++show(head list1)
																		identificadorPalabras (listAux:listTailAux) (tail list2) (listAux:listTailAux)
																	else do appendFile "analizador.txt" ("Identificador   =>   "++show(head list2)++ "\n") 
																		--putStrLn $ "Identificador: "++show(head list2)
																		identificadorPalabras (listAux:listTailAux) (tail list2) (listAux:listTailAux) 
															else if (elem (head list2) list1)
																	then do appendFile "analizador.txt" (show(tail list1)++"  =>  "++show(head list2)++ "\n") 
																		--putStrLn $ show(tail list1)++" "++show(head list2)
																		identificadorPalabras (listAux:listTailAux) (tail list2) (listAux:listTailAux)
																	else identificadorPalabras listTail list2 (listAux:listTailAux) 