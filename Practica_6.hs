import System.Random
import Control.Monad



-- =======================================================
-- ==                   PRACTICA 6                     ==
-- ==                                                   ==
-- ==         MIJANGOS GÁLVEZ JAFET 19011399            == 
-- =======================================================


-- =======================================
-- ==          EJERCICIO 1              ==
-- =======================================

--1.- Un juego de azar popular es el juego de dados conocido como “Craps”, el cual se juega en casinos y 
--callejones por todo el mundo. Las reglas del juego son simples:
--Un jugador tira dos dados. Cada uno tiene seis caras, las cuales contienen uno, dos, tres cuatro, 
--cinco y seis puntos negros, respectivamente. Una vez que los dados dejan de moverse, se calcula la 
--suma de los puntos negros en las dos caras superiores. Si la suma es 7 u 11 en el primer tiro, el jugador 
--gana. Si la suma es 2, 3 o 12 en el primer tiro (llamado “Craps”), el jugador pierde 
--(es decir, la “casa” gana). Si la suma es 4, 5, 6, 8, 9 o 10 en el primer tiro, esta suma se convierte en el 
-- “punto” del jugador. Para ganar, el jugador debe seguir tirando los dados hasta que salga otra vez “su punto”
--(es decir, que tire ese mismo valor de punto). El jugador pierde si tira un 7 antes de llegar a su punto.

--Elabore un programa en haskell que simule este juego utilizando la programación iterativa y recursiva, así 
--como descomposición funciones y la generación de números aleatorios.

bienvenido = do
            putStrLn "Bienvenido al Juego Crap"


-- Funcion Principal --
juegaCrap :: IO () 
juegaCrap = do
    
    bienvenido
    dado1<-randomRIO(1,6)
    dado2<-randomRIO(1,6)
    let resultado = suma (dado1) (dado2)

    case (show (resultado)) of
        "7" ->  do
              putStrLn ("Felicidades Ganaste")
        "11" ->  do
              putStrLn ("Felicidades Ganaste")
        "2" ->  do
              putStrLn ("Perdiste")
        "3" ->  do
              putStrLn ("Perdiste")
        "12" -> do
              putStrLn ("Perdiste")
        _    -> do
              putStrLn ("Nadie Gano")

              let punto = resultado
              dados1<-randomRIO(1,6)
              dados2<-randomRIO(1,6)
              let resultado2 = suma (dados1) (dados2)

              if(resultado2 == punto)then(putStrLn ("Felicidades Ganaste"))else do juegaCrap2 punto

    return ()


juegaCrap2 :: Int -> IO ()
juegaCrap2 a = do

     dados1<-randomRIO(1,6)
     dados2<-randomRIO(1,6)
     let resultado2 = suma (dados1) (dados2)

     case (show (resultado2)) of
        "7" ->  do
              putStrLn ("Perdiste")
        a ->  do
              putStrLn ("Felicidades Ganaste")
        _    ->  do
              juegaCrap2 a

     return ()

suma::Int -> Int -> Int
suma x y = (x+y)



--a.- Modifique el programa para que el usuario pueda hacer apuesta y el programa indique quien gana la 
--apuesta. El programa debe solicitar al usuario si después de cada juego quiere seguir apostando, 
--cuando el usuario indique que no desea seguir jugando el programa indicará el monto que gano el jugador y 
--el monto que gano el computador, declarando un ganador que estará en función de la cantidad de juegos que 
--uno de los dos gano más veces que el otro, por ejemplo si se jugaron 5 rondas, y 3 gano el usuario y 2 el 
--computador, el ganador fue el usuario, si por alguna razón la misma cantidad de juegos fueron para ambos, 
--se declara un empate.


juegaCrapApuesta :: IO ()
juegaCrapApuesta = do
       putStrLn ("Bienvenido Al juego Crap con Apuestas")
       putStrLn ("")
       putStrLn ("Ingrese cuanto quiere postar")
       dinero <- getLine
       juegaCraps (read (dinero)::Int) (0) (0)

       
juegaCraps :: Int -> Int-> Int-> IO () 
juegaCraps dinero jugador pc = do
    
    bienvenido
    dado1<-randomRIO(1,6)
    dado2<-randomRIO(1,6)
    let resultado = suma (dado1) (dado2)

    case (show (resultado)) of
        "7" ->  do
              putStrLn ("Felicidades Ganaste")
              repetir (dinero) (aumenta (jugador)) (pc)

        "11" ->  do
              putStrLn ("Felicidades Ganaste")
              repetir (dinero) (aumenta (jugador)) (pc)
        "2" ->  do
              putStrLn ("Perdiste")
              repetir (dinero) (jugador) (aumenta (pc))
        "3" ->  do
              putStrLn ("Perdiste")
              repetir (dinero) (jugador) (aumenta (pc))
        "12" -> do
              putStrLn ("Perdiste")
              repetir (dinero) (jugador) (aumenta (pc))
        _    -> do
              putStrLn ("Sigue Intentando")

              let punto = resultado
              dados1<-randomRIO(1,6)
              dados2<-randomRIO(1,6)
              let resultado2 = suma (dados1) (dados2)


              case (show (resultado2)) of
                  punto ->  do
                            putStrLn ("Felicidades Ganaste")
                            repetir (dinero) (aumenta (jugador)) (pc)
                  _     -> do
                           juegaCraps2 punto dinero jugador pc

    return ()


juegaCraps2 :: Int -> Int-> Int-> Int -> IO () 
juegaCraps2 a dinero jugador pc= do

     dados1<-randomRIO(1,6)
     dados2<-randomRIO(1,6)
     let resultado2 = suma (dados1) (dados2)

     case (show (resultado2)) of
        "7" ->  do
              putStrLn ("Perdiste")
              repetir (dinero) (jugador) (aumenta (pc))
        a ->  do
              putStrLn ("Felicidades Ganaste")
              repetir (dinero) (aumenta (jugador)) (pc)
        _    ->  do
              juegaCraps2 a dinero jugador pc

     return ()


repetir :: Int -> Int-> Int-> IO () 
repetir dinero jugador pc = do

      putStrLn ("¿Quieres Seguir Jugando?  [YES/NO]")
      respuesta <- getLine
      case respuesta of
        "YES" ->  do 
                  juegaCraps dinero jugador pc
        "NO" ->  do
                  salir dinero jugador pc
      
      return ()
                  

salir :: Int -> Int-> Int-> IO () 
salir dinero jugador pc = do

      if (jugador < pc)then(putStrLn ("El ganaor es el Computador"))else(putStrLn ("Felicidades Ganaste"))

      putStrLn ("=========== Registro de Apuesta ===========")
      putStrLn ("")
      putStrLn ("Dinero del Jugador : "++ show (costos dinero jugador))
      putStrLn ("Dinero del Computador : "++ show (costos  dinero pc))



        
-- Funciones Extras
costos numero cantidad = numero * cantidad 
aumenta n = n + 1 



       




--2.- Elabore un programa que integre los ejercicios del robot para que solicite al usuario un robot y 
--después vaya solicitando las opciones que tiene para recibir órdenes.
--a) Mover 1 segmento el robot en dirección indicada por el mismo robot.
--b) Girar el robot a cualquiera de las 4 direcciones (norte, sur, este u oeste).
--c) Terminar

--El terminar el programa deberá mostrar la ruta que hizo, así como el tiempo final realizado.


robot :: IO ()
robot =
    do
        putStrLn "Ingresa un nuevo robot"
        putStrLn ("")
        putStrLn ("Cordenada I")
        i <- getLine
        putStrLn ("Cordenada J")
        j <- getLine
        putStrLn ("Cardinalidad")
        c <- getLine
        putStrLn ("Tiempo")
        t <- getLine

        case c of
            "Norte" -> do
                  operaRobot  (Robot (read (i)::Int, read (j)::Int) (read (t)::Int) Norte)
            "Este" -> do
                  operaRobot  (Robot (read (i)::Int, read (j)::Int) (read (t)::Int) Este)                    
            "Sur" -> do
                  operaRobot  (Robot (read (i)::Int, read (j)::Int) (read (t)::Int) Sur)
            "Oeste" -> do
                  operaRobot  (Robot (read (i)::Int, read (j)::Int) (read (t)::Int) Oeste)
                                   

operaRobot :: Robot-> IO ()
operaRobot (Robot (i,j) t c)=
    do putStr ("Ingresa la accion del robot [mover/girar/terminar]")
       putStrLn ""
       let lista = "Robot :  Cordenada (" ++ show i ++" , " ++ show j ++" )  Tiempo : " ++ show t ++ " Orientación : " ++ show c ++")"
       putStrLn ""
       operacion <- getLine
       case operacion of
            "girar" -> do 
                     putStr ("Ingresa la direccion del robot [Norte/Este/Sur/Oeste]")
                     putStrLn ""
                     n <- getLine
                     case n of
                        "Norte" -> do
                                   operaRobot (giraRobot (Robot (i,j) t c) (Norte))
                        "Este" -> do
                                   operaRobot (giraRobot (Robot (i,j) t c) (Este))
                        "Sur" -> do
                                   operaRobot (giraRobot (Robot (i,j) t c) (Sur))
                        "Oeste" -> do
                                   operaRobot (giraRobot (Robot (i,j) t c) (Oeste))
                        _ ->  operaRobot (Robot (i,j) t c)
            "mover" -> do 
                  operaRobot (moveRobot (Robot (i,j) t c))

            "terminar" -> do 
                     putStrLn ("Detalles de tu Robot") 
                     putStrLn ("Cordenada :  ("++show i++ " , "++show j++")") 
                     putStrLn ("Tiempo Transcurrido :  "++ show t)

                     case c of
                        Norte -> do
                                   putStrLn ("Cardinalidad :  Norte")
                        Este -> do
                                   putStrLn ("Cardinalidad :  Este")
                        Sur -> do
                                    putStrLn ("Cardinalidad :  Sur")
                        Oeste -> do
                                   putStrLn ("Cardinalidad :  Oeste")
                             
                        
            _ -> operaRobot (Robot (i,j) t c)

funcionTiempo t = t + 2      
                                    
moveRobot :: Robot -> Robot
moveRobot (Robot (i,j) t c) | c == Norte && i > 0  = Robot (i-1,j) t c
                            | c == Este && j < 5 = Robot (i,j+1) t c
                            | c == Sur && i < 5 = Robot (i+1,j) t c
                            | c == Oeste && j > 0 =  Robot (i,j-1) t c
                            | otherwise  =  Robot (i,j) t c 

data Robot = Robot (Int,Int) Int Cardinales deriving (Show, Eq, Ord)
data Cardinales = Norte | Este | Sur | Oeste deriving (Show, Eq, Ord)
creatNewRobot (i,j) c = Robot (i,j) 0 c

ruta1 = [(1,2),(2,2),(3,2),(4,2),(4,3)]
ruta2 = [(1,2),(2,2),(3,2),(3,3),(4,3)]
ruta3 = [(1,2),(2,2),(2,3),(3,3),(4,3)]
ruta4 = [(1,2),(1,3),(2,3),(3,3),(4,3)]

allRutas = [ruta1,ruta2,ruta3,ruta4]



horizontal3 =  [(x,2)| x<-[0..29]] 

horizontal2 l | length l == 0 = []
              | fst (head l) == 2 =  [(fst(head l),1)] ++  horizontal2 (drop 1 l)
              | fst (head l) == 3 =  [(fst(head l),1)] ++  horizontal2 (drop 1 l)
              | fst (head l) == 4 =  [(fst(head l),1)] ++  horizontal2 (drop 1 l)
              | fst (head l) == 15 = [(fst(head l),1)] ++  horizontal2 (drop 1 l)
              | fst (head l) == 21 = [(fst(head l),1)] ++  horizontal2 (drop 1 l)
              | fst (head l) == 22 = [(fst(head l),1)] ++  horizontal2 (drop 1 l)
              | otherwise = [head l] ++ horizontal2 (drop 1 l)

horizontal =  horizontal2 horizontal3


vertical3 =  [(x,2)| x<-[0..29]] 

vertical2 l | length l == 0 = []
              | fst (head l) == 16 =  [(fst(head l),1)] ++  vertical2 (drop 1 l)
              | fst (head l) == 17 =  [(fst(head l),1)] ++  vertical2 (drop 1 l)
              | fst (head l) == 28 =  [(fst(head l),1)] ++  vertical2 (drop 1 l)
              | fst (head l) == 29 = [(fst(head l),1)] ++  vertical2 (drop 1 l)
              | otherwise = [head l] ++ vertical2 (drop 1 l)

vertical = vertical2 vertical3
-- ============ Consultas ==================
consultaV n = [ x | (p,x) <- vertical, p == n ]
consultaH n = [ x | (p,x) <- horizontal, p == n ]


calLineas l    | length l == 0 = 0
               | length (drop 1 l) == 0 = 0
               | not (fst (head l) == fst (head (drop 1 l)))  = head (consultaV (snd (head l) * 5 + fst (head l))) + calLineas (drop 1 l) 
               | otherwise =  head (consultaH (fst (head l) * 5 + snd (head l))) + calLineas (drop 1 l)
               

calVuelta l  | length l <= 2 = 0
             | (fst (head l) < fst (head (drop 1 l)) && fst (head (drop 1 l)) <  fst (head (drop 2 l))  || snd (head l) == snd (head (drop 1 l)) && snd (head (drop 1 l)) ==  snd (head (drop 2 l))) = 0 + calVuelta (drop 1 l)
             | fst (head l) == fst (head (drop 1 l)) && fst (head (drop 1 l)) ==  fst (head (drop 2 l))  || snd (head l) < snd (head (drop 1 l)) && snd (head (drop 1 l)) <  snd (head (drop 2 l)) = 0 + calVuelta (drop 1 l)
             | otherwise = 5 +  calVuelta (drop 1 l)


-- ======= Calculo de vueltas y segmanetos
getTiempo (Robot (i,j) t c) = t

calvueltaInicio l |  fst (head l)  < fst (head (drop 1 l))  =  getTiempo(giraRobot (Robot (0,0) 0 Oeste) (Sur))
                  |  fst (head l)  > fst (head (drop 1 l))  =  getTiempo(giraRobot (Robot (0,0) 0 Oeste) (Norte))
                  |  snd (head l)  > snd (head (drop 1 l))  =  getTiempo(giraRobot (Robot (0,0) 0 Oeste) (Oeste))
                  |  otherwise  =  getTiempo(giraRobot (Robot (0,0) 0 Oeste) (Este))

calTol l = calLineas l + calVuelta l + calvueltaInicio l


-- ====== Lista de todos los tiempos de las Rutas ==============
alltiempos l | length l == 0 = []
             | otherwise = [[calTol (head l)]] ++ alltiempos (drop 1 l)

-- ======== Lista de la Ruta mas Optima FUNCION --> 4.6 <-- =========================
rutaOptima l | length l == 1 = head l
             | head (alltiempos l) <= head (drop 1 (alltiempos l)) = rutaOptima ([head l] ++ drop 2 l)
             | otherwise = rutaOptima (drop 1 l)
             
giraRobot (Robot (i,j) t c) (n) = if (c == n)then(Robot (i,j) (t) c)else(Robot (i,j) (abs(((giros n) - (giros c))*5)) n)
         where
            giros v = if (v == Norte)then(1)else(if(v == Este)then(2)else(if(v == Sur)then(3)else(4)))


-- =======================================
-- ==          EJERCICIO 3              ==
-- =======================================


--3.- Elabore un programa que gestione un Arbol binario, permitiendo agregar nodos en forma ordenada. 
--Imprimir el árbol en orden, preorden y postorden, conocer su altura, contabilizar las hojas. 
--Los ítems del árbol tendrán registros que contenga instancias de un tipo definido por el programador 
--llamado Libro que tenga título, lista de autores (Puede solo tener un autor) y número de páginas.

-- [(Título,Autor,Numero Hojas)]
info = [("Un libro","Donkijote",300),("La Excelencia","Arturo Emiliano",430),("El Final","Jafet",253),("Continuara","Paty",40)]
gestionaArbolBinario:: IO()
gestionaArbolBinario =
     do
       arbol info
       putStrLn ("Fin del programa gestion de arbol binario")

arbol :: [([Char], [Char], Int)] -> IO ()
arbol a =
    do
    let arbol1 = crearLista a
    putStrLn ("Gestion de arboles")
    putStr ("Que accion desea realizar? [insertar/imprimir/altura/totalHojas/salir]")
    putStrLn ("")
    s <- getLine
    case s of
        "insertar" -> do
                        putStrLn ("Ingresa un nuevo nodo")
                        putStrLn ("Título del Libro")
                        x <-  getLine
                        putStrLn ("Autor del Libro")
                        y <-  getLine
                        putStrLn ("Numero de Páginas del Libro")
                        z <-  getLine
                        arbol (a ++ [(x, y , read (z)::Int)])
        "imprimir" -> do
                        putStr ("Imprimir en: [orden/preorden/postorden]")
                        putStrLn ""
                        s2 <- getLine
                        case s2 of

                            "orden"     -> do
                                        putStrLn (show (preorden arbol1))
                                        arbol a
                            "preorden"  -> do
                                        putStrLn (show (preorden arbol1))
                                        arbol a
                            "postorden" -> do
                                        putStrLn (show (postorden arbol1))
                                        arbol a
        "altura" -> do
                      putStrLn (show (alturaarbol arbol1))
                      arbol a
        "totalHojas" -> do
                            putStrLn (show (totaldehojass arbol1))
                            arbol a
        "salir"  -> return()
        _        -> arbol a

data Abb a = Vacio | Nodo a (Abb a) (Abb a) deriving (Show)

crearLista [] = Vacio
crearLista (raiz:sub) = Nodo raiz (crearLista (filter (<= raiz) sub)) (crearLista (filter (> raiz) sub))

totaldehojass Vacio = 0 
totaldehojass (Nodo actual Vacio Vacio) = 1
totaldehojass (Nodo actual izq der) = totaldehojass izq + totaldehojass der

alturaarbol Vacio = 0
alturaarbol (Nodo _ izq der) = 1 + max (alturaarbol izq) (alturaarbol der)

preorden Vacio = []
preorden (Nodo actual izq der) = [actual] ++ preorden izq ++ preorden der

orden Vacio = []
orden (Nodo actual izq der) = orden izq ++ [actual] ++ orden der

postorden Vacio = []
postorden (Nodo actual izq der) = postorden izq ++ postorden der ++[actual]