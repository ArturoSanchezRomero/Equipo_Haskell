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

perdiste = do
            putStrLn "Perdiste "

ganaste = do
            putStrLn "Felicidades Ganaste"

tiraDado::IO Int
tiraDado = do
           dado<-randomRIO(1,6)
           return dado 

tiraDado2::IO Int
tiraDado2= do
           dado1<-randomRIO(1,6)
           dado2<-randomRIO(1,6)
           return (dado1+dado2)

valor::IO Int
valor= do
           return 7

juegaCrap :: IO () 
juegaCrap = do
    
    bienvenido
    dado1<-randomRIO(1,6)
    dado2<-randomRIO(1,6)
    let resultado = suma (dado1) (dado2)

    case (show (resultado)) of
        "7" ->  do
              putStrLn ("Felicidades Ganaste : Resultado 7")
        "11" ->  do
              putStrLn ("Felicidades Ganaste : Resultado 11")
        "2" ->  do
              putStrLn ("Perdiste : Resultado 2")
        "3" ->  do
              putStrLn ("Perdiste : Resultado 3")
        "12" -> do
              putStrLn ("Perdiste : Resultado 12")
        _    -> do
              putStrLn ("Nadie Gano : Vuelte a tirar")

              let punto = resultado
              dados1<-randomRIO(1,6)
              dados2<-randomRIO(1,6)
              let resultado2 = suma (dados1) (dados2)

              if(resultado2 == punto)then(putStrLn ("Iguales"))else do (if(juegaCrap2 == punto)then(putStrLn ("Iguales"))else(do juegaCrap2))

    return ()


juegaCrap2 ::IO ()
juegaCrap2 = do
     dados1<-randomRIO(1,6)
     dados2<-randomRIO(1,6)
     return (dados1+dados2)

     




suma::Int -> Int -> Int
suma x y = (x+y)



--Elabore un programa en haskell que simule este juego utilizando la programación iterativa y recursiva, 
--así como descomposición funciones y la generación de números aleatorios.

--a.- Modifique el programa para que el usuario pueda hacer apuesta y el programa indique quien gana la 
--apuesta. El programa debe solicitar al usuario si después de cada juego quiere seguir apostando, 
--cuando el usuario indique que no desea seguir jugando el programa indicará el monto que gano el jugador y 
--el monto que gano el computador, declarando un ganador que estará en función de la cantidad de juegos que 
--uno de los dos gano más veces que el otro, por ejemplo si se jugaron 5 rondas, y 3 gano el usuario y 2 el 
--computador, el ganador fue el usuario, si por alguna razón la misma cantidad de juegos fueron para ambos, 
--se declara un empate.


--2.- Elabore un programa que integre los ejercicios del robot para que solicite al usuario un robot y 
--después vaya solicitando las opciones que tiene para recibir órdenes.
--a) Mover 1 segmento el robot en dirección indicada por el mismo robot.
--b) Girar el robot a cualquiera de las 4 direcciones (norte, sur, este u oeste).
--c) Terminar

--El terminar el programa deberá mostrar la ruta que hizo, así como el tiempo final realizado.


--3.- Elabore un programa que gestione un Arbol binario, permitiendo agregar nodos en forma ordenada. 
--Imprimir el árbol en orden, preorden y postorden, conocer su altura, contabilizar las hojas. 
--Los ítems del árbol tendrán registros que contenga instancias de un tipo definido por el programador 
--llamado Libro que tenga título, lista de autores (Puede solo tener un autor) y número de páginas.

