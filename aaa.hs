robot :: IO ()
robot =
    do
        putStrLn "Ingresa un nuevo robot"
        i <- getLine
        j <- getLine
        t <- getLine
        c <- getLine
        operaRobot i j t c
        putStrLn ("TERMINO EL PROGRAMA ROBOT")

operaRobot :: Int -> Int -> Int -> Cardinales -> IO ()
operaRobot i j t c =
    do putStr ("Ingresa la accion del robot [mover/girar/terminar]")
       putStrLn ""
       let lista = "Robot :  Cordenada (" ++ show i ++" , " ++ show j ++" )  Tiempo : " ++ show t ++ " Orientación : " ++ show c ++")"
       creatNewRobot (i,j) t c 
       list <- parsechar (lista)
       putStrLn ""
       operacion <- getLine
       case operacion of
           "girar" -> do putStr ("Ingresa la direccion del robot [N/S/E/O]")
                         putStrLn ""



data Robot = Robot (Int,Int) Int Cardinales deriving (Show, Eq, Ord)
data Cardinales = Norte | Este | Sur | Oeste deriving (Show, Eq, Ord)
creatNewRobot (i,j) c = Robot (i,j) 0 c

ruta1 = [(1,2),(2,2),(3,2),(4,2),(4,3)]
ruta2 = [(1,2),(2,2),(3,2),(3,3),(4,3)]
ruta3 = [(1,2),(2,2),(2,3),(3,3),(4,3)]
ruta4 = [(1,2),(1,3),(2,3),(3,3),(4,3)]

allRutas = [ruta1,ruta2,ruta3,ruta4]


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
             
