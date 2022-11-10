
-- =======================================================
-- ==        PRACTICA 5 - FIGURAS Y ROBOT               ==
-- ==                                                   ==
-- ==         MIJANGOS GÁLVEZ JAFET 19011399            == 
-- =======================================================




-- =======================================
-- ==          EJERCICIO 1              ==
-- =======================================

data Figura = Rectangulo Float Float| Circulo Float | Triangulo Float Float | Cuadrado Float deriving (Show, Eq, Ord)

-- === Figuras de Prueba ===
figRectangulo = Rectangulo 12 6
figTriangulo = Triangulo 14 17
figCirculo = Circulo 5
figCuadrado = Cuadrado 10

-- =======================================
-- ==          EJERCICIO 2              ==
-- =======================================
getArea :: Figura -> Float 
getArea (Rectangulo b a)= b*a
getArea (Circulo r)= pi * (r^2)
getArea (Triangulo b a)= (b*a)/2
getArea (Cuadrado l) = l*l

-- =======================================
-- ==          EJERCICIO 3              ==
-- =======================================

data ListaFiguras = Lista [Figura] deriving (Show)

-- =======================================
-- ==        EJERCICIO 3.1              ==
-- =======================================
--Elaborar función para agregar una figura al final de una lista de figuras.

setNewFiguraAddListLast :: [Figura] -> Figura -> [Figura]
setNewFiguraAddListLast lf f = lf ++ [f]

-- =======================================
-- ==        EJERCICIO 3.2              ==
-- =======================================
--Elaborar función para agregar una figura al inicio de una lista de figuras

setNewFiguraAddListFirst :: [Figura] -> Figura -> [Figura]
setNewFiguraAddListFirst lf f = [f] ++ lf 

-- =======================================
-- ==        EJERCICIO 3.3              ==
-- =======================================
--Elaborar función para obtener una lista de cuadrados a partir de una lista de figuras.

getListCuadrados lf = [x | x <- lf , (take 8 (show (x))) == "Cuadrado" ]


-- =======================================
-- ==        EJERCICIO 3.4              ==
-- =======================================
--Elaborar función para obtener una lista de triángulos a partir de una lista de figuras.

getListTriangulos lf = [x | x <- lf , (take 9 (show (x))) == "Triangulo" ]


-- =======================================
-- ==        EJERCICIO 3.5              ==
-- =======================================
--Elaborar función para obtener una lista de círculos a partir de una lista de figuras.

getListCirculos lf = [x | x <- lf , (take 7 (show (x))) == "Circulo" ]

-- =======================================
-- ==        EJERCICIO 3.6              ==
-- =======================================
--Elaborar función para obtener una lista de círculos a partir de una lista de figuras.

getListRectangulos lf = [x | x <- lf , (take 10 (show (x))) == "Rectangulo" ]


-- =======================================
-- ==        EJERCICIO 3.7              ==
-- =======================================
--Elaborar una función que obtenga de una lista de figuras el área total que ocuparían 
--dentro de tela impresa.

getAreaTotal lf = sum [ getArea x | x <- lf ]





-- =======================================
-- ==         EJERCICIO 4               ==
-- =======================================
--Consideremos un robot móvil que se puede desplazar por una malla 
--similar a la siguiente:

--Giro de 90° -> 5 Segundos
-- 1 Metro -> 2 Segundos (Delgado)
-- 1 Metro -> 1 Segundos (Grueso)



-- =======================================
-- ==        EJERCICIO 4.1              ==
-- =======================================
--Indique de qué forma representaría la malla con el grueso de los tramos, 
--así como la posición del robot, el sentido al que apunta y el tiempo que lleva de recorrido. 
--Con la representación define una malla de 5x5 con al menos 12 segmentos gruesos y los demás delgados. 
--(una malla de 5x5 tiene 60 segmentos). Puedes utilizar arreglos o utilizar listas y/o tuplas.

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


-- =======================================
-- ==         EJERCICIO 4.2             ==
-- =======================================
--Elabore un función que cree un robot en una posición determinada con tiempo de recorrido a 0.

data Robot = Robot (Int,Int) Int Cardinales deriving (Show, Eq, Ord)

creatNewRobot (i,j) c = Robot (i,j) 0 c


-- =======================================
-- ==         EJERCICIO 4.3             ==
-- =======================================
--Elabore función que reciba un robot y retorne en qué posición se localiza.

positionRobot :: Robot -> (Int,Int)
positionRobot (Robot (i,j) _ _) = (i,j) 


-- =======================================
-- ==         EJERCICIO 4.4             ==
-- =======================================
--Elabore función que gire el robot en alguno de los sentidos válidos, considerar que 
--si está en posición a que se quiere mover no toma tiempo. Deberá retornar el nuevo estado 
--del robot (Posición, tiempo consumido y orientación)

data Cardinales = Norte | Este | Sur | Oeste deriving (Show, Eq, Ord)

giraRobot :: Robot -> Cardinales -> Robot 
giraRobot (Robot (i,j) t c) (n) = if (c == n)then(Robot (i,j) (t) c)else(Robot (i,j) (abs(((giros n) - (giros c))*5)) n)
         where
            giros v = if (v == Norte)then(1)else(if(v == Este)then(2)else(if(v == Sur)then(3)else(4)))

-- =======================================
-- ==         EJERCICIO 4.5             ==
-- =======================================
--Recorrer un lado de la malla (segmento) en función a donde está posicionado y 
--orientado el robot, retornar el nuevo estado del robot.

--Vetical es i 
--Horizontal es j

moveRobot :: Robot -> Robot
moveRobot (Robot (i,j) t c) | c == Norte && i > 0  = Robot (i-1,j) t c
                            | c == Este && j < 5 = Robot (i,j+1) t c
                            | c == Sur && i < 5 = Robot (i+1,j) t c
                            | c == Oeste && j > 0 =  Robot (i,j-1) t c
                            | otherwise  =  Robot (i,j) t c   

-- =======================================
-- ==         EJERCICIO 4.6             ==
-- =======================================
--De la posición (1,2) con orientación oeste guía al robot para que llegue a la 
--posición (4,3), elabora todas las posibilidades que consideres y verifica que ruta es la que 
--le consume menos tiempo, siguiendo el método del algoritmo de la fuerza bruta.


-- Estructura del árbol
data Abb a = Vacio | Nodo a (Abb a) (Abb a) deriving (Show)

-- #Fila * Cantidad de Columnas + #de Columna


--Formula Ubicación del valor (Grosor) Horizontal  #i x #Columnas Totales + #j = Posicion valor
--Formula Ubicacion del Valor (Grosor) Vertical    #j x #Filas Totales + #i  = Poisicon valor 


-- Crea un nuevo árbol a partir de una lista
-- crearDeLista (Lista de números)
crearDeLista :: (Ord a) => [a] -> Abb a
crearDeLista [] = Vacio
crearDeLista (raiz:sub) = Nodo raiz (crearDeLista (filter (<= raiz) sub)) (crearDeLista (filter (> raiz) sub))

rutas (inicioI,inicioJ) (finI, finJ) = [ruta1,ruta2]
   where
      ruta1 = [(inicioI,inicioJ)] ++ [(x,inicioJ)|x<-listax] ++ [(finI,y)|y<-listay]
      ruta2 = [(inicioI,inicioJ)] ++ [(inicioI,y)|y<-listay] ++ [(x,finJ)|x<-listax]
      listax = [x | x<-[(inicioI+1)..(finI)]]
      listay = [y | y<-[(inicioJ+1)..finJ]]


--ruta1 = [(1,2),(2,2),(3,2),(4,2),(4,3)]
--ruta2 = [(1,2),(2,2),(3,2),(3,3),(4,3)]
--ruta3 = [(1,2),(2,2),(2,3),(3,3),(4,3)]
--ruta4 = [(1,2),(1,3),(2,3),(3,3),(4,3)]
--allRutas = [ruta1,ruta2,ruta3,ruta4]



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
             


-- =======================================
-- ==         EJERCICIO 4.7             ==
-- =======================================
--Elabora una función que tome una malla, un robot y se dirija a la posición (x,y). 
--La función deberá retornar una lista de las acciones que debió realizar para llegar a su destino.


