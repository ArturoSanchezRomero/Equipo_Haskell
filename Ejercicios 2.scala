import scala.math
import scala.util.matching.Regex
import scala.compiletime.ops.string


// =======================================
// ==          EJERCICIO 1              ==
// =======================================


//1.- Elaborar función que, dado un entero natural, obtenga una lista con todos sus divisores. 
//Por ejemplo, si se pasa 15 deberá retornar [1, 3, 5, 15]

// ============Funcion Recursiva================

def funcion1 (n:Long):List[Long]= {
    (divisores (n, n)).reverse 
}

def divisores (z:Long ,n:Long):List[Long]= {
    if(z == 0){
    List()
    }else if (z == n){
    List(z) ++ (divisores ((z-1), n))
    }else{
        if((n % z) == 0){
        List(z) ++ (divisores ((z-1), n))
        }else{
            divisores ((z-1), n)
        }
    }
}


// =======================================
// ==          EJERCICIO 2              ==
// =======================================

//2.- Se dice que un número es perfecto si la suma de sus factores da el mismo número, 
//los factores deben excluir al mismo número, se tiene que elaborar una función que, dado un 
//entero, calcule los números perfectos que son menores o igual a dicho número, por ejemplo, 
//si se pasa 500 deberá retornar [6, 28, 496], ya que 6 = 1+2+3 28= 14+7+4+2+1
//496= 248+124+62+31+16+8+4+2+1



// ============Listas Por Comprención================
def funcion2_2(n:Int):List[Int]={
    for (i <- List.range(1, n) if (divs(i)).sum == i) yield (divs(i)).sum

}


// =======================================
// ==          EJERCICIO 3              ==
// =======================================

//3.- Implemente una función que obtenga todas las tuplas de grado 3 de enteros, que representan un 
//triángulo rectángulo, según el teorema de Pitágoras, La función solo recibe el límite máximo que 
//puede tener la hipotenusa. Por ejemplo, si se pasa 10 tendría que 
//retornar [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]


// ============Listas Por Comprención================
def funcion3_3(n:Int):List[((Int,Int,Int),(Int,Int,Int))]={
    for (c <- List.range(1, n+1); b <- List.range(1, c); a <- List.range(1, b) if math.pow(a,2) + math.pow(b,2) == math.pow(c,2)) yield ((a,b,c),(b,a,c))
}

// =======================================
// ==          EJERCICIO 4              ==
// =======================================

//4.- En cierta empresa los equipos de trabajo etiquetan las tareas a realizar en función a su 
//prioridad en función del día actual que puede ser cualquier dia de la semana. Por ejemplo 
//si tenemos la siguiente lista de tareas con su prioridad [5,4,3,12,3,2,1,9,8,1,4,3] 
//con día=3 (hoy) quiere decir que todas las tareas por debajo de 3 ya vencieron y se deben atender 
//inmediatamente incluyendo las del día de hoy, en este caso son 6 tareas(3,3,2,1,1,3), Para administrar 
//las tareas próximas se tiene como límite una semana inglesa (5 días) quiere decir
//que del día actual al 5to día son tareas próxima a realizar, en este caso 4 tareas (5,4,8,4), 
//una tercera categoría de tareas son las que podemos dejar para la siguiente semana su administración 
//siempre y cuando el vencimiento supere la semana inglesa partiendo del día de hoy, en este caso 2 
//tareas (12, 9). 
//Elaborar función que partiendo de una lista de vencimientos de tareas y el día actual, 
//retornar en una lista la lista de tareas que pertenecen a cada segmento 
//(tareas en vencimiento, tareas próximas a realizar y tareas para después de una semana).

// ============Funcion Recursiva================

def funcion4(l:List[Int],dia:Int):List[Int]={
    if(!(l.isEmpty)){
        if(dia <= 3 && (l.head) <= 3){
            List(l.head) ++ funcion4 ((l.drop(1)), dia)
        }else if(dia >= 4 && dia <= 5 && (l.head) >= 4 && (l.head) <= 8){
            List(l.head) ++ funcion4 ((l.drop(1)), dia)
        }else if(dia >= 6 && (l.head) >= 9 ){
            List(l.head) ++ funcion4 ((l.drop(1)), dia)
        }else{
            funcion4 ((l.drop(1)), dia)
        }
    }else{
        List()
    }
}


// =======================================
// ==          EJERCICIO 5              ==
// =======================================

//5.- Partiendo de una lista conformada por tuplas de la forma (nombre, edad, estado civil, sexo) 
//y resuelva los siguientes problemas.
//a) Obtener los nombres y estado Civil de los elementos de la lista (Proyección)
//b) Obtener los nombres de los que son divorciados (Proyección y Selección)
//c) Obtener de las mujeres la cantidad que son mayores de edad y están solteras (Agregación y selección)
//d) Obtener la cantidad de hombres que son mayores de edad, están casados y que proporción reflejan respecto a todos los hombres en la lista. (Agregación y selección)
//e) Obtener aquellas tuplas que son menores de edad indistintamente del sexo (Selección)

// ============Funcion Recursiva================

// funcion5A (List(("Marcos",30,"Divorciado","H"),("Juan",23,"Soltero","H"),("Pedro",15,"Soltero","H"),("Carlos",40,"Casado","H"), ("Kurumi",25,"Divorciada","M"), ("Nanami",23,"Soltera","M"), ("Beatriz",35,"Casada","M"), ("Yumeko",15,"Soltera","M"))) 
def funcion5A(l:List[(String,Int,String,String)]):List[(String,String)]={
    if(!(l.isEmpty)){
        List(nombreEstado (l.head)) ++ funcion5A (l.drop(1))
    }else{
        List()
    }
}


def funcion5B(l:List[(String,Int,String,String)]):List[(String)]={
    if(!(l.isEmpty)){
        nombreDivorciados (l.head) ++ funcion5B (l.drop(1))
    }else{
       List() 
    }
}

def funcion5C(l:List[(String,Int,String,String)]):Int={
    if(!(l.isEmpty)){
        mayoresSolteras (l.head) + funcion5C (l.drop(1))
    }else{
        0 
    }
}

def funcion5D(l:List[(String,Int,String,String)]):(Int,String)= (mayoresCasados (l), ( (mayoresCasados (l)* 100) / (hombres(l))).toString() ++ "%")


def funcion5E(l:List[(String,Int,String,String)]):List[(String,Int,String,String)]={
    if(!(l.isEmpty)){
        if(((l.head)._2) < 18){
        List(((l.head)._1 , (l.head)._2 , (l.head)._3 , (l.head)._4)) ++ funcion5E (l.drop(1))
        }else{
         funcion5E (l.drop(1)) 
        }
    }else{
        List() 
    }
} 

//Sub Funciones Requeridas para Ejercicio 5
def nombreEstado(l:(String,Int,String,String)):(String,String)= (l._1,l._3)
def nombreDivorciados(l:(String,Int,String,String)):List[String]= if((l._3).equalsIgnoreCase("Divorciado") ||  (l._3).equalsIgnoreCase("Divorciada")){List(l._1)}else{List()}
def mayoresSolteras(l:(String,Int,String,String)):Int= if((l._2) > 18 && (l._3).equalsIgnoreCase("Soltera") && (l._4).equalsIgnoreCase("M")){1}else{0}
def mayoresCasados(l:List[(String,Int,String,String)]):Int= if(!(l.isEmpty)){if(((l.head)._2) > 18 && ((l.head)._3).equalsIgnoreCase("Casado") && ((l.head)._4).equalsIgnoreCase("H")){1 + mayoresCasados(l.drop(1))}else{mayoresCasados(l.drop(1))}}else{0}
def hombres(l:List[(String,Int,String,String)]):Int= if(!(l.isEmpty)){if(((l.head)._4).equalsIgnoreCase("H")){1 + hombres(l.drop(1))}else{hombres(l.drop(1))}}else{0}


// =======================================
// ==          EJERCICIO 6              ==
// =======================================

//6.- Elaborar función que dado una palabra, donde encuentre una vocal la repita n veces. 
//Por ejemplo, si la función recibe abuela y 1, lo que retornará será aabuueelaa.


// ============Funcion Recursiva================

def funcion6(l:String, n:Int):String={
    if(!(l.length() == 0)){
        if((l.head).toString().equalsIgnoreCase("a") || (l.head).toString().equalsIgnoreCase("e") || (l.head).toString().equalsIgnoreCase("i") ||
           (l.head).toString().equalsIgnoreCase("o") || (l.head).toString().equalsIgnoreCase("u") ){
            (agrega(l,n)) ++ funcion6 (l.drop(1), n)
           }else{
            (l.head).toString() ++ funcion6 (l.drop(1), n)
           }
    }else{""}
}

def agrega(l:String ,contador:Int):String = if(contador == 0){(l.head).toString()}else{(l.head).toString() + agrega (l ,(contador-1))}


// =======================================
// ==          EJERCICIO 7              ==
// =======================================

//7.- Elaborar funciones que de una lista, que contiene los pagos que cada cliente ha realizado,
// en dicha lista el primer elemento son los pagos del primer clientes, el segundo elemento son
// los pagos del segundo cliente, así sucesivamente [[430,560,200],[1200,300],[120,130,150,180,120] ….] 
//se obtenga:
//a.- El número mínimo de pagos que se han realizado por todos los clientes
//b.- El número máximo de pagos que se han realizado por todos los clientes
//c.- El pago promedio de cada cliente
//d.- Una lista con la cantidad de pagos realizados por cada cliente
//e.- El monto de pago mínimo realizado por cada cliente
//f.- La cantidad de clientes que realizaron el pago mínimo de todos los clientes

//List(List(430,560,200),List(1200,300),List(120,130,150,180,120,100),List(300,200,100),List(300,100,700))
// ============Funcion Recursiva================

def funcion7A (l:List[List[Int]]):Int={

    if(!(l.length== 0)){
        if(l.length== 1){
            (l.head).length
        }else if((l.drop(2)).length == 0  && ((l.head).length <= ((l.drop(1)).head).length)){
            (l.head).length
        }else if((l.head).length <= ((l.drop(1)).head).length){
            funcion7A (List(l.head) ++ List((l.drop(2)).head))
        }else{
            funcion7A (l.drop(1))
        }
    }else{0}
}

def funcion7B (l:List[List[Int]]):Int={

    if(!(l.length== 0)){
        if(l.length== 1){
            (l.head).length
        }else if((l.drop(2)).length == 0  && ((l.head).length >= ((l.drop(1)).head).length)){
            (l.head).length
        }else if((l.head).length >= ((l.drop(1)).head).length){
            funcion7B (List(l.head) ++ List((l.drop(2)).head))
        }else{
            funcion7B (l.drop(1))
        }
    }else{0}
}


def funcion7C(l:List[List[Int]]):List[Int]={

    if(l.length == 0){
        List()
    }else{
        List(((l.head).sum)/((l.head).length)) ++ funcion7C (l.drop(1))
    }
}

def funcion7D(l:List[List[Int]]):List[List[Int]]={
    if(l.length == 0){
        List()
    }else{
        List(List(cuantapagos (l.head))) ++ funcion7D (l.drop(1))
    }
}

def funcion7E(l:List[List[Int]]):List[List[Int]]={
    if(l.length==0){
        List()
    }else{
        List(valorminimo (l.head)) ++ funcion7E (l.drop(1))
    }
}

def funcion7F(l:List[List[Int]]):Int={
    if(l.length == 0){
        0
    }else{
        guardar(l,l)
    }
}

//Funciones Extras Ejercicio 7
def cuantapagos(l:List[Int]):Int=if(l.length == 0){0}else{1 + cuantapagos (l.drop(1))}
def valorminimo(l:List[Int]):List[Int]=if(!(l.length==0)){if(l.length==1){List(l.head)}else if((l.head) <= ((l.drop(1)).head)){valorminimo (List(l.head) ++ (l.drop(2)))}else{valorminimo (l.drop(1))}}else{List()}

def guardar(l:List[List[Int]],ls:List[List[Int]]):Int={
    if(!(ls.length==0)){
        if((ls.drop(1)).length ==0 && (masminimo(l)) == ((funcion7E(ls)).head)){
            1
        }else if((ls.drop(1)).length==0){
            0
        }else if((masminimo(l)) == ((funcion7E(ls)).head)){
            1 + guardar (l ,(ls.drop(1)))
        }else{
            guardar(l, (ls.drop(1))) 
        } 
    }else{
        0
    }
}

def masminimo(l:List[List[Int]]):List[Int]={
    if(!(l.length==0)){
        if((l.drop(1)).length == 0){
            (funcion7E(l)).head
        }else if(((funcion7E(l)).head.head) <= (((funcion7E(l)).drop(1)).head.head)){
            masminimo (List((l.head)) ++ (l.drop(2)))
        }else{
            masminimo (l.drop(1))
        }
    }else{
        List()
    }
}


