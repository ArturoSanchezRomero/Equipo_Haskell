import scala.math
import scala.util.matching.Regex



//1.- Elabora función que reciba dos enteros, el primero indica (x) el tamaño en dígitos de 
//los enteros que están codificados, el segundo es el entero codificado y retorne una lista 
//donde cada entero obtenido de longitud x, por ejemplo, si se pasa 3 y 83910304831 deberá 
//retornar [83,910,304,831].


//scala> funcion1(3,83910304831)
//val res11: List[Long] = List(83, 910, 304, 831)
def funcion1(n:Int, num:Long):List[Long]= {
    if(!(num==0)){
        (funcion1(n,(num/(math.pow(10, n)).round))).concat(List((num % (math.pow(10, n)).round)))
    }else{
        List()
    }
}

//2.- Elaborar una función que reciba una lista de enteros y n de tal forma que obtenga aquellos 
//que tengan más de n dígitos. Si se pasa [929,3241,123,829910,32] 4 ➔ [3241,829910]    

//scala> funcion2(List(929,3241,123,829910,32),4)
//val res10: List[Long] = List(3241, 829910)
def cuenta(n:Long):Int = if(n==0) 0 else 1 + cuenta (n/10)

def funcion2(l:List[Long], n:Int):List[Long]= l.filter( cuenta(_) >= n )

//3.- Elaborar función que dada una lista de nombres obtenga las personas cuyo nombre 
//tenga un patrón que es pasada como argumento. Por ejemplo: si se pasa:
//["juana","pedro","susana","maría","luisa","luisiana","martin","anayansi","sanatolia"] "ana" y 
// retorne [“juana”, “susana”, “luisiana”, “anayansi”, “sanatolia”]

//Jajajajaaj Atencion "Mounstro de Ifs" Pero Funciona :D 
//funcion3(List("juana","pedro","susana","maría","luisa","luisiana","martin","anayansi","sanatolia"), "ana")
//List[String] = List(juana, susana, luisiana, anayansi, sanatolia)
def funcion3(l:List[String],n:String):List[String] ={
    
     if(!(l.isEmpty)){

     if(((l.head).length)==(n.length)){

        if((l.head).equalsIgnoreCase(n)){
             (List(l.head)).concat(funcion3((l diff List(l.head)),n))
        }else{
           funcion3((l diff List(l.head)),n)
        }
       
        }else{

        if(((l.head).take(n.length)).equalsIgnoreCase(n)){
            (List(l.head)).concat(funcion3((l diff List(l.head)),n))
        }else{
           if(!(funcion3(List(((l.head).drop(1))),n)).isEmpty){
             (List(l.head)).concat(funcion3((l diff List(l.head)),n))
           }else{
            funcion3((l diff List(l.head)),n)
           }
        }
        }

    }else{
         List()
    }
}

//4.- Elaborar función que dada una lista que contienen listas de números que
//representan las calificaciones de los alumnos, obtenga cuales no presentan cambios
//descendientes en dichas calificaciones, por ejemplo:
//[[80,85,85,90],[70,80,75],[100,100,95],[70,90,75,70],[90,90,95]], en este caso solo
//2 no presentan cambios descendentes, [[80,85,85,90],[90,90,95]].

//funcion4(List(List(80,85,85,90),List(70,80,75),List(100,100,95),List(70,90,75,70),List(90,90,95)))
// List[List[Int]] = List(List(80, 85, 85, 90), List(90, 90, 95))
def funcion4(l:List[List[Int]]):List[List[Int]] = {

    if(!((l.drop(1))).isEmpty){
        if((decrece (l.head))==0){
             List(l.head).concat(funcion4(l.drop(1)))
        }else{
             funcion4(l.drop(1))
        }
    }else{
         List(l.head)
     }

}

def decrece(l:List[Int]):Int ={

    if(!(l.isEmpty)){
        if(!((l.drop(1))).isEmpty){
            if((l.head) > ((l.drop(1)).head)){
            1 + (decrece(l.drop(1)))
            }else{
            0 + (decrece(l.drop(1)))
        }
        }else{0}
    }else{
        0
    }
}


//5.- Elaborar función que dada una lista de números que representan las ventas diarias, 
//que obtenga una lista con la cantidad de días consecutivos se han tenido alzas en las 
//ventas, por ejemplo, si se pasa [10,45,67,32,90,13,20,10,90] ➔ [2,1,1,1] los primeros 2 
//días está al alza [10,45,67] el cuarto día fua a la baja, y subió al siguiente día (1 día), 
//ya que al siguiente fue nuevamente a la baja y así sucesivamente.

//funcion5(List(10,45,67,32,90,13,20,10,90))
//List[Int] = List(2, 1, 1, 1)
def funcion5(l:List[Int]):List[Int] = {

    if((aumenta (l))==((l.length)-1)){
        List(aumenta(l))
    }else{
        List(aumenta(l)).concat(funcion5(l.drop((aumenta (l))+1)))
    }
}


def aumenta(l:List[Int]):Int = {
    if(!(l.isEmpty)){
        if(!(l.drop(1)).isEmpty){
            if((decrece(List(l.head,(l.drop(1)).head)))==0){
            1+ aumenta(l.drop(1))
        }
        else{0}}else{0}}else{0}
} 


//6.- Elabore función que de una lista de cadenas de caracteres, obtenga una lista con la 
//cantidad de caracteres que no son letras que contiene cada lista. Por ejemplo si se pasa 
//["829juu", "ijioj2","hola"] ➔ [3,1,0]

//scala> funcion6(List("829juu","ijioj2","hola","12+sa2421ass{"))
//val res8: List[Int] = List(3, 1, 0, 8)

def funcion6(l:List[String]):List[Int] = if(!(l.isEmpty)){List((quitaletras(l.head)).length).concat(funcion6(l.drop(1)))}else{List()}

def quitaletras(a:String):String = {
    val x = new Regex("[qwertyuiopasdfghjklñzxcvbnm]")
    (x replaceAllIn(a,""))
}

//7.- Elaborar función que dado una lista de tuplas de enteros, cada tupla es de 2 enteros, 
//obtenga una lista con los valores menores entre cada par, si los valores son los mimos deberá 
//retornar 0. Por ejemplo si se pasa [(9,4), (7,3),(12,54),(8,8)] ➔ [4,3,12,0]

//funcion7(List((9,4),(7,3),(12,54),(8,8)))
//List[Int] = List(4, 3, 12, 0)
def funcion7(lt:List[(Int,Int)]):List[Int] = if(!(lt.isEmpty)){List(enteroMayor(lt.head)).concat(funcion7(lt.drop(1)))}else{List()}

def enteroMayor(t:(Int,Int)):Int = if((t._1) > (t._2)){t._2}else if((t._1) == (t._2)){0}else{t._1}