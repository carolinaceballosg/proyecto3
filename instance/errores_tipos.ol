// Programa CON errores a proposito, para probar el Checker.
// Cada linea marcada con ERROR deberia producir un mensaje.

object Animal(val nombre: String)

object Perro extends Animal

object Gato extends Mascota // ERROR: Mascota no existe

object Caja[T](val contenido: T, val etiqueta: Etiqueta) // ERROR: Etiqueta no existe

method main {
  val a: Int = "hola" // ERROR: String no es Int
  val b: Boolean = 3 > 2
  val c: Char = 'x'
  val d: Char = "x" // ERROR: String no es Char
  val e = 1 + true // ERROR: no se puede sumar Int con Boolean
  val f = neg 5  // ERROR: neg espera Boolean
  println(noExiste) // ERROR: variable no declarada
  val p: Animal = new Perro("rex")
  val q: Perro = new Animal("tom")// ERROR: Animal no es subtipo de Perro
  val r = new Pajaro // ERROR: Pajaro no existe
  if 3 then println("x") // ERROR: la condicion debe ser Boolean
}
