// Programa CON errores a proposito: tipos de retorno y acceso a miembros.
// Cada linea marcada con ERROR deberia producir un mensaje (6 en total).

object Animal(val nombre: String) {
  sonido(): String = 5                 // ERROR: debe devolver String
  edad(): Int = 3
}

object Perro extends Animal

method main {
  val p = new Perro("Rex")
  val a: Int = p.nombre                // ERROR: nombre es String
  val b: String = p.edad()             // ERROR: edad() devuelve Int
  println(p.volar())                   // ERROR: Perro no tiene volar
  p.correr()                           // ERROR: Perro no tiene correr
  p.nombre = 3                         // ERROR: nombre es String
  val xs = new List(1, 2)
  val n: Int = xs.size()
  val ok: String = p.sonido()
}
