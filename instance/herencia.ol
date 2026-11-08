// Programa CORRECTO que usa miembros heredados y genericos.
// revisar(...) debe decir "Sin errores de tipo".
// correr(...) debe imprimir:
// Rex dice guau
// Rex tiene 4 patas
// Michi dice miau
// regalo
// Perro

object Animal(val nombre: String) {
  var patas = 4
  sonido(): String = "..."
}

object Perro extends Animal {
  override sonido(): String = nombre + " dice guau"
  contarPatas(): Int = patas
}

object Caja[T](val contenido: T)

Caja.abrir = method(): T {
  println(contenido)
}

method main {
  val p = new Perro("Rex")
  println(p.sonido())
  println("${p.nombre} tiene ${p.contarPatas()} patas")
  val gato = new Animal("Michi") {
    override sonido(): String = nombre + " dice miau"
  }
  println(gato.sonido())
  val c = new Caja("regalo")
  c.abrir()
  println(p.getClass())
}
