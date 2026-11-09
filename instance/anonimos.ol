// toString y objetos anonimos.
// Esperado: "Ana (20)", "hola desde un objeto anonimo", "Persona anonima: Luis"
object Persona(val nombre: String, val edad: Int) {
  toString(): String = nombre + " (" + edad + ")"
}
object Saludo {
  decir(): String = "hola"
}

method main {
  val p = new Persona("Ana", 20)
  println(p)
  val s = new Saludo {
    override decir(): String = "hola desde un objeto anonimo"
  }
  println(s.decir())
  val q = new Persona("Luis", 30) {
    override toString(): String = "Persona anonima: " + nombre
  }
  println(q)
}
