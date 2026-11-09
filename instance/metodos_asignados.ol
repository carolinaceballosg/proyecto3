// Clase.metodo = method(...). Esperado: "Michi dice miau", "Rex dice guau", "hola"
object Cat(val nombre: String)
object Dog(val nombre: String)

Cat.talk = method(): String {
  println(nombre + " dice miau")
}
Dog.talk = method(): String {
  println(nombre + " dice guau")
}
Dog.eco = method(texto: String): String {
  texto
}

method main {
  val c = new Cat("Michi")
  val d = new Dog("Rex")
  c.talk()
  d.talk()
  println(d.eco("hola"))
}
