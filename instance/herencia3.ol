// Herencia, override y polimorfismo.
// Esperado: "Rex dice guau", "Toby dice guau (pero chiquito)", "Luna dice guau (pero chiquito)", "Luna juega"
object Animal(val nombre: String) {
  sonido(): String = "..."
  presentar(): String = nombre + " dice " + sonido()
}
object Perro extends Animal {
  override sonido(): String = "guau"
}
object Cachorro extends Perro {
  override sonido(): String = "guau (pero chiquito)"
  jugar(): String = nombre + " juega"
}

method main {
  val a: Animal = new Perro("Rex")
  val b: Animal = new Cachorro("Toby")
  val c: Cachorro = new Cachorro("Luna")
  println(a.presentar())
  println(b.presentar())
  println(c.presentar())
  println(c.jugar())
}
