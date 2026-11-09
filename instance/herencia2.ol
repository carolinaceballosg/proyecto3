object Animal(val nombre: String) {
  val patas = 4
  sonido(): String = "..."
}
object Perro extends Animal {
  override sonido(): String = "guau"
  ladrar(): String = nombre + " dice " + sonido() + " y tiene " + patas + " patas"
}
object Cachorro extends Perro {
  jugar(): String = nombre + " juega y " + ladrar()
}
method main {
  val c = new Cachorro("rex")
  println(c.jugar())
}