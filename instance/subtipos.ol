// Esperado: 2 errores (c y d). a, b y e estan bien
object Animal
object Perro extends Animal
object Gato extends Animal

method main {
  val a: Animal = new Perro
  val b: Animal = new Gato
  val c: Perro = new Animal
  val d: Perro = new Gato
  val e: Gato = new Gato
}
