// Ejemplo 4: objetos, herencia, override, metodos asignados, listas, for e interpolacion
// Salida esperada:
// Hola, soy Firulais
// Firulais dice guau
// Michi dice miau
// patas: 4
// 3 animales
// - Firulais
// - Michi
// - Rex
// Rex corre 2 veces
// Rex tiene 5 patas
// Pio dice pio
// x=5, doble=10
// "comillas" y \barra

object Animal(val nombre: String) {
  var patas = 4
  saludar(): String = "Hola, soy $nombre"
  sonido(): String = "..."
  hablar(): String = nombre + " dice " + sonido()
}

object Perro extends Animal {
  override sonido(): String = "guau"
}

object Gato extends Animal {
  override sonido(): String = "miau"
}

Perro.correr = method(veces: Int): String {
  println("$nombre corre $veces veces")
}

method main {
  val firulais = new Perro("Firulais")
  println(firulais.saludar())
  println(firulais.hablar())
  val michi: Animal = new Gato("Michi")
  println(michi.hablar())
  println("patas: ${michi.patas}")
  val rex = new Perro("Rex")
  val animales: List[Animal] = new List(firulais, michi, rex)
  println("${animales.size()} animales")
  for (a in animales) println("- ${a.nombre}")
  rex.correr(2)
  rex.patas = 5
  println("${rex.nombre} tiene ${rex.patas} patas")
  val pio = new Animal("Pio") {
    override sonido(): String = "pio"
  }
  println(pio.hablar())
  val x = 5
  println("x=$x, doble=${x * 2}")
  println("\"comillas\" y \\barra")
}
