// Solo para probar el PARSER: el interprete todavia no soporta objetos.
// Incluye los casos que fallaban en la retroalimentacion del proyecto 2.

object Animal(val nombre: String) {
  val patas = 4
  saludar(): String = "hola"
}

object Dog extends Animal

object Caja[T](val contenido: T)

// Asignacion de metodo con nombre de clase (mayuscula)
Animal.hablar = method(): String {
  println("...")
}

// Asignacion de metodo sin cuerpo
Dog.correr = method(veces: Int): Int

method main {
  val d = new Dog("firulais")
  val p = new Dog
  val lista: List[Dog] = new List(new Dog)
  val m = if 3 > 2 then 3 else 2
  d.hablar()
  new Dog
  new Dog("rex").hablar()
  for (x in lista) println(x)
  val texto = "dijo \"hola\" y $m o ${ m + 1 }"
  val negate = 1
  val newton = 2
  val v = negate + newton
}
