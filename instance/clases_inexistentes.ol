// Esperado: 5 errores (Animal, Persona, Fantasma, Marciano, Alien)
object Perro extends Animal
object Casa(val dueno: Persona)
Fantasma.hablar = method(): String { print("boo") }

method main {
  val x = new Marciano
  val y: Alien = x
}
