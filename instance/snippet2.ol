// Snippet 2 del enunciado del proyecto 1 (lineas marcadas).
// Se agregaron las clases y un main para poder ejecutarlo.
// Salida esperada:
// Cats meows
object Animal
object Cat extends Animal

Cat.talk = method(): String {          // snippet 2
  print("Cats meows")                  // snippet 2
}                                      // snippet 2

method main {
  val michi = new Cat
  michi.talk()
}
