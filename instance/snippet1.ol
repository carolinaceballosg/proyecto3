// Snippet 1 del enunciado del proyecto 1
// Salida esperada:
// Print the line of animal dogs: Dog()
object Animal
object Dog extends Animal
object Cat extends Animal

object List[+A](val item: A)

method main {
  val listOfDogs: List[Dog] = new List(new Dog)

  val listOfAnimals: List[Animal] = listOfDogs

  print("Print the line of animal dogs: ${listOfAnimals.item}")
}
