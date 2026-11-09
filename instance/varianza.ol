// Varianza como en el snippet 3 del enunciado, pero completo y ejecutable.
// Esperado: "Preparando comida para un animal..." y luego "Resultado: DogFood"
object Animal
object Dog extends Animal
object Puppy extends Dog

object Food
object DogFood extends Food

object Transformer[-In, +Out]
Transformer.transform = method(input: In): Out

method main {
  val animalFeeder: Transformer[Animal, DogFood] = new Transformer[Animal, DogFood] {
    override transform(animal: Animal): DogFood = {
      println("Preparando comida para un animal...")
      new DogFood
    }
  }
  val puppyFeeder: Transformer[Puppy, Food] = animalFeeder
  val myPuppy = new Puppy
  val resultantFood: Food = puppyFeeder.transform(myPuppy)
  println("Resultado: ${resultantFood.getClass.getSimpleName}")
}
