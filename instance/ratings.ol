// Estilo snippet 4 (for con tupla e interpolacion).
// Esperado: "Movie: Alien, Rating: 9", "Movie: Cars, Rating: 6", "Movie: Up, Rating: 8", "Mejor puntaje: 9"
method main {
  val ratings = new List(new List("Alien", 9), new List("Cars", 6), new List("Up", 8))
  for ((name, rating) <- ratings) println("Movie: $name, Rating: $rating")
  val a = 9
  val b = 6
  val max = if a > b then a else b
  println("Mejor puntaje: $max")
}
