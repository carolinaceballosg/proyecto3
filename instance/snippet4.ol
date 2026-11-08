// Snippet 4 del enunciado del proyecto 1 (lineas marcadas).
// Se agrego un main con a, b y ratings para poder ejecutarlo.
// Salida esperada (print no salta de linea):
// 7
// Movie: Matrix, Rating: 5Movie: Up, Rating: 4
method main {
  val a = 7
  val b = 3
  val ratings = new List(new List("Matrix", 5), new List("Up", 4))

  val max = if a > b then a else b                                             // snippet 4
  println(max)
  for ((name, rating) <- ratings) print("Movie: $name, Rating: $rating")      // snippet 4
}
