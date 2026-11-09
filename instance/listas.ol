// Listas y ciclos. Esperado: "Suma: 14", "Tamano: 5", "a vale 1", "b vale 2"
method main {
  val numeros: List[Int] = new List(3, 1, 4, 1, 5)
  var suma = 0
  for (n in numeros) suma = suma + n
  println("Suma: $suma")
  println("Tamano: ${numeros.size()}")
  val pares = new List(new List("a", 1), new List("b", 2))
  for ((letra, valor) <- pares) println("$letra vale $valor")
}
