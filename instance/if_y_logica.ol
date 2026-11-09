// if, logica, comparaciones, aritmetica.
// Esperado: "max = 7", "a es mayor", true, false, false, z, siete, 1, 14, 20, 512, 2, -7
method main {
  val a = 7
  val b = 3
  val max = if a > b then a else b
  println("max = $max")
  if a > b then println("a es mayor") else println("b es mayor")
  val ambos = a > 5 and b < 5
  val alguno = a < 5 or b > 5
  println(ambos)
  println(alguno)
  println(neg ambos)
  val letra: Char = 'z'
  println(letra)
  println(if a = 7 then "siete" else "otro")
  println(a % 2)
  println(2 + 3 * 4)
  println((2 + 3) * 4)
  println(2 ** 3 ** 2)
  val n = -3 + 5
  println(n)
  println(-a)
}
