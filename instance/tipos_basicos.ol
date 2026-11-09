// Errores de tipos basicos. Esperado: 5 errores (a, b, c, d, e); f esta bien
method main {
  val a: Int = "hola"
  val b: Boolean = 5
  val c: Char = "x"
  val d: String = 'x'
  val e: Int = 3 + true
  val f: Int = 4
}
