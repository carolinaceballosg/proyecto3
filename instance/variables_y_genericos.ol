// Esperado: 3 errores (noDeclarada, Q, T)
object Caja[T](val contenido: T)

method main {
  val a = noDeclarada + 1
  val b: Caja[Q] = new Caja[Int](1)
  val c: T = 5
}
