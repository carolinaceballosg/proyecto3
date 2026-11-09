// Clases genericas. Esperado: "Caja con 5", "Caja con hola", 1, uno
object Caja[T](val contenido: T) {
  mostrar(): String = "Caja con " + contenido
}
object Par[A, B](val primero: A, val segundo: B)

method main {
  val c: Caja[Int] = new Caja[Int](5)
  val d = new Caja[String]("hola")
  val p = new Par[Int, String](1, "uno")
  println(c.mostrar())
  println(d.mostrar())
  println(p.primero)
  println(p.segundo)
}
