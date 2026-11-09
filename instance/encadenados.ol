// Llamadas encadenadas. Esperado: 6 y 10
object Calc(val base: Int) {
  doble(n: Int): Calc = new Calc(n * 2)
  valor(): Int = base
}

method main {
  val c = new Calc(1)
  println(c.doble(3).valor())
  c.doble(2).doble(5).valor()
  println(c.doble(2).doble(5).valor())
}
