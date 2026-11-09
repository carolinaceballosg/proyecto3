// var, asignaciones y campos var. Esperado: 10, 10, 15
object Contador(var cuenta: Int) {
  sumar(n: Int): Int = cuenta + n
}

method main {
  val c = new Contador(0)
  var total = 0
  total = total + 5
  total = total * 2
  println(total)
  c.cuenta = 10
  println(c.cuenta)
  println(c.sumar(5))
}
