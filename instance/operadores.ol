// Esperado: 7 errores, uno por cada linea de main
method main {
  val a = neg 5
  val b = if 3 then 1 else 2
  val c = true and 1
  val d = "x" < 3
  val e = 1 - "a"
  val f = -true
  val g = if true then 1 else "x"
}
