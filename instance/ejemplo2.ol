// Ejemplo 2: operadores, if como expresion y booleanos
// Salida esperada:
// 2
// 17
// false
// true
// true
// x vale 17
// contador: 1
// igual
method main {
  val x = 17
  val w = 5
  println(x % w)
  val mayor = if x > w then x else w
  println(mayor)
  val ambos = x > 10 and w > 10
  println(ambos)
  val alguno = x > 10 or w > 10
  println(alguno)
  println(neg ambos)
  println("x vale " + x)
  var contador = 0
  contador = contador + 1
  print("contador: ")
  println(contador)
  if x = 17 then println("igual") else println("distinto")
}
