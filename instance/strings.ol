// Strings. Esperado: Hola Ana / Tienes 20 anios y el doble es 40 / Dijo "hola" y costo $5 / Linea1 (salto) Linea2 / Suma: 12 / a / Letra: b / (linea vacia)
method main {
  val nombre = "Ana"
  val edad = 20
  println("Hola $nombre")
  println("Tienes ${ edad } anios y el doble es ${edad * 2}")
  println("Dijo \"hola\" y costo \$5")
  println("Linea1\nLinea2")
  println("Suma: " + 1 + 2)
  println('a')
  println("Letra: " + 'b')
  println("")
}
