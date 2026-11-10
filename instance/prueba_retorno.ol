object Animal {
  hablar(): String = "hola"
}

method main {
  val a = new Animal
  val x: Int = a.hablar()
}