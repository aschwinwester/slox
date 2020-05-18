object Main extends App {
  println("slox started")

  if (args.isEmpty) {
    println("no file")
  } else {
    val fileName:String = args(0)
    println("parsing file " + fileName)

  }

}

