object Main {
  // example "main" function
  def main(args: Array[String]) {
    args.foreach { case a =>
      println("Command line argument: '"+identity(a)+"'")
                }
  }
}
