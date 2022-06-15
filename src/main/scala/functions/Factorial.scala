package functions

object Factorial {
  
  // TASK:
  //      Implement factorial as a recursive model.  What is the base
  //      case of the recursion?   Experimentally determine what happens
  //      if the input to the factorial function is large.  What is the
  //      largest Int for which you can correctly compute the factorial?
  def factorial(n: Int): Int = {

    if (n == 0)
      1
    else
      n * factorial(n - 1)
  }
  def main(argv: Array[String]): Unit = {
    for { n <- 1 to 20
    } println(s"$n --> Sc : ${factorial(n)}")
  }
}

//s = interprelate --> searches value into the println
//(s"$n  --> ${factorial(n)}") --> sequence good for debugging
//off-by-one problem --> 
//  functional programing is done without mutations --> Optimization / pessimation