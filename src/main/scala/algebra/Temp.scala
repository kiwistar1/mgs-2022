package algebra

object Temp {
   def main(argv:Array[String]):Unit = {
       val mylist = Set(1,2,3,4,6,8,10)

      println( mylist.exists{x => x%2 == 0})

      val evens = for{ x <- mylist
        if x%2 == 0
      } yield x

      println(evens.head)

      
      println(mylist.find{x => x > 100})
      println(mylist.find{x => x % 2 == 0}.get)

      val id = mylist.find{x => x < 2}

      println(mylist - id.get)
   }
}
