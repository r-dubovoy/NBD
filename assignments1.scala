import scala.annotation.tailrec

object assignments1 {
  val days = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  val products = Map("clothes" -> 1000, "shoes" -> 500)

  def main(args: Array[String]): Unit = {
    println("task_1:")
    println(task_1("a"))
    println(task_1("b"))
    println(task_1("c"))
    println("task_2:")
    println(task_2("a", days))
    println(task_2("b", days))
    println("task_3:")
    println(task_3(days))
    println("task_4:")
    println(task_4("a"))
    println(task_4("b"))
    println(task_4("c"))
    println("task_5:")
    println(task_5)
    println("task_6:")
    println(task_6(1 :: 2 :: 3 :: Nil))
    println("task_7")
    println(task_7(-11 :: -2.4 :: 3.0 :: 20.3 :: Nil))
    println("task_8")
    task_8(Tuple3(1, 300.2, "abc"))
    println("task_9")
    println(task_9(-1.3 :: 2.1 :: 0.0 :: 3.4 :: 0.0 :: Nil))
    println("task_10")
    task_10("shoes")
    task_10("clothes")
    task_10("socks")

  }

  def task_1(t: String): String = {
    var s: String = ""
    if (t.equals("a"))
      for (day <- days)
        s += day + ", "
    else if (t.equals("b"))
      for (day <- days if day.toLowerCase.startsWith("s"))
        s += day + ", "
    else if (t.equals("c")) {
      var i: Int = 0
      while (i < 7) {
        s += days(i) + ", "
        i += 1
      }
    }
    return s.substring(0, s.length - 2)
  }

  def task_2(t: String, l: List[String]): String = {
    if (t == "a") {
      if (l.tail.isEmpty)
        return l.head
      return l.head + ", " + task_2("a", l.tail)
    }
    if (t == "b") {
      if (l.tail.isEmpty)
        return l.head
      return task_2("a", l.tail) + ", " + l.head
    } else return ""
  }
  def task_3(ll: List[String]): String = {
    @tailrec
    def task33(l: List[String],s:String): String = {
      if (l.tail.isEmpty)
        return s+l.head
      else   task33(l.tail,s+l.head + ", ")
    }
    task33(ll,"")
  }

  def task_4(t: String): String = {
    var s: String = ""
    if (t.equals("a"))
      s = days.foldLeft("")(_ + _ + ", ")
    else if (t.equals("b"))
      s = days.foldRight("")(_ + ", " + _)
    else if (t.equals("c"))
      s = days.foldRight("") { (next, sum) => if (next.toLowerCase.startsWith("s")) next + ", " + sum else sum }
    return s.substring(0, s.length - 2)
  }

  def task_5: Map[String, Double] = products.mapValues(_ * 0.9)

  def task_6(l: List[Int]): List[Int] = l.map(_ + 1)

  def task_7(l: List[Double]): List[Double] = l.filter(-5 < _).filter(_ < 12).map(_.abs)

  def task_8(tuple: Tuple3[Int, Double, String]): Unit = println(tuple)

  def task_9(l: List[Double]): List[Double] = {
    if (l.isEmpty) l
    else if (l.head == 0) task_9(l.tail)
    else l.head :: task_9(l.tail)
  }

  def task_10(product: String) = {
    val price: Option[Int] = products.get(product.toLowerCase)
    println(price.getOrElse("There is no such product -> " + product))
    if (price.isDefined && price.get >= 1000)
      println(product + " is 10% off")
  }

}