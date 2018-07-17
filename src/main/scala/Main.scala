import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Main extends App {
  val a = SMatrix.random(3, 3)
  //val b = SVector.random(-1)
  //println(SMatrix.test)


  println(a)
  println(a.row(0))
  //println(b)
  //println(a * b)
  //println(b*a)
  val a: Array[Double] = new Array[Int](2)
  println(SMatrix.<<(3, 3)(1, 2, 3, 4, 5, 6, 7, 8, 9).T)
}
