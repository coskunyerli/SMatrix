import scala.util.Random

case class SVector private(private val array: Array[Double]) {

  def +(vec: SVector): SVector = {
    val arr = (for (i <- vec.array.indices) yield {
      array(i) + vec.array(i)
    }).toArray
    SVector(arr)
  }

  def dot(vec: SVector): Double = {
    this.array.indices.foldLeft(0.0)((sum, i) => sum + vec.array(i) * this.array(i))
  }

  def cos(vec: SVector): Double = {
    this.dot(vec) / (this.length * vec.length)
  }

  def angle(vec: SVector): Double = Math.acos(this.cos(vec))

  def cross(vec: SVector): SVector = ???

  def dim: Int = this.array.length

  def *(s: Double): SVector = {
    SVector(this.array.map(_ * s))
  }

  def /(s: Double): SVector = {
    this * (1.0 / s)
  }

  def norm: Double = {
    Math.sqrt(this.dot(this))
  }

  def length: Double = norm

  def unit: SVector = {
    this / this.length
  }

  def normalize: SVector = this.unit


  override def toString: String = {
    new StringBuilder("[ ").append(this.array.mkString(", ")).append(" ]").toString()
  }
}

object SVector {
  def zeros(size: Int): SVector = { // TODO negative or zero dimention exception
    SVector(Array.fill[Double](size)(0.0))
  }

  def ones(size: Int): SVector = { // TODO negative or zero dimention exception
    SVector(Array.fill[Double](size)(1.0))
  }

  def random(size: Int): SVector = { // TODO negative or zero dimention exception
    SVector(Array.fill[Double](size)(Random.nextDouble))
  }

  def random: SVector = {
    val size: Int = Random.nextInt(10)
    random(size)
  }

  def unitRandom(size: Int): SVector = {
    random(size).normalize
  }
}
