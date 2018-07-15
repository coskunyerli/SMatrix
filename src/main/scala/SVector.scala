import scala.util.Random

class SVector(private val array: Array[Double]) {

  def apply(i: Int): Double = this.array(i)

  def +(vec: SVector): SVector = {
    val array = new Array[Double](this.array.length)
    var i = 0
    while (i < this.array.length) {
      array(i) = this.array(i) + vec.array(i)
      i += 1
    }
    SVector(array)
  }

  def -(vec: SVector): SVector = {
    val array = new Array[Double](this.array.length)
    var i = 0
    while (i < this.array.length) {
      array(i) = this.array(i) - vec.array(i)
      i += 1
    }
    SVector(array)
  }

  def dot(vec: SVector): Double = this.array.indices.foldLeft(0.0)((sum, i) => sum + vec.array(i) * this.array(i))

  def cos(vec: SVector): Double = this.dot(vec) / (this.length * vec.length)

  def angle(vec: SVector): Double = Math.acos(this.cos(vec))

  def cross(vec: SVector): SVector = SMatrix.crossMatrix(vec) * this

  def dim: Int = this.array.length

  def *(s: Double): SVector = SVector(this.array.map(_ * s))

  def *:(vec: SVector) = SVector(this.array.zip(vec.array).map(tuple => tuple._1 * tuple._2))

  def ^(s: Double): SVector = SVector(this.array.map(n => Math.pow(n, s)))

  def *(mat: SMatrix): SVector = {
    if (mat.cols != this.length) {
      throw new Exception("Matrix size is not equal with vector")
    }
    var zeros = SVector.zeros(this.length)
    mat.vecs.zip(this.array).foreach { tuple =>
      zeros = zeros + tuple._1 * tuple._2
    }
    zeros
  }

  def /(s: Double): SVector = {
    if (s == 0.0) {
      throw new Exception("Vector is not divided by zero")
    }
    this * (1.0 / s)
  }

  def norm: Double = Math.sqrt(this.dot(this))

  def length: Int = dim

  def unit: SVector = this / this.length

  def normalize: SVector = this.unit

  def map(f: (Double) => Double): SVector = SVector(this.array.map(f))

  def foreach(f: (Double) => Unit): Unit = this.array.foreach(f)

  def foldLeft[B](s: B)(f: (B, Double) => B): B = this.array.foldLeft(s)(f)

  def sum: Double = this.array.foldLeft(0.0)(_ + _)

  def isPerpendicular(vec: SVector): Boolean = (this dot vec) <= 0.0

  def toMatrix(h: Boolean = false): SMatrix = {
    if (h) {
      SMatrix(this.array.map(num => SVector(Array(num))))
    } else {
      SMatrix(Array(this))
    }
  }

  override def toString: String = new StringBuilder("[ ").append(this.array.mkString(", ")).append(" ]").toString()

  def data: Array[Double] = this.array

}

object SVector {

  def zeros(size: Int): SVector = {
    if (size <= 0) {
      throw new DimensionException("Vector can not be created because of size")
    }
    SVector(Array.fill[Double](size)(0.0))
  }

  def ones(size: Int): SVector = {
    if (size <= 0) {
      throw new DimensionException("Vector can not be created because of size")
    }
    SVector(Array.fill[Double](size)(1.0))
  }

  def random(size: Int): SVector = {
    if (size <= 0) {
      throw new DimensionException("Vector can not be created because of size")
    }
    SVector(Array.fill[Double](size)(Random.nextDouble))
  }

  def unitRandom(size: Int): SVector = SVector.random(size).normalize

  def apply(array: Array[Double]): SVector = new SVector(array)
}
