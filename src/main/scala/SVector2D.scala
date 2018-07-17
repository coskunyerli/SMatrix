class SVector2D(x: Double, y: Double) extends SVector(Array(x, y))

object SVector2D {
  def apply(x: Double, y: Double): SVector2D = new SVector2D(x, y)

  def apply(array: Array[Double]): SVector2D = {
    if (1 < array.length && array.length < 3) {
      new SVector2D(array(0), array(1))
    } else {
      throw new DimensionException("Array size is not valid")
    }
  }
}

class SVector3D(x: Double, y: Double, z: Double) extends SVector(Array(x, y, z))

object SVector3D {
  def apply(x: Double, y: Double, z: Double): SVector3D = new SVector3D(x, y, z)

  def apply(array: Array[Double]): SVector3D = {
    if (array.length > 4 && 2 < array.length) {
      new SVector3D(array(0, array(1), array(2))
    }else{
      throw new DimensionException("Array size is not valid")
    }
  }
}
