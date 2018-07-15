class SMatrix(val vecs: Array[SVector]) {
  // This class is column major

  def apply(i: Int, j: Int): Double = this.vecs(i)(j)

  def *(vec: SVector): SVector = vec * this

  def *(s: Double): SMatrix = SMatrix(this.vecs.map(vec => vec * s))

  def /(s: Double): SMatrix = {
    if (s == 0.0) {
      throw new DimensionException("Matrix can not divided by zero")
    }
    this * (1.0 / s)
  }

  def ^(s: Double): SMatrix = SMatrix(this.vecs.map(vec => vec ^ s))

  def *(mat: SMatrix): SMatrix = {
    if (this.cols != mat.rows) {
      throw new DimensionException("Matrix dimentions is not equal")
    }
    SMatrix(mat.vecs.map(vec => vec * this))
  }

  def :*(mat: SMatrix): SMatrix = {
    if (this.size != mat.size) {
      throw new DimensionException("Matrix size is not equal")
    }
    SMatrix(mat.vecs.zip(this.vecs).map(tuple => tuple._1 *: tuple._2))
  }

  def :/(mat: SMatrix): SMatrix = this :* (mat ^ -1.0)

  def +(mat: SMatrix): SMatrix = {
    if (mat.size != this.size) {
      throw new DimensionException("Matrix size is not equal")
    }
    var i = 0
    val length = this.vecs.length
    val newArray = new Array[SVector](length)
    while (i < length) {
      newArray(i) = mat.vecs(i) + this.vecs(i)
      i += 1
    }
    SMatrix(newArray)
  }

  def -(mat: SMatrix): SMatrix = this + (mat * -1.0)

  def inv: SMatrix = {
    SMatrix(null)
  }

  def det: Int = {
    4
  }

  def transpose: SMatrix = {
    val vArray = new Array[SVector](this.rows)
    for (i <- Range(0, this.vecs(0).length)) {
      val array = new Array[Double](this.cols)
      for (j <- this.vecs.indices) {
        array(j) = this.apply(j, i)
      }
      vArray(i) = SVector(array)
    }
    SMatrix(vArray)
  }

  def size: Size = Size(this.rows, this.cols)

  def cols: Int = this.vecs.length

  def rows: Int = this.vecs(0).length

  def data: Array[Double] = this.vecs.flatMap(vec => vec.data)

  def col(index: Int): SVector = this.vecs(index)

  def row(index: Int): SVector = {
    SVector(this.vecs.map(vec => vec(index)))
  }

  def resize(row: Int, col: Int): SMatrix = {
    val length = this.cols * this.rows
    if (length != row * col || length % row != 0 || length % col != 0) {
      throw new DimensionException("Matrix size is not valid to resize for given parameters")
    }
    val data = this.data
    val vArray = new Array[SVector](col)
    var start = 0
    for (i <- Range(0, col)) {
      val newVector = data.slice(start, start + row)
      vArray(i) = SVector(newVector)
      start += row
    }
    SMatrix(vArray)
  }

  def reshape(row: Int, col: Int): SMatrix = this.resize(row, col)

  override def toString: String = {
    val text = new StringBuilder()
    text.append("[")
    var i = 0
    val length = this.vecs.length
    while (i < this.vecs(0).length) {
      var j = 0
      while (j < this.vecs.length) {
        val num = this.apply(j, i)
        text.append(s"$num, ")
        j += 1
      }
      text.delete(text.length - 2, text.length)
      text.append(s";\n")
      i += 1
    }
    text.delete(text.length - 2, text.length)
    text.append("]")
    text.toString()
  }

}

object SMatrix {
  def apply(vecs: Array[SVector]): SMatrix = new SMatrix(vecs)

  def crossMatrix(vec: SVector): SMatrix = ???

  def random(s: Size): SMatrix = random(s.x, s.y)

  def random(x: Int, y: Int): SMatrix = {
    if (x <= 0 || y <= 0) {
      throw new DimensionException("Matrix can not be created because of dimension")
    }
    val array = new Array[SVector](y).map(col => SVector.random(x))
    SMatrix(array)
  }

  def zeros(s: Size): SMatrix = zeros(s.x, s.y)

  def zeros(x: Int, y: Int): SMatrix = {
    if (x <= 0 || y <= 0) {
      throw new DimensionException("Matrix can not be created because of dimension")
    }
    var array = new Array[SVector](x).map(col => SVector.zeros(y))
    SMatrix(array)
  }

  def ones(s: Size): SMatrix = ones(s.x, s.y)

  def ones(x: Int, y: Int): SMatrix = {
    if (x <= 0 || y <= 0) {
      throw new DimensionException("Matrix can not be created because of dimension")
    }
    var array = new Array[SVector](x).map(col => SVector.ones(y))
    SMatrix(array)
  }

  def identity(s: Int): SMatrix = {
    val vArray = new Array[SVector](s)
    for (i <- Range(0, s)) {
      val array = Array.fill[Double](s)(0.0)
      array(i) = 1.0
      vArray(i) = SVector(array)
    }
    SMatrix(vArray)
  }

  def create(data: Double*): SMatrix = SMatrix(Array(SVector(data.toArray)))

  // def T(vec: SVector): SMatrix = ???

  //def R(eul: SVector): SMatrix = ???

  def test = SMatrix(Array(SVector(Array(1, 2, 3)), SVector(Array(1, 2, 3)), SVector(Array(1, 2, 3))))


}




