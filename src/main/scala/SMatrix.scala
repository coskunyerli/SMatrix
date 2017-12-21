case class SMatrix(array: Array[SVector]) {
  def *(vec: SVector): SVector = ???

  def *(s: Double): SMatrix = ???

  def *(mat: SMatrix): SMatrix = ???

  def +(mat: SMatrix): SMatrix = ???

  def -(mat: SMatrix): SMatrix = ???

  def inv() = ???

  def det() = ???

  def trans() = ???

}

object SMatrix {
  def crossMatrix(vec: SVector): SMatrix = ???

  def random(s: Size): SMatrix = ???

  def random(x: Int, y: Int): SMatrix = random(Size(x, y))

  def zeros(s: Size): SMatrix = ???

  def zeros(x: Int, y: Int): SMatrix = zeros(Size(x, y))

  def ones(s: Size): SMatrix = ???

  def ones(x: Int, y: Int): SMatrix = ones(Size(x, y))

  def identity(s: Int): SMatrix = ???


}




