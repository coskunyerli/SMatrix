package utils.exceptions.svector

/**
  * Created by berkaycan on 21.12.2017.
  */

class InvalidDimensionException(message: String, cause: Throwable) extends RuntimeException(message, cause)

object InvalidDimensionException {

  def apply(message: String, cause: Throwable): InvalidDimensionException = new InvalidDimensionException(message, cause)

  def apply(message: String): InvalidDimensionException = this (message)
}
