package tk.monnef.mcmapper

import java.io.File

object Utils {

  implicit final class ForwardPipe[T](val x: T) extends AnyVal {
    def |>[B](f: (T) => B) = f(x)
  }

  implicit final class OrElseCrash[T](val x: Option[T]) extends AnyVal {
    def orElseCrash: T = x match {
      case None => throw new McMapperException
      case Some(a) => a
    }

    def orElseCrash(msg: String): T = x match {
      case None => throw new McMapperException(msg)
      case Some(a) => a
    }
  }

  def getCurrentPath() = new File(".").getAbsolutePath

}
