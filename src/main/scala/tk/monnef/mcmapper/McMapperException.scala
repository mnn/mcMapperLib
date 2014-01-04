package tk.monnef.mcmapper

class McMapperException(msg: String, cause: Throwable) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)

  def this() = this("", null)

  def this(cause: Throwable) = this("", cause)
}
