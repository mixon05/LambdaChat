object Utils {
  def hash(s: String): String = java.util.Base64.getEncoder.encodeToString(s.getBytes)
  def newId(): String     = java.util.UUID.randomUUID().toString
}
