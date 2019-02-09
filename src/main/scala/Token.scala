
sealed trait Token {
}

case object X extends Token {
  override def toString: String = "x"
}

case object O extends Token {
  override def toString: String = "o"
}

case object Empty extends Token {
  override def toString: String = " "
}



