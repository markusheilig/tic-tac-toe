
sealed trait Mark {
}

case object X extends Mark {
  override def toString: String = "x"
}

case object O extends Mark {
  override def toString: String = "o"
}

case object Empty extends Mark {
  override def toString: String = " "
}



