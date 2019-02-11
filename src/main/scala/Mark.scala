
sealed trait Mark {
  def opponent: Mark
}

case object X extends Mark {
  override def toString: String = "x"

  override def opponent: Mark = O
}

case object O extends Mark {
  override def toString: String = "o"

  override def opponent: Mark = X
}

case object Empty extends Mark {
  override def toString: String = " "

  override def opponent: Nothing = throw new Error("Empty.opponent")
}



