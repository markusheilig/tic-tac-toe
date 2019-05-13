import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object InputReader {

  @tailrec
  def read[T](inputProvider: () => T,
              inputValidator: T => Boolean,
              onInputError: Any => Unit): T = {
    Try(inputProvider()) match {
      case Success(value) if inputValidator(value) =>
        value
      case Success(value) =>
        onInputError(value)
        read(inputProvider, inputValidator, onInputError)
      case Failure(exception) =>
        onInputError(exception)
        read(inputProvider, inputValidator, onInputError)
    }
  }

  def readChoice[T](inputProvider: () => T,
                    choices: Set[T],
                    onInputError: Any => Unit): T = {
    read[T](inputProvider, choices.contains, onInputError)
  }

  def readBoolean(inputProvider: () => Char,
                  onInputError: Any => Unit): Boolean = {
    readChoice(inputProvider, Set('y', 'Y', 'n', 'N'), onInputError).toLower == 'y'
  }


}
