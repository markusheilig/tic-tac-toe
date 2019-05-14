import org.scalatest.{FlatSpec, Matchers}

class InputReaderTest extends FlatSpec with Matchers {

  "An InputReader" should "read until the input satisfies a certain condition" in {

    // increment myInt and return the new value on each function call
    var myInt = 0
    val incrementAndGet = () => {
      myInt += 1
      myInt
    }

    val input = InputReader.read[Int](incrementAndGet, _ >= 3, _ => {})
    input shouldBe myInt
  }

  it should "call an error function when the input provider throws an error" in {
    val numberOfErrors = 2
    var myInt = 0
    val incrementAndProvide = () => {
      if (myInt < numberOfErrors) {
        myInt += 1
        throw new RuntimeException()
      }
      myInt
    }

    var errorCounter = 0
    InputReader.read[Int](incrementAndProvide, _ => true, _ => errorCounter += 1)
    errorCounter shouldBe numberOfErrors
  }

  it should "read a choice" in {
    InputReader.readChoice[Integer](() => 2, Set(1, 2, 3), _ => {}) shouldBe 2
  }

  it should "read a boolean choice" in {
    InputReader.readBoolean(() => 'y', _ => {}) shouldBe true
    InputReader.readBoolean(() => 'Y', _ => {}) shouldBe true
    InputReader.readBoolean(() => 'n', _ => {}) shouldBe false
    InputReader.readBoolean(() => 'N', _ => {}) shouldBe false
  }

}
