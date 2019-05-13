import org.scalatest.{FlatSpec, Matchers}

class InputReaderTest extends FlatSpec with Matchers {

  "A Reader" should "read until a certain condition is fulfilled" in {
    var myInt = 0
    val incrementAndProvide = () => {
      myInt += 1
      myInt
    }

    var errorCounter = 0
    InputReader.read[Int](incrementAndProvide, _ > 3, _ => errorCounter += 1)
    errorCounter shouldBe 3
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
