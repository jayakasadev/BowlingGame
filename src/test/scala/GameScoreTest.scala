import org.scalatest.FlatSpec

/**
  * Created by kasa2 on 5/29/2017.
  */
class GameScoreTest extends FlatSpec{

  // these values denote valid input
  // will use these to get the score initially
  val strikes = "XXXXXXXXXXXX"
  val nines = "9-9-9-9-9-9-9-9-9-9-"
  val spares = "5/5/5/5/5/5/5/5/5/5/5"
  val random = "X7/9-X-88/-6XXX81"
  val random2 = "X7/9-X-88/-6X-/8"
  val misses = "--------------------"

  val scorer = new GameScore

  "A GameScore input of XXXXXXXXXXXX" should "be valid and output a value of 300" in {
    // no validation code implemented yet
    // will implement if i have time
    assert(scorer.score(strikes) == 300)
  }

  "A GameScore input of 9-9-9-9-9-9-9-9-9-9-" should "be valid and output a value of 90" in {
    // no validation code implemented yet
    // will implement if i have time
    assert(scorer.score(nines) == 90)
  }

  "A GameScore input of 5/5/5/5/5/5/5/5/5/5/5" should "be valid and output a value of 150" in {
    // no validation code implemented yet
    // will implement if i have time
    assert(scorer.score(spares) == 150)
  }

  "A GameScore input of --------------------" should "be valid and output a value of 0" in {
    // no validation code implemented yet
    // will implement if i have time
    assert(scorer.score(misses) == 0)
  }

  "A GameScore input of X7/9-X-88/-6XXX81" should "be valid and output a value of 167" in {
    // no validation code implemented yet
    // will implement if i have time
    assert(scorer.score(random) == 167)
  }

  "A GameScore input of X7/9-X-88/-6X-/8" should "be valid and output a value of 128" in {
    // no validation code implemented yet
    // will implement if i have time
    assert(scorer.score(random2) == 128)
  }
}
