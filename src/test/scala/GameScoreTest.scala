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

  val invalid = "X7/"
  val invalid2 = "X7/9-X-88/-6XXX8"
  val invalid3 = "X7/9-X-88/-6XX/8"
  val invalid4 = "/7/9-X-88/-6XX/8"
  val invalid5 = "/7/9-X-88/-6XX/8/7/9-X-88/-6XX/8"
  val invalid6 = "0/7/9-X-88/-6XX/"

  val scorer = new GameScore

  "A GameScore input of XXXXXXXXXXXX" should "be valid and output a value of 300" in {
    assert(scorer.validate(strikes))
    assert(scorer.score(strikes) == 300)
  }

  "A GameScore input of 9-9-9-9-9-9-9-9-9-9-" should "be valid and output a value of 90" in {
    assert(scorer.validate(nines))
    assert(scorer.score(nines) == 90)
  }

  "A GameScore input of 5/5/5/5/5/5/5/5/5/5/5" should "be valid and output a value of 150" in {
    assert(scorer.validate(spares))
    assert(scorer.score(spares) == 150)
  }

  "A GameScore input of --------------------" should "be valid and output a value of 0" in {
    assert(scorer.validate(misses))
    assert(scorer.score(misses) == 0)
  }

  "A GameScore input of X7/9-X-88/-6XXX81" should "be valid and output a value of 167" in {
    assert(scorer.validate(random))
    assert(scorer.score(random) == 167)
  }

  "A GameScore input of X7/9-X-88/-6X-/8" should "be valid and output a value of 128" in {
    assert(scorer.validate(random2))
    assert(scorer.score(random2) == 128)
  }

  "A GameScore input of X7/" should "not be valid" in {
    assert(!scorer.validate(invalid))
  }

  "A GameScore input of X7/9-X-88/-6XXX8" should "not be valid" in {
    assert(!scorer.validate(invalid2))
  }

  "A GameScore input of X7/9-X-88/-6XX/8" should "not be valid" in {
    assert(!scorer.validate(invalid3))
  }

  "A GameScore input of /7/9-X-88/-6XX/8" should "not be valid" in {
    assert(!scorer.validate(invalid4))
  }

  "A GameScore input of /7/9-X-88/-6XX/8/7/9-X-88/-6XX/8" should "not be valid" in {
    assert(!scorer.validate(invalid5))
  }
}
