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
}
