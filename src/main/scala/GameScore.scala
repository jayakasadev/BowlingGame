import scala.annotation.tailrec

/**
  * Created by kasa2 on 5/29/2017.
  */
object GameScore{
  def main(args: Array[String]): Unit = {
    // will use this to run some code if I need to
  }
}

class GameScore {

  /**
    * Method to compute the score for an input String
    *
    * @param input
    * @return
    */
  def score(input: String): Int = {
    // will need a helper function to look up points for symbols
    def valueReference(in: Char): Int = in match{
      case 'X' => 10
      case '/' => 10
      case '-' => 0
      // case x => x.toInt - 48
      case x => x.asDigit
    }

    /**
      * Helper Method to calculate the score.
      * Takes advantage of tailrecursion to compute score
      *
      * @param input
      * @param score
      * @return
      */
    @tailrec
    def helper(input: List[Char], score: Int): Int = input match {
      case Nil => score
      case _ => helper(Nil, score + 1)
    }

    helper(input.toList, 0)
  }

}
