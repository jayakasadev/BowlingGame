import scala.annotation.tailrec
import scala.util.control.TailCalls.{TailRec, done, tailcall}

/**
  * Class to Score a 10 pin Bowling Game
  */
object GameScore{
  def main(args: Array[String]): Unit = {
    // will use this to run some code if I need to
  }
}

class GameScore {


  /**
    * Method to validate the input
    *
    * Will model this with an FSM using mutual recursion
    *
    * @param input
    * @return
    */
  def validate(input: String): Boolean = {

    // no helper method needed for spare
    //its a special case
    // gonne use more case statements and pattern matching instead of if loops for validation this time

    /**
      * Methods matches first char of X or a Strike
      *
      * @param input
      * @param count
      * @return
      */
    def X(input: List[Char], count: Int): TailRec[Boolean] = input match {
      case Nil => {
        // println("X Nil " + count)
        done(true)
      }
      case 'X' :: rest if(count == 9) => rest match {
        case x :: y :: Nil => x match {
          case 'X' => y match {
            case x if(x == 'X' | x.asDigit > 0 | x == '-') => done(true)
            case _ => done(false)
          }
          case z if(z == '-' | z.asDigit > 0) => y match {
            case x if(x == '/' | x.asDigit > 0 | x == '-') => done(true)
            case _ => done(false)
          }
          case _ => done(false)
        }
        case _ => done(false)
      }
      case 'X' :: rest => rest match {
        case Nil => {
          // println("in X Nil " + count)
          done(true)
        }
        case 'X' :: tail => {
          // println("X X " + count)
          tailcall(X(rest, count+1))
        }
        case '-' :: tail => {
          // println("X - " + count)
          tailcall(miss(rest, count+1))
        }
        case y :: tail if(y.asDigit > 0) => {
          // println("X " + y  + " " + count)
          tailcall(num(rest, count+1))
        }
        case _ => {
          // println(input + " " + count)
          done(false)
        }
      }
      case _ => {
        // println("weird match X " + count)
        done(false)
      }
    }

    /**
      * Methods matches first char of - or a Miss
      *
      * @param input
      * @param count
      * @return
      */
    def miss(input: List[Char], count: Int): TailRec[Boolean] = input match {
      case Nil => {
        // println("- Nil " + count)
        done(true)
      }
      case '-' :: rest => rest match {
        case Nil => {
          // println("in - Nil " + count)
          done(true)
        }
        case x :: tail if(x == '/' & count == 9) => tail match {
          case y :: Nil => y match {
            case z if(z == 'X' | z == '-' | z.asDigit > 0) => done(true)
            case _ => done(false)
          }
          case _ => done(false)
        }
        case x :: tail if(x == '-' | x == '/' | x.asDigit > 0) => tail match {
          case 'X' :: tail2 => {
            // println("- - or - / or - num followed by X " + count)
            tailcall(X(tail, count+1))
          }
          case '-' :: tail2 => {
            // println("- - or - / or - num followed by - " + count)
            tailcall(miss(tail, count+1))
          }
          case x :: tail2 if(x.asDigit > 0) => {
            // println("- - or - / or - num followed by num " + count)
            tailcall(num(tail, count+1))
          }
          case Nil => {
            // println("- - or - / or - num followed by Nil " + count)
            done(true)
          }
          case _ => {
            // println("- - or - / or - num followed by / " + count)
            done(false)
          }
        }
        case _ => {
          // println("miss X " + count)
          done(false)
        }
      }
      case _ => {
        // println("weird match miss " + count)
        done(false)
      }
    }

    /**
      * Methods matches first char which is a number
      *
      * @param input
      * @param count
      * @return
      */
    def num(input: List[Char], count: Int): TailRec[Boolean] = input match {
      case Nil => {
        // println("num Nil " + count)
        done(true)
      }
      case x :: rest if(x.asDigit > 0) => rest match {
        case Nil => {
          // println("in num Nil " + count)
          done(true)
        }
        case x :: tail if(x == '/' & count == 9) => tail match {
          case y :: Nil => y match {
            case z if(z == 'X' | z == '-' | z.asDigit > 0) => done(true)
            case _ => done(false)
          }
          case _ => done(false)
        }
        case x :: tail if(x == '-' | x == '/' | x.asDigit > 0) => tail match {
          case 'X' :: tail2 => {
            // println("num - or num / or num num followed by X " + count)
            tailcall(X(tail, count+1))
          }
          case '-' :: tail2 => {
            // println("num - or num / or num num followed by - " + count)
            tailcall(miss(tail, count+1))
          }
          case x :: tail2 if(x.asDigit > 0) => {
            // println("num - or num / or num num followed by num " + count)
            tailcall(num(tail, count+1))
          }
          case Nil => {
            // println("num - or num / or num num followed by Nil " + count)
            done(true)
          }
          case _ => {
            // println("num - or num / or num num followed by / " + count)
            done(false)
          }
        }
        case _ => {
          // println("num X " + count)
          done(false)
        }
      }
      case _ => {
        // println("weird match num " + count)
        done(false)
      }
    }

    if(input.length < 12 || input.length > 21) {
      // println("invalid size")
      false
    }
    else{
      val list = input.toList
      list match {
          // input can start in 3 states only: X, num or -
        case 'X' :: rest => {
          // println("X " + list)
          tailcall(X(list, 0)).result
        }
          // matches misses
        case '-' :: rest => {
          // println("- " + list)
          tailcall(miss(list, 0)).result
        }
          // matches numeric input greater than 0 since range is 0 - 9
        case x :: rest if(x.asDigit > 0) => {
          // println("num " + list)
          tailcall(num(list, 0)).result
        }
          // base case
        case _ => false
      }
    }
  }

  /**
    * Method to compute the score for an input String
    *
    * @param input game score string
    * @return score of game as int
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
      * Used if statements, will try to use more case statements next time
      *
      * Figured the best approach here is to use pattern matching and recursion
      *
      * @param input
      * @param score
      * @return
      */
    @tailrec
    def helper(input: List[Char], score: Int): Int = input match {
        // base case
      case Nil => score
        /*
          matching cases where I have a strike

          score of strike is current score + 10 + sum of value of next two rolls
         */
      case 'X' :: y :: z :: rest => {
        // println("X " + y + " " + z)
        // reached the end
        if(rest == Nil){
          score + 10 + valueReference(y) + valueReference(z)
        }
          // followed by a spare
        else if(z == '/'){
          helper(y :: z :: rest, score + 20)
        }
          // all other cases
        else{
          helper(y :: z :: rest, score + 10 + valueReference(y) + valueReference(z))
        }
      }
        // player got a spare
      case x :: '/' :: z :: rest => {
        // println(x + " / " + z + " " + (score + 10 + valueReference(z)))
        // reached the end
        if(rest == Nil){
          score + 10 + valueReference(z)
        }
          // all other cases
        else{
          helper(z :: rest, score + 10 + valueReference(z))
        }

      }
        // not a special case, may be miss or may be a number
        // no input validation done here
      case x :: rest => {
        // println(x + " " + (score + valueReference(x)))
        helper(rest, score + valueReference(x))
      }
    }

    // turning the input string into a list of char to make it easier to iterate over
    helper(input.toList, 0)
  }

}
