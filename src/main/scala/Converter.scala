import scala.annotation.tailrec

object Converter extends App {

  def convert(numeral: String): Int = {

    val numList = numeral.map {
      case 'I' => 1
      case 'V' => 5
      case 'X' => 10
      case 'L' => 50
      case 'C' => 100
      case 'D' => 500
      case 'M' => 1000
      case _ => throw new IllegalArgumentException("Invalid character in input. Valid characters are I,V,X,L,C,D,M")
    }

    @tailrec
    def getPrevious(current: Int, numList: List[Int]): Option[Int] = numList match  {
      case Nil               => None
      case x :: `current` :: _ => Some(x)
      case _ :: xs           => getPrevious(current, xs)
    }

    numList.foldLeft(0)((acc, num) => {

      val prev = getPrevious(num, numList.toList).getOrElse(0)
      if (prev < num) acc - prev + (num - prev)
      else acc + num
    })
  }
}
