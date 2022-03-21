package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 1,
    Key.exec.maxWarmupRuns := 4,
    Key.exec.benchRuns := 15,
    Key.verbose := false
  ) withWarmer (Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    //val threshold = 10000
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing
    extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean =
    val res = chars.foldLeft(0)((acc, el) => {
      if acc < 0 then Int.MinValue
      else if el == '(' then acc + 1
      else if el == ')' then acc - 1
      else acc
    })

    res == 0

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if idx >= until then (arg1, arg2)
      else
        (arg1, arg2, chars(idx)) match {
          case (_, r, ')') if r > 0  => traverse(idx + 1, until, arg1, r - 1)
          case (_, r, ')') if r <= 0 => traverse(idx + 1, until, arg1 + 1, r)
          case (_, _, '(')           => traverse(idx + 1, until, arg1, arg2 + 1)
          case (_, _, _)             => traverse(idx + 1, until, arg1, arg2)

        }

    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val mid = ((until - from) / 2) + from

      //println(s"from: ${from}, until: ${until}, mid:${mid}")
      if until - from < threshold then traverse(from, until, 0, 0)
      else
        val (t1, t2) = parallel(reduce(from, mid), reduce(mid, until))

        if t1._2 - t2._1 > 0 then (t1._1, t2._2 + (t1._2 - t2._1))
        else (t1._1 - (t1._2 - t2._1), t2._2)
    }

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!
