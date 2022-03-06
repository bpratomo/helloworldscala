package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double]
  ): Signal[Double] =
    Signal {
      val aval = a()
      val bval = b()
      val cval = c()

      bval * bval - 4 * aval * cval
    }

  def computeSolutions(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double],
      delta: Signal[Double]
  ): Signal[Set[Double]] =
    Signal {
      val bval = b()
      val aval = a()
      val dval = delta()
      if dval < 0 then Set()
      else
        Set(
          (-bval + math.sqrt(dval)) / (2 * aval),
          (-bval - math.sqrt(dval)) / (2 * aval)
        )
    }
