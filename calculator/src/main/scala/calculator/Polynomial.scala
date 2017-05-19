package calculator

import scala.math

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var((b() * b()) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
        if (delta() < 0) Var(Set())
        else Var(Set((-b() + scala.math.sqrt(delta()))/(2*a()), (-b() - scala.math.sqrt(delta()))/(2*a())))
  }
}
