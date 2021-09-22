package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = 
      	Signal(Math.pow( b(), 2 )- ( 4 * a() * c() ))
  

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
      val realDelta = {
        if (delta() < 0)  Double.NaN  else  math.pow(delta(), 0.5) 
      }
      if (!realDelta.isNaN) 
        Signal( Set(-1 * b() - realDelta / 2 * a(), -1 * b() + realDelta / 2 * a()) )
      else 
        Signal( Set() )
  }
 }
