package poly

case class Vector2(x: Double, y: Double)

def norm(v: Vector2): Double =
  Math.sqrt(v.x * v.x + v.y * v.y).toFloat

def OverallGrade(labScore: Double, midtermScore: Double, finalScore: Double): Double = ???
