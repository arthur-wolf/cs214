package boids

import scala.collection.parallel.CollectionConverters.VectorIsParallelizable
import scala.util.Random
import cs214.Vector2
import org.openjdk.jmh.annotations.Benchmark
import boids.Benchmarks.generateRandomBoids
import boids.Benchmarks.defaultPhysics

/** Supporting elements for benchmarks
  */
object Benchmarks:

  val seed = 0x0ddba11
  Random.setSeed(seed)

  case class Limits(
      xMin: Float,
      xMax: Float,
      yMin: Float,
      yMax: Float,
      vxMin: Float,
      vxMax: Float,
      vyMin: Float,
      vyMax: Float
  )

  // generation parameters
  val defaultLimits = Limits(
    xMin = -200f,
    xMax = +200f,
    yMin = -200f,
    yMax = +200f,
    vxMin = -5f,
    vxMax = +5f,
    vyMin = -5f,
    vyMax = +5f
  )

  val defaultPhysics = Physics(
    minimumSpeed = 2f,
    maximumSpeed = 5f,
    perceptionRadius = 80f,
    avoidanceRadius = 15f,
    avoidanceWeight = 1f,
    cohesionWeight = 0.001f,
    alignmentWeight = 0.027f,
    containmentWeight = 0.5f
  )

  def generateRandomBoid(lim: Limits): Boid =
    Boid(
      position = Vector2(
        x = Random.between(lim.xMin, lim.xMax),
        y = Random.between(lim.yMin, lim.yMax)
      ),
      velocity = Vector2(
        x = Random.between(lim.vxMin, lim.vxMax),
        y = Random.between(lim.vyMin, lim.vyMax)
      )
    )

  def generateRandomBoids(size: Int): Vector[Boid] =
    require(size > 0)
    Vector.tabulate(size)(_ => generateRandomBoid(defaultLimits))

end Benchmarks

/** Class for adding JMH benchmarks
  */
class Benchmarks:
  @Benchmark
  def tickWorldComplete1K =
    val allBoids = generateRandomBoids(1000)
    // UNCOMMENT THIS WHEN YOU HAVE CONVERTED EVERYTHING TO VECTORS
    tickWorld(allBoids, defaultPhysics)

end Benchmarks
