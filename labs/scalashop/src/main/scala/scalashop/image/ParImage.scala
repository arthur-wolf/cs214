package scalashop.image

import scalashop.common.*

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import scala.collection.parallel.ForkJoinTaskSupport

private def availableProcessors = sys.runtime.availableProcessors()

final class ParImage(
    src: Image,
    private var parallelization: Int = availableProcessors
) extends Image(src.height, src.width):

  // start buildSequential
  private def buildSequential(
      destination: ArrayImage,
      xFrom: Int,
      xTo: Int,
      yFrom: Int,
      yTo: Int
  ): Unit =
    for 
      x <- xFrom until xTo
      y <- yFrom until yTo 
    yield destination(x, y) = src(x, y)
    
  // end buildSequential

  // start build
  override def build: ArrayImage =
    // compute the collection to work on
    val splitWidth = math.max(1, width / parallelization)
    val splits: Seq[(Int, Int)] = (0 until width by splitWidth).map(x => (x, x + splitWidth))
    val parSplits = splits.par
    parSplits.tasksupport = ForkJoinTaskSupport(ForkJoinPool(parallelization)) // make sure we apply the desired level of parallelism

    val destination = ArrayImage(height, width)

    // perform your computation in parallel
    parSplits.foreach { 
      case (xFrom, xTo) =>
        buildSequential(destination, xFrom, xTo, 0, height)
    }

    // return the constructed image
    destination
  // end build

  def apply(x: Int, y: Int): Pixel = src(x, y)

  override def seq: Image = src.seq // recursively eliminate parallelization
  override def par: ParImage = par(availableProcessors)
  override def par(n: Int): ParImage =
    require(n >= 1)
    if n == parallelization then this
    else ParImage(src, n)
