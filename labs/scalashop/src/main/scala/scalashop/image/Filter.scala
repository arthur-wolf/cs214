package scalashop.image

import scalashop.common.*

/** Identity filter, does not change pixels of the source image.
  */
class Identity(src: Image) extends Image(src.height, src.width):
  def apply(x: Int, y: Int): Pixel =
    src(x, y)

/** Black and white filter, transforms the source image in a grayscale one.
  */
class BlackAndWhite(src: Image) extends Image(src.height, src.width):
  // we generate a weighted grayscale image
  // to do this, we compute the "Luma" of each pixel
  // these numbers come from a standard called Rec 601
  // and are computed based on how we perceive colour and brightness
  // see: https://en.wikipedia.org/wiki/Luma_(video)
  val lumaR = 0.299f
  val lumaG = 0.587f
  val lumaB = 0.114f
  def grayscale(input: Pixel) =
    val r = red(input).toFloat
    val g = green(input).toFloat
    val b = blue(input).toFloat
    val gray = (r * lumaR + g * lumaG + b * lumaB).toInt
    argb(alpha(input), gray, gray, gray)

  def apply(x: Int, y: Int): Pixel = grayscale(src(x, y))

class RedSplash(src: Image) extends BlackAndWhite(src):
  def isRedEnough(px: Pixel) =
    val r = red(px).toFloat
    val g = green(px).toFloat
    val b = blue(px).toFloat
    (r/g > 1.7) && (r/b > 1.7)

  override def apply(x: Int, y: Int): Pixel = 
    if isRedEnough(src(x, y)) then src(x, y) else grayscale(src(x, y))

/** Performs a simple box-blur of given radius by averaging over a pixel's
  * neighbours
  *
  * @param src
  *   source image
  */
class SimpleBlur(src: Image) extends Image(src.height, src.width):
  val radius: Int = 3

  def apply(x: Int, y: Int): Pixel =
    val xMin = math.max(0, x - radius)
    val xMax = math.min(src.width - 1, x + radius)
    val yMin = math.max(0, y - radius)
    val yMax = math.min(src.height - 1, y + radius)

    var r = 0
    var g = 0
    var b = 0
    var a = 0

    for (i <- xMin to xMax) {
      for (j <- yMin to yMax) {
        val px = src(i, j)
        r += red(px)
        g += green(px)
        b += blue(px)
        a += alpha(px)
      }
    }

    argb(a / ((xMax - xMin + 1) * (yMax - yMin + 1)), r / ((xMax - xMin + 1) * (yMax - yMin + 1)), g / ((xMax - xMin + 1) * (yMax - yMin + 1)), b / ((xMax - xMin + 1) * (yMax - yMin + 1)))

/** Produce the convolution of an image with a kernel
  *
  * @param src
  *   source image
  * @param kernel
  *   kernel to convolve with
  */
class Convolution(src: Image, kernel: Kernel) extends Image(src.height, src.width):
  def apply(x: Int, y: Int): Pixel =
    val xMin = math.max(0, x - kernel.width / 2)
    val xMax = math.min(src.width - 1, x + kernel.width / 2)
    val yMin = math.max(0, y - kernel.height / 2)
    val yMax = math.min(src.height - 1, y + kernel.height / 2)

    var r = 0
    var g = 0
    var b = 0
    var a = 0

    for (i <- xMin to xMax) {
      for (j <- yMin to yMax) {
        val px = src(i, j)
        val k = kernel(i - x + kernel.width / 2, j - y + kernel.height / 2)
        r += (red(px) * k).toInt
        g += (green(px) * k).toInt
        b += (blue(px) * k).toInt
        a += (alpha(px) * k).toInt
      }
    }

    argb(a, r, g, b)

/** Blur filter, computes a convolution between the image and the given blurring
  * kernel.
  */
class Blur(src: Image, kernel: Kernel) extends Image(src.height, src.width):
  private val convolution = Convolution(
    src,
    kernel.map(_ / kernel.sum)
  ) // for blurring, kernels are normalized to have sum = 1
  def apply(x: Int, y: Int): Pixel = convolution(x, y)

/** Box blur filter, blur filter with matrix of size `(radius * 2 + 1) x (radius
  * * 2 + 1)` filled with ones.
  */
class BoxBlur(src: Image, radius: Int) extends Blur(src, Kernel.uniform(radius * 2 + 1))

/** Gaussian blur filter, blurs with a 3x3 Gaussian kernel.
  */
class GaussianBlur(src: Image) extends Blur(src, Kernel.gaussian3x3)

/** Sobel edge detection filter, used to detect the horizontal and vertical
  * edges of an image. Take a look at `Kernel.sobelX` and `Kernel.sobelY` for
  * default kernels for this filter.
  */
class SobelEdgeDetection(src: Image, kernelX: Kernel, kernelY: Kernel)
    extends Image(src.height, src.width):
  require((kernelX.width, kernelX.height) == (kernelY.width, kernelY.height))

  val bwSrc = BlackAndWhite(src)
  val xConvo = Convolution(bwSrc, kernelX)
  val yConvo = Convolution(bwSrc, kernelY)

  def apply(x: Int, y: Int): Pixel =
    // Keep only 1 channel as they're all the same (black and white)
    val xVal = red(xConvo(x, y))
    val c = red(yConvo(x, y))
    val grayScale = math.sqrt(xVal * xVal + xVal * xVal).toInt

    argb(255, grayScale, grayScale, grayScale)
