package mandelbrot

import java.awt.Color
import java.awt.Component
import java.awt.Toolkit
import java.awt.image.BufferedImage
import java.awt.Image
import java.util

import scala.annotation.tailrec

object MandelbrotImage {
  private val SCREEN_DIMENSION = Toolkit.getDefaultToolkit.getScreenSize
  private val BUFFER_WIDTH = SCREEN_DIMENSION.width
  private val BUFFER_HEIGHT = SCREEN_DIMENSION.height
  val MAX_ITERATION = 4000

  def compute(x0: Double, y0: Double): Int = {
    @tailrec
    def compute0(x: Double, y: Double, iteration: Int): Int = {
      if ((x * x + y * y) < 4 && iteration < MAX_ITERATION) {
        val (nX, nY) = (x * x - y * y + x0, 2 * x * y + y0)
        compute0(nX, nY, iteration + 1)
      } else iteration
    }
    compute0(x0, y0, 0)
  }
}

final class MandelbrotImage(var originX: Double, var originY: Double, var scale: Double) {
  private var image = new BufferedImage(
    MandelbrotImage.BUFFER_WIDTH,
    MandelbrotImage.BUFFER_HEIGHT,
    BufferedImage.TYPE_3BYTE_BGR)
  private var x0 = 0.0
  private var y0 = 0.0
  private val repaintListeners = new util.ArrayList[Component]

  zoomTo(originX, originY, scale)

  def zoomTo(originX: Double, originY: Double, scale: Double): Unit = {
    x0 = originX
    y0 = originY
    recalculateAndRepaint()
  }

  def panTo(originX: Double, originY: Double): Unit = {
    x0 = originX
    y0 = originY
    recalculateAndRepaint()
  }

  def scaleTo(scale: Double): Unit = {
    this.scale = scale
    recalculateAndRepaint()
  }

  def scaleBy(factor: Double): Unit = {
    scale *= factor
    recalculateAndRepaint()
  }

  private def getColor(value: Int) = Color.HSBtoRGB(
    (360F * value) / MandelbrotImage.MAX_ITERATION,
    1.0F,
    if (value >= MandelbrotImage.MAX_ITERATION) 0.0F else 1F)

  // Simple sequential calculation

  private def recalculate(): Unit = {
    var x = 0
    var y = 0
    while (y < MandelbrotImage.BUFFER_HEIGHT) {
      x = 0
      while (x < MandelbrotImage.BUFFER_WIDTH) {
        var value = MandelbrotImage.compute(getX(x), getY(y))
        var color = getColor(value)
        image.setRGB(x, y, color)
        x += 1
      }
      y += 1
    }
  }

  private def recalculateAndRepaint(): Unit = {
    val start = System.nanoTime()
    recalculate()
    val time = System.nanoTime() - start
    println(f"Calculation time ${time/1000000000.0}%7.3f seconds")
    notifyListeners()
  }

  def getImage: Image = image

  def getX(x: Int): Double = x0 + ((x - (MandelbrotImage.BUFFER_WIDTH / 2)) * scale)

  def getY(y: Int): Double = y0 - ((y - (MandelbrotImage.BUFFER_HEIGHT / 2)) * scale)

  def addRepaintListener(obs: Component): Unit = {
    repaintListeners.add(obs)
  }

  private def notifyListeners(): Unit =
    repaintListeners.forEach( _ repaint ())
}
