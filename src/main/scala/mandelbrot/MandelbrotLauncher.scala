package mandelbrot

import java.awt.BorderLayout
import javax.swing.{JFrame, WindowConstants}

class MandelbrotLauncher {
  private val frame: JFrame = new JFrame("Mandelbrot Image")
  private val panel: MandelbrotImagePane = new MandelbrotImagePane(
    new MandelbrotImage(-0.75, 0, 3.5 / 1000)
  )

  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.add(panel, BorderLayout.CENTER)
  frame.setSize(800, 800)
  frame.setVisible(true)
}

object MandelbrotLauncher {
  def main(args: Array[String]): Unit = {
    new MandelbrotLauncher
  }
}
