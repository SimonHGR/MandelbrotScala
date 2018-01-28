package mandelbrot

import java.awt.BorderLayout
import javax.swing.JFrame


object MandelbrotLauncher {
  def main(args: Array[String]): Unit = {
    new MandelbrotLauncher().go()
  }
}

class MandelbrotLauncher {
  private var frame: JFrame = _
  private var panel: MandelbrotImagePane = _

  def go(): Unit = {
    frame = new JFrame("Mandelbrot Image")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val mi = new MandelbrotImage(-0.75, 0, 3.5 / 1000)
    panel = new MandelbrotImagePane(mi)
    frame.add(panel, BorderLayout.CENTER)
    frame.setSize(800, 800)
    frame.setVisible(true)
  }
}