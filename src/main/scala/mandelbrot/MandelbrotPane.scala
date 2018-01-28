package mandelbrot

import java.awt.AWTEvent
import java.awt.Graphics
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import javax.swing.JPanel

final class MandelbrotImagePane(var mandelbrotImage: MandelbrotImage) extends JPanel with MouseListener with KeyListener {
  addMouseListener(this)
  enableEvents(AWTEvent.KEY_EVENT_MASK)
  addKeyListener(this)
  mandelbrotImage.addRepaintListener(this)

  override def paint(g: Graphics): Unit = {
    val windowWidth = getWidth
    val windowHeight = getHeight
    val bufferWidth = mandelbrotImage.getImage.getWidth(null)
    val bufferHeight = mandelbrotImage.getImage.getHeight(null)
    g.translate(-(bufferWidth - windowWidth) / 2, -(bufferHeight - windowHeight) / 2)
    g.drawImage(mandelbrotImage.getImage, 0, 0, this)
  }

  override def mouseClicked(e: MouseEvent): Unit = {
    val x = e.getX
    val y = e.getY
    val windowWidth = getWidth
    val windowHeight = getHeight
    val bufferWidth = mandelbrotImage.getImage.getWidth(null)
    val bufferHeight = mandelbrotImage.getImage.getHeight(null)
    val targetX = mandelbrotImage.getX(x + (bufferWidth - windowWidth) / 2)
    val targetY = mandelbrotImage.getY(y + (bufferHeight - windowHeight) / 2)
    mandelbrotImage.panTo(targetX, targetY)
    this.repaint()
  }

  override def isFocusable = true

  override def keyTyped(e: KeyEvent): Unit = {
    var changed = true
    e.getKeyChar match {
      case '+' =>
        mandelbrotImage.scaleBy(0.5)
      case '-' =>
        mandelbrotImage.scaleBy(2)
      case _ =>
        changed = false
    }
    if (changed) this.repaint()
  }

  override def mousePressed(e: MouseEvent): Unit = {}
  override def mouseReleased(e: MouseEvent): Unit = {}
  override def mouseEntered(e: MouseEvent): Unit = {}
  override def mouseExited(e: MouseEvent): Unit = {}
  override def keyPressed(e: KeyEvent): Unit = {}
  override def keyReleased(e: KeyEvent): Unit = {}
}