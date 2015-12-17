import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.util.{Failure, Success, Try}

case class Colour(red: Int, green: Int, blue: Int)

//this is a hastily written collection of functions to demonstrate an average blur over an image
//the image is written to disk to out.png
//note: when the pixel size does not divide the width or height there will be blank (black pixels) for the remainder
object Main {

  def loadImage(path: String): Try[BufferedImage] = {
    try {
      Success(ImageIO.read(new File(path)))
    }
    catch {
      case e: Throwable => Failure(e)
    }
  }

  def toRGBMatrix(bufferedImage: BufferedImage): Seq[Int] = {
    for {
      y <- 0 until bufferedImage.getHeight
      x <- 0 until bufferedImage.getWidth
    } yield {
      bufferedImage.getRGB(x, y)
    }
  }

  def toColourMatrix(intSeq: Seq[Int], fn: Color => Colour ): Seq[Colour] = {
    intSeq.map(i => fn(new Color(i)))
  }

  def imgToColourMatrix: (BufferedImage, Color => Colour) => Seq[Colour] = (img, fn) => {
    toColourMatrix(toRGBMatrix(img), fn)
    //toColourMatrix _ compose toRGBMatrix
  }

  def getXYFunc [T] (seq: Seq[T], width: Int): ((Int, Int)) => T = {
    (tup: (Int, Int)) => seq(tup._1 + tup._2*width)
  }

  def miniGridPos (x: Int, y: Int, w: Int, l: Int): Seq[(Int, Int)] = {
    for {
      j <- 0 until l
      i <- 0 until w
    } yield {
      (x+i, y+j)
    }
  }

  def miniGridColour[T](x: Int, y: Int, w: Int, l: Int, f: ((Int, Int)) => T): Seq[T] = {
    miniGridPos(x, y, w, l).map(f)
  }

  def avgSequence(s: Seq[Colour]): Colour = {
    val (r, g, b) = s.foldLeft(0, 0, 0) {
      case ((r, g, b), curr) => (r + curr.red, g + curr.green, b + curr.blue)
    }
    Colour(r/s.length, g/s.length, b/s.length)
  }

  def partitionGrid(width: Int, height: Int, xStepSize: Int, yStepSize: Int): Seq[Seq[(Int, Int)]] = {
    val numHori = Math.ceil((width / xStepSize)).toInt
    val numVert = Math.ceil((height / yStepSize)).toInt
    (for {
      y <- 0 to numVert
      x <- 0 to numHori
    } yield {
      val (xAnchor, yAnchor) = (x * xStepSize, y * yStepSize)
      val fil = miniGridPos(xAnchor, yAnchor, xStepSize, yStepSize)

      fil.filter { case (_x, _y) => {
        _x < width && _y < height
      }}

    }).filter(_ != Nil)
  }

  def avgGrid(img: BufferedImage, xStepSize: Int, yStepSize: Int, fn: Color => Colour) = {
    val colourMatrix = imgToColourMatrix(img, fn)
    val getColourAt = getXYFunc(colourMatrix, img.getWidth)
    partitionGrid(img.getWidth, img.getHeight, xStepSize, yStepSize).map { pos =>
      avgSequence(pos.map(getColourAt))
    }
  }

  def writeImage (cs: Seq[Colour], path: String, width: Int, height: Int, stepSizeX: Int, stepSizeY: Int) = {
    val numBlocksPerRow = Math.ceil(width.toFloat/stepSizeX).toInt
    val numBlocksPerCol = Math.ceil(height.toFloat/stepSizeY).toInt
    val img = new BufferedImage(numBlocksPerRow*stepSizeX, numBlocksPerCol*stepSizeY, BufferedImage.TYPE_3BYTE_BGR)
//    println(numBlocksPerRow)
    for ((e, i) <- cs.view.zipWithIndex) {
      val c = new Color(e.red, e.green, e.blue)
      val xAnchor = i % numBlocksPerRow
      val yAnchor = i / numBlocksPerRow

      for (k <- 0 until stepSizeY; j <- 0 until stepSizeX) {
        val trueX = xAnchor*stepSizeX + j
        val trueY = yAnchor*stepSizeY + k
        if (trueX < width && trueY < height) {
          img.setRGB(trueX, trueY, c.getRGB)
        }
      }
    }
    ImageIO.write(img, "png",  new File(path))
  }

  def main(args: Array[String]) {
    //do nothing
  }

  def test (path: String, w: Int, h: Int, fn: Color => Colour): Unit = {
    val img = Main.loadImage(path)
    println(img.get.getWidth)
    println(img.get.getHeight)
    val a = Main.avgGrid(img.get, w, h, fn)
    Main.writeImage(a, "out.jpg", img.get.getWidth, img.get.getHeight, w, h)

  }

  def defaultTransform(color: Color) = {
    Colour(color.getRed, color.getGreen, color.getBlue)
  }
}
