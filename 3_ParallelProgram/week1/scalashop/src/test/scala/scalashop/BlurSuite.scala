package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite {
  val radius = 3
  val width = 1920
  val height = 1080
  val src = Img(width, height)
  val dst = Img(width, height)

  test("Box blur kernel in the middle should work") {
    val src1 = Img(width, height)
    src1.update(3, 4, 5)
    assertEquals(src1(3, 4), 5)
  }
  test("Box blur kernel in the edges shoud work") {
    val src2 = Img(width, height)
    src2.update(width - 1, height - 1, 5)
    assertEquals(src2(width - 1, height - 1), 5)
  }

  test("BoxBlurKernel(_,_,0) should be identity") {
    val src3 = Img(width, height)
    src3.update(3, 4, 5)
    assertEquals(src3(3, 4), boxBlurKernel(src3, 3, 4, 0))

  }

  test("BoxBlurKernel should correctly blur a 3x4 image") {
    val src4 = Img(3, 4)
    for
      x <- (0 to 2)
      y <- (0 to 3)
    yield src4.update(x, y, x)

    println(boxBlurKernel(src4, 2, 3, 2))
    println(boxBlurKernel(src4, 2, 3, 2))

  }

  test("Blurring of 3 by 4 image via Horizontal Blur") {}

}
