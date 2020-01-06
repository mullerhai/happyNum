package com.core.happy

import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

object HappyNum {

  def computeSquareSum(num: Int): Int = {
    var sum: Int = 0
    var step = num
    do {
      sum += (step % 10) * (step % 10)
      step = step / 10
    }
    while (step != 0)
    sum
  }

  def judgeHappyNum(num: Int): Boolean = {
    var slow: Int = num
    var fast: Int = num
    var flag: Boolean = false
    breakable {
      do {
        slow = computeSquareSum(slow)
        fast = computeSquareSum(computeSquareSum(fast))
        if (slow == fast && slow == 1) {
          flag = true
          break()
        }
      } while (fast != slow)
    }
    flag
  }

  def wrapperJudgeHappyGetArray(option: Option[Int]): ArrayBuffer[Int] = {

    val num = option.getOrElse(0)
    val seq: ArrayBuffer[Int] = new ArrayBuffer[Int]()
    if (num > 0) {
      val flag = judgeHappyNum(num)
      if (flag) {
        seq.+=(num)
      } else {
        seq.+=(0)
      }
    } else {
      for (i <- 1 to 1000) {
        if (judgeHappyNum(i)) {
          seq.+=(i)
        }
      }
      seq
    }
  }

  def main(args: Array[String]): Unit = {
    val testArray: Seq[Option[Int]] = List(Some(29), Some(19), None)
    for (numCase <- testArray) {
      val resultArray = wrapperJudgeHappyGetArray(numCase)
      val result = resultArray(0)
      if (result == 0 && resultArray.size == 1) {
        println("unhappy num")
      } else if (numCase.getOrElse() == result && resultArray.size == 1) {
        println("happy num")
      } else {
        println(resultArray)
      }

      //      Output:
      //      unhappy num
      //      happy num
      //      ArrayBuffer(1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97, 100, 103, 109, 129, 130, 133, 139, 167, 176, 188, 190, 192, 193, 203, 208, 219, 226, 230, 236, 239, 262, 263, 280, 291, 293, 301, 302, 310, 313, 319, 320, 326, 329, 331, 338, 356, 362, 365, 367, 368, 376, 379, 383, 386, 391, 392, 397, 404, 409, 440, 446, 464, 469, 478, 487, 490, 496, 536, 556, 563, 565, 566, 608, 617, 622, 623, 632, 635, 637, 638, 644, 649, 653, 655, 656, 665, 671, 673, 680, 683, 694, 700, 709, 716, 736, 739, 748, 761, 763, 784, 790, 793, 802, 806, 818, 820, 833, 836, 847, 860, 863, 874, 881, 888, 899, 901, 904, 907, 910, 912, 913, 921, 923, 931, 932, 937, 940, 946, 964, 970, 973, 989, 998, 1000)

    }

  }
}
