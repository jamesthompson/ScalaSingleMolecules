package com.jamesrthompson.CLEAN

import collection.mutable.ArrayBuffer

/**
 * Clean class implemented in Scala
 * Author: James R. Thompson, D.Phil
 * Date: 7/16/12
 * Time: 9:31 AM
 * USC Mork Family Dept. of Chem. Eng. & Mat. Sci.
 */

abstract class PixelType
case class EdgePixel(loc:(Int,Int), intensity:Double) extends PixelType
case class GotchaPixel(loc:(Int,Int), intensity:Double) extends PixelType
case class DimPixel(loc:(Int,Int), intensity:Double) extends PixelType

class Clean(img:Array[Short], width:Int, height:Int, val patchSize:Int) {

  def findSpots(threshold:Double = mean * 2) = {
    while(i.max > threshold) definePixels(i.max, threshold)
    (i, foundSpots.toArray)
  }

  implicit def conv2DTo1D(loc:(Int,Int)) : Int = loc._2 * width + loc._1

  implicit def conv1Dto2D(loc:Int) : (Int,Int) = (loc % width, math.floor(loc / width).toInt)

  implicit def normalize(in:Array[Short]) : Array[Double] = {
    val max = in.max.toDouble
    val min = in.min.toDouble
    in.map(s => (s.toDouble - min) * (1 / (max - min)))
  }

  lazy val i : Array[Double] = img

  lazy val mean = i.sum / i.length

  val foundSpots = ArrayBuffer[(Double,Double)]()

  def definePixels(brightest:Double, threshold:Double) = {
    val loc = i.indexOf(brightest)
    assignPixelType(loc, brightest, threshold) match {
      case e:EdgePixel => i.update(loc,mean)
      case g:GotchaPixel => foundSpots += cleanMe(loc)
      case d:DimPixel => ()
    }

    def assignPixelType(loc:(Int,Int), intensity:Double, threshold:Double) = {
      if(loc._1 <= patchSize || loc._2 <= patchSize || loc._1 >= width - patchSize || loc._2 >= height - patchSize) EdgePixel(loc, intensity)
      else if(intensity < threshold) DimPixel(loc, intensity)
      else GotchaPixel(loc, intensity)
    }

    def cleanMe(loc:(Int,Int)) = {
      val pixelMap = for(x <- loc._1 - patchSize to loc._1 + patchSize; y <- loc._2 - patchSize to loc._2 + patchSize) yield ((x,y),i.apply((x,y)))
      pixelMap.map(p => i.update(p._1, mean))
      def moment(in:IndexedSeq[((Int,Int), Double)]) = {
        val d = in.map(_._2).sum
        (in.map(v => v._1._1 * v._2).sum / d, in.map(v => v._1._2 * v._2).sum / d)
      }
      moment(pixelMap)
    }
  }
}