package com.jamesrthompson.CLEAN

import com.jamesrthompson.Spots.{Spot, Track}

/**
 * Cleaner object - a utility object to perform cleaning operations and handle data
 * Author: James R. Thompson, D.Phil
 * Date: 7/16/12
 * Time: 9:31 AM
 * USC Mork Family Dept. of Chem. Eng. & Mat. Sci.
 */

object Cleaner {

  def cleanFrame(imgData:Array[Short], width:Int, height:Int, patchSize:Int, threshold:Double) = {
    if (threshold >= 1.0) new Clean(imgData, width, height, patchSize).findSpots()._2 else new Clean(imgData, width, height, patchSize).findSpots(threshold)._2
  }

  def cleanAllFrames(imgData:Array[Array[Short]], width:Int, height:Int, patchSize:Int, minFrames:Int, maxRange:Double, threshold:Double) : Array[Array[Spot]] = {
    if(threshold >= 1.0) {
      val output : IndexedSeq[Array[(Double, Double)]] = for(slice<-imgData) yield new Clean(slice, width, height, patchSize).findSpots()._2  // Supplying no argument as I want automatic mean * 2 thresholding here...
      val track = new Track(output, maxRange, minFrames)
      val tracksOut = track.doTracking
      tracksOut.map(_.toArray).toArray
    } else {
      val output : IndexedSeq[Array[(Double, Double)]] = for(slice<-imgData) yield new Clean(slice, width, height, patchSize).findSpots(threshold)._2  // Supplying no argument as I want automatic mean * 2 thresholding here...
      val track = new Track(output, maxRange, minFrames)
      val tracksOut = track.doTracking
      tracksOut.map(_.toArray).toArray
    }
  }

}
