package com.jamesrthompson.Spots

import collection.mutable.ArrayBuffer
import annotation.tailrec

/**
 * Track class
 * Author: James R. Thompson, D.Phil
 * Date: 7/16/12
 * Time: 9:31 AM
 * USC Mork Family Dept. of Chem. Eng. & Mat. Sci.
 */

class Track(inputSpots:IndexedSeq[Array[(Double, Double)]], rangeThreshold:Double, minNumFrames:Int) {

  lazy val spots : IndexedSeq[Spot] = for(frame<-inputSpots;spot<-frame) yield new Spot(spot,inputSpots.indexOf(frame))

  def printSpots = println(spots.map(_.fancyString).mkString("\n"))

  def doTracking = spots.map(spot => trackSpot(spot, spots, ArrayBuffer[Spot](spot))).filter(_.length >= minNumFrames)

  @tailrec private def trackSpot(checkSpot:Spot, spotList:IndexedSeq[Spot], track:ArrayBuffer[Spot]) : ArrayBuffer[Spot] = {
    checkSpot.frame < spotList.map(_.frame).max && checkSpot.notTracked match {
      case true => {
        val nextLinkedSpot = spotList.filter(_.frame == checkSpot.frame + 1).sortWith(_.distanceTo(checkSpot) < _.distanceTo(checkSpot)).head
        if(nextLinkedSpot.distanceTo(checkSpot) < rangeThreshold) {
          track += nextLinkedSpot
          trackSpot(nextLinkedSpot, spotList, track)
        } else {
          track.map(_.gotTracked)
          track
        }
      }
      case false => {
        track.map(_.gotTracked)
        track
      }
    }
  }

  @tailrec final def msd(track:Array[Spot], dt:Double, dist:Double, msdVsT:ArrayBuffer[(Double,Double)], deltaT:Int = 0) : Array[(Double,Double)] = {
  deltaT < track.length match {
    case true => {
      def sqr(in:Double) = in * in
      val distances = for(i <- 0 to track.length - deltaT) yield sqr(track.apply(i).distanceTo(track.apply(i + deltaT)))
      val weight = 1 - (deltaT / (track.length - 1))
      val timePoint = dt * deltaT
      val msdVal = (distances.sum * sqr(dist)) / ((track.length - deltaT).toDouble * weight)
      val newPoint = (timePoint, msdVal)
      msdVsT += newPoint
      msd(track, dt, dist, msdVsT, deltaT + 1)
    }
    case false => msdVsT.toArray
  }
 }


}