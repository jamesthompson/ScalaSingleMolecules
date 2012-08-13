package com.jamesrthompson.Spots

/**
 * Spot class
 * Author: James R. Thompson, D.Phil
 * Date: 7/16/12
 * Time: 9:31 AM
 * USC Mork Family Dept. of Chem. Eng. & Mat. Sci.
 */

class Spot(val loc:(Double,Double), val frame:Int, var notTracked:Boolean = true) {

  def distanceTo(that:Spot) = math.sqrt(sqr(this.loc._1 - that.loc._1) + sqr(this.loc._2 - that.loc._2))

  def sqr(d:Double) = d * d

  def gotTracked : Unit = this.notTracked = false

  def getX = loc._1
  def getY = loc._2

  def fancyString = "Frame : " + frame.toString + ": x = " + loc._1.toString + "\ty = " + loc._2.toString + "\tnot tracked = " + notTracked.toString

  override def toString = loc._1.toString + "\t" + loc._2.toString
}