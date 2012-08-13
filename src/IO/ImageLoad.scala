package com.jamesrthompson.IO

import java.io.{FileInputStream, File}
import java.util.ArrayList
import actors.Actor

/**
 * ImageLoad object - loads 8 bit or 16 bit stacked TIFFs and returns an Array[Array[Short]] object
 * Author: James R. Thompson, D.Phil
 * Date: 7/16/12
 * Time: 9:31 AM
 * USC Mork Family Dept. of Chem. Eng. & Mat. Sci.
 */

object ImageLoad extends Actor {

  def load(file: File): Option[(Array[Array[Short]], Int, Int)] = {
    start()
    val out = this !? file
    out match {
      case a: (Array[Array[Short]], Int, Int) => Some(a)
      case _ => None
    }
  }

  override def act() = {
    react {
      case file: File => {
        println("Loading ...")
        val startTime = System.currentTimeMillis()
        reply(doLoad(file))
        println((System.currentTimeMillis() - startTime).toString + " ms")
        println("Finished loading multi-page TIFF!")
        exit()
      }
      case _ => exit()
    }
  }

  private def doLoad(file: File): (Array[Array[Short]], Int, Int) = {
    val td = new TiffDecoder(file)
    val info: Option[ArrayList[FileInfo]] = Option(td.getTiffInfo)
    val imgOpt = fileCheck(info, file)
    (imgOpt.get, info.get.get(0).width, info.get.get(0).height)
  }

  private def fileCheck(opt: Option[ArrayList[FileInfo]], file: File): Option[Array[Array[Short]]] = opt match {
    case ok: Some[ArrayList[FileInfo]] => Some(loadImage(ok.get.get(0), file))
    case _ => None
  }

  private def loadImage(fileData: FileInfo, file: File) = {
    val is = new FileInputStream(file)
    val reader = new Reader(fileData)
    var skip = fileData.getOffset
    val out = for (i <- 0 until fileData.nImages) yield {
      val pixels = reader.readPixels(is, skip)
      skip = fileData.gapBetweenImages
      matchBitDepth(pixels)
    }
    is.close()
    out.toArray
  }

  private def matchBitDepth(obj: java.lang.Object): Array[Short] = obj match {
    case b: Array[Byte] => b.map(_ toShort)
    case s: Array[Short] => s
  }

}