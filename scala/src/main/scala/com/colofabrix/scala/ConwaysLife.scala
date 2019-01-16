/*
MIT License

Copyright (c) 2019 Fabrizio Colonna <colofabrix@tin.it>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package com.colofabrix.scala

import cats.implicits._
import cats.data.Store

import scala.annotation.tailrec
import scala.util.Random

object ConwaysLife {

  val gridSize = Coord( 6, 6 )

  /** Coordinates in the field */
  case class Coord( x: Int, y: Int )

  /** Type of the field of the game */
  type Grid = Store[Coord, Boolean]

  /** Find the coordinates of the 8 neighbours on an infinite plane */
  def neighbours( c: Coord ): List[Coord] =
    for {
      dx <- List(-1, 0, 1)
      dy <- List(-1, 0, 1)
      if dx != 0 || dy != 0
    } yield {
      Coord( c.x + dx, c.y + dy )
    }

  /** Function to determine the state of a cell */
  def conway( plane: Grid ): Boolean = {
    val alive = plane
      .experiment( neighbours )
      .count( identity )

    plane.extract match {
      case true if alive < 2 => false
      case true if alive > 3 => false
      case false if alive == 3 => true
      case x => x
    }
  }

  /** Prints the field */
  def render( plane: Grid ): String = {
    val coords: List[Coord] = for {
      x <- (0 until gridSize.x).toList
      y <- (0 until gridSize.y).toList
    } yield {
      Coord(x, y)
    }

    plane.experiment( _ => coords )
      .map( cell => if( cell ) " x" else " ." )
      .grouped( gridSize.x )
      .map( _.mkString )
      .mkString( "\n" )
  }

  /** The main game loop */
  @tailrec
  def gameLoop( current: Grid ): Grid  = {
    println( "\033\143\n" + render(current) )
    Thread.sleep( 1000 )
    gameLoop( current.coflatMap( conway ) )
  }

  /** Start here */
  def main( args: Array[String] ): Unit = {
    val grid = List.tabulate(gridSize.x, gridSize.y) { (x, y) =>
      Random.nextInt(5) == 0
    }
    def accessGrid( c: Coord ) = {
      val x = (grid.length + c.x % -grid.length) % grid.length
      val y = (grid(x).length + c.y % -grid(x).length) % grid(x).length
      grid(x)(y)
    }
    gameLoop( Store(accessGrid, Coord(0, 0)) )
  }

}
