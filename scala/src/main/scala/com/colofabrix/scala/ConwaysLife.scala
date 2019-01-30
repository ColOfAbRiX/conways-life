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

import scala.annotation.tailrec
import scala.util.Random

object ConwaysLife {

  /** Configuration */
  val gridSize = Coord( 10, 10 )

  /** Coordinates in the field */
  case class Coord( x: Int, y: Int )

  /** Type of the field of the game */
  type Grid = Store[Coord, Boolean]

  /** Prints the field */
  def render( plane: Grid ): String = {
    plane.experiment( _ => scan )
      .map( cell => if( cell ) " x" else " ." )
      .grouped( gridSize.x )
      .map( _.mkString )
      .mkString( "\n" )
  }

  /** Find the coordinates of all the grid */
  val scan: List[Coord] =
    for {
      x <- (0 until gridSize.x).toList
      y <- (0 until gridSize.y).toList
    } yield {
      Coord(x, y)
    }

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

  /** The main game loop */
  @tailrec
  def gameLoop( current: Grid ): Grid  = {
    println( "\033\143" )
    println( render(current) )

    Thread.sleep( 1000 )
    gameLoop(
      current.coflatMap( conway )
    )
  }

  /** Start here */
  def main( args: Array[String] ): Unit = {
    val grid = List.fill(gridSize.x, gridSize.y) {
      Random.nextInt(5) == 0
    }

    def wrap(n: Int): Int =
      (grid.length + n % -grid.length) % grid.length

    def accessGrid( c: Coord ) =
      grid( wrap(c.x) )( wrap(c.y) )

    gameLoop( Store(accessGrid, Coord(0, 0)) )
  }

}
