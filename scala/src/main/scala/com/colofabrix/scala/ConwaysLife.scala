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

  /** Coordinates in the grid */
  case class Coord( x: Int, y: Int )

  /** Type of the grid of the game */
  type Grid = Store[Coord, Boolean]

  /** Prints the grid */
  def render( plane: Grid ): String = {
    // The coordinates corresponding to the whole grid
    def wholeGrid: List[Coord] = for {
      x <- (0 until gridSize.x).toList
      y <- (0 until gridSize.y).toList
    } yield {
      Coord(x, y)
    }

    // Scan the grid, transform cells into charactes and group them
    plane.experiment( _ => wholeGrid )
      .map( cell => if( cell ) " x" else " ." )
      .grouped( gridSize.x )
      .map( _.mkString )
      .mkString( "\n" )
  }

  /** Function to determine the state of a cell */
  def conway( grid: Grid ): Boolean = {
    // The coordinates corresponding to the neighbours of a cell
    def neighbours( c: Coord ): List[Coord] = for {
      dx <- List(-1, 0, 1)
      dy <- List(-1, 0, 1) if dx != 0 || dy != 0
    } yield {
      Coord( c.x + dx, c.y + dy )
    }

    // Count the neighbours of the current cell
    val alive = grid
      .experiment( neighbours )
      .count( identity )

    // Perform the Conway's calculation for the next status
    grid.extract match {
      case true if alive < 2 || alive > 3 => false
      case false if alive == 3 => true
      case x => x
    }
  }

  /** The main game loop */
  @tailrec
  def gameLoop( current: Grid ): Grid  = {
    // Clean the screen and print
    println( "\033\143" )
    println( render(current) )
    Thread.sleep( 1000 )

    // Apply the transformation to the whole grid and loop
    gameLoop(
      current.coflatMap( conway )
    )
  }

  /** Access an element in a grid wrapping around the edges */
  def accessGrid( grid: List[List[Boolean]] )( c: Coord ): Boolean = {
    def wrap(m: Int, n: Int) = (m + n % -m) % m
    val x = wrap( grid.length, c.x )
    grid( x )( wrap(grid(x).length, c.y) )
  }

  /** Returns a grid filled randomly */
  val randomGrid: List[List[Boolean]] = List.fill(gridSize.x, gridSize.y) {
    Random.nextInt(5) == 0
  }

  /** Start here */
  def main( args: Array[String] ): Unit = gameLoop(
    Store( accessGrid(randomGrid), Coord(0, 0) )
  )

}
