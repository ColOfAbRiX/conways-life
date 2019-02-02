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

  /** Coordinates in the grid */
  case class Coord( x: Int, y: Int )

  /** Type of the grid of the game */
  type Grid = Store[Coord, Boolean]

  /** Configuration */
  case class Config(
    width: Int = 20,
    height: Int = 20,
    aliveSymbol: String = "#",
    deadSymbol: String = ".",
    delay: Double = 0.5,
    filling: Int = 5,
    fieldFile: Option[String] = None
  )

  /** Prints the grid */
  def render( config: Config )( plane: Grid ): String = {
    // The coordinates corresponding to the whole grid
    def wholeGrid: List[Coord] = for {
      x <- (0 until config.width).toList
      y <- (0 until config.height).toList
    } yield {
      Coord(x, y)
    }

    // Scan the grid, transform cells into charactes and group them
    val alive = " " + config.aliveSymbol
    val dead = " " + config.deadSymbol
    plane.experiment( _ => wholeGrid )
      .map( cell => if( cell ) alive else dead )
      .grouped( config.width )
      .map( _.mkString )
      .mkString( "\n" )
  }

  /** Function to determine the state of a cell */
  def conway( config: Config )( grid: Grid ): Boolean = {
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
  def gameLoop( config: Config )( current: Grid ): Grid  = {
    // Clean the screen and print
    //println( "\033\143" )
    println( render( config )( current ) )
    Thread.sleep( (config.delay * 1000).toLong )

    System.exit(0)

    // Apply the transformation to the whole grid and loop
    gameLoop( config )( current.coflatMap( conway(config) ) )
  }

  /** Access an element in a grid wrapping around the edges */
  def accessGrid( grid: List[List[Boolean]] )( c: Coord ): Boolean = {
    def wrap(m: Int, n: Int) = (m + n % -m) % m
    val x = wrap( grid.length, c.x )
    grid( x )( wrap(grid(x).length, c.y) )
  }

  /** Returns a grid filled randomly */
  def randomGrid( config: Config ): List[List[Boolean]] =
    List.fill(config.width, config.height) {
      Random.nextInt( config.filling ) == 0
    }

  /** Start here */
  def main( args: Array[String] ): Unit = {
    // Reading configuration from command line
    val parser = new scopt.OptionParser[Config]("conway") {
      head("Conway's Game of Life", "1.0.0")

      opt[Int]("width")
        .action((width, c) => c.copy(width = width))
      opt[Int]("height")
        .action((height, c) => c.copy(height = height))
      opt[Double]("delay")
        .action((delay, c) => c.copy(delay = delay))
      opt[String]("alive-symbol")
        .action((symbol, c) => c.copy(aliveSymbol = symbol))
      opt[String]("dead-symbol")
        .action((symbol, c) => c.copy(deadSymbol = symbol))
      opt[Double]("filling")
        .action((filling, c) => c.copy(
          filling = Math.max(Math.ceil(1.0 / filling).toInt - 1, 0))
        )
    }

    parser.parse(args, Config()) match {
      case None =>
      case Some( config ) =>
        val accessor = accessGrid( randomGrid(config) )( _ )
        gameLoop( config )( Store( accessor, Coord(0, 0) ) )
    }
  }

}
