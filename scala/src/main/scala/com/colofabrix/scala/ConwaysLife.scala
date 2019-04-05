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
FITNESS FOR A PARTICULAR PURPOSE AND NON INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package com.colofabrix.scala

import java.io.File

import cats.implicits._
import cats.data.Reader

import scala.annotation.tailrec
import scala.util.Random


object ConwaysLife {

  /**
    * Configuration
    */
  case class Config(
    width: Int = 20,
    height: Int = 20,
    aliveSymbol: String = "#",
    deadSymbol: String = ".",
    delay: Double = 0.5,
    filling: Int = 5,
    loadFile: Option[File] = None
  )

  /**
    * Coordinates in the grid
    */
  case class Coord( x: Int, y: Int )

  /**
    * Type of the grid of the game
    */
  type Grid = Store[Coord, Boolean]
  type MaterialGrid = List[List[Boolean]]

  /**
    * Options parser for scopt
    */
  private val parser = new scopt.OptionParser[Config]("conway") {
    import math._

    head("Conway's Game of Life", "1.0.0")

    // Width of the field
    opt[Int]('w', "width")
      .action( (w, c) => c.copy(width = w) )
      .text( "Width of the field" )

    // Height of the field
    opt[Int]('h', "height")
      .action( (h, c) => c.copy(height = h) )
      .text( "Height of the field" )

    // Delay in ms between generations
    opt[Double]('d', "delay")
      .action( (d, c) => c.copy(delay = d) )
      .text( "Delay in ms between generations" )

    // The symbol to display for an alive cell
    opt[String]("alive-symbol")
      .action( (s, c) => c.copy(aliveSymbol = s) )
      .text( "The symbol to display for an alive cell" )

    // The symbol to display for a dead cell
    opt[String]("dead-symbol")
      .action( (s, c) => c.copy(deadSymbol = s) )
      .text( "The symbol to display for a dead cell" )

    // Percentage of alive cell when filling the field randomly
    opt[Double]("filling")
      .action( (f, c) => c.copy( filling = max(ceil(1.0 / f).toInt - 1, 0)) )
      .text( "Percentage of alive cell when filling the field randomly" )

    // Optional field file to load at startup
    arg[File]("<field>")
      .action( (f, c) => c.copy(loadFile = Some(f)) )
      .text("Optional field file to load at startup")
      .optional()
  }

  /**
    * Start here
    */
  def main( args: Array[String] ): Unit = {
    parser.parse(args, Config()) match {
      case None =>
      case Some( config ) =>
        // Create a grid from either a file or randomly
        val grid = config.loadFile match {
          case None => randomGrid
          case Some(_) => fileGrid
        }

        // Define how to access the grid and start
        val accessor = accessGrid( grid.run(config) )( _ )
        gameLoop( config )( Store( accessor, Coord(0, 0) ) )
    }
  }

  /**
    * Access an element in a grid wrapping around the edges
    */
  def accessGrid( grid: MaterialGrid )( c: Coord ): Boolean = {
    def wrap(m: Int, n: Int) = (m + n % -m) % m
    val x = wrap( grid.length, c.x )
    grid( x )( wrap(grid(x).length, c.y) )
  }

  /**
    * Returns a grid filled randomly
    * TODO: Implement it for real
    */
  def fileGrid: Reader[Config, MaterialGrid] = randomGrid

  /**
    * Returns a grid loaded from a file
    */
  def randomGrid: Reader[Config, MaterialGrid] = Reader { c: Config =>
    List.fill( c.width, c.height ) {
      Random.nextInt( c.filling ) == 0
    }
  }

  /**
    * The main game loop
    */
  @tailrec
  def gameLoop( config: Config )( current: Grid ): Grid  = {
    // Clean the screen and print
    println( "\033\143" )
    println( render( config )( current ) )
    Thread.sleep( (config.delay * 1000).toLong )

    // Apply the transformation to the whole grid and loop
    gameLoop( config )( current.coflatMap( conway(config) ) )
  }

  /**
    * Prints the grid
    */
  def render( config: Config )( plane: Grid ): String = {
    // The coordinates corresponding to the whole grid
    def wholeGrid: List[Coord] = for {
      x <- (0 until config.width).toList
      y <- (0 until config.height).toList
    } yield {
      Coord(x, y)
    }

    // Scan the grid, transform cells into characters and group them
    val alive = " " + config.aliveSymbol
    val dead = " " + config.deadSymbol
    plane.experiment( _ => wholeGrid )
      .map( cell => if( cell ) alive else dead )
      .grouped( config.width )
      .map( _.mkString )
      .mkString( "\n" )
  }

  /**
    * Function to determine the state of a cell
    */
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
}
