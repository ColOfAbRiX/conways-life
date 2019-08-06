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

// TODO: Use the cats representable Store comonad instead of a custom one

package com.colofabrix.scala

import java.io.File

import cats.implicits._

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
    * Options parser for scopt
    */
  private val parser = new scopt.OptionParser[Config]("conway") {
    import math._

    head("Conway's Game of Life", "1.0.2")

    help("help")
      .text( "{p}rints this usage text" )

    // Width of the field
    opt[Int]('w', "width")
      .action( (w, c) => c.copy(width = w) )
      .text( s"Width of the field. Default: ${Config().width}" )

    // Height of the field
    opt[Int]('h', "height")
      .action( (h, c) => c.copy(height = h) )
      .text( s"Height of the field. Default: ${Config().height}" )

    // Delay in ms between generations
    opt[Double]('d', "delay")
      .action( (d, c) => c.copy(delay = d) )
      .text( s"Delay in ms between generations. Default: ${Config().delay}" )

    // The symbol to display for an alive cell
    opt[String]("alive-symbol")
      .action( (s, c) => c.copy(aliveSymbol = s) )
      .text( s"The symbol to display for an alive cell. Default: ${Config().aliveSymbol}" )

    // The symbol to display for a dead cell
    opt[String]("dead-symbol")
      .action( (s, c) => c.copy(deadSymbol = s) )
      .text( s"The symbol to display for a dead cell. Default: ${Config().deadSymbol}" )

    // Percentage of alive cell when filling the field randomly
    opt[Double]("filling")
      .action( (f, c) => c.copy( filling = max(ceil(1.0 / f).toInt - 1, 0)) )
      .text( s"Percentage of alive cell when filling the field randomly" )

    // Optional field file to load at startup
    arg[File]("<field>")
      .action( (f, c) => c.copy(loadFile = Some(f)) )
      .text( s"Optional field file to load at startup. Default: ${Config().loadFile}" )
      .optional()
  }


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
    * Start here
    */
  def main( args: Array[String] ): Unit = {
    parser.parse( args, Config() ) match {
      case None           =>  // Managed by scopt
      case Some( config ) =>
        implicit val implicitConfig = config

        // Create a grid from either a file or randomly
        val materialGrid = config.loadFile match {
          case None    => randomGrid
          case Some(_) => gridFromFile
        }

        // Define an accessor to the grid, "how to access the grid"
        val accessor = getGridAccessor( materialGrid )

        // Start at coord 0, 0 and... Go!
        val gridStore = Store( accessor, Coord(0, 0) )
        gameLoop( gridStore )
    }
  }

  /**
    * Access an element in a grid wrapping around the edges
    * NOTE: the wrapping is a change in original game of life which has an
    * infinite grid
    */
  def getGridAccessor( grid: MaterialGrid ): Coord => Boolean = { c =>
    def wrap(m: Int, n: Int) = (m + n % -m) % m
    val x = wrap( grid.length, c.x )
    grid( x )( wrap(grid(x).length, c.y) )
  }

  /**
    * Returns a grid loaded from a file
    */
  def gridFromFile( implicit config: Config ): MaterialGrid = {
    io.Source.fromFile(config.loadFile.get)
      .getLines()
      .map { line =>
        line.replaceAll("""\s+""", "")
          .split("")
          .toList.map { symbol =>
            symbol == config.aliveSymbol
          }
      }.toList
  }

  /**
    * Returns a grid filled randomly
    */
  def randomGrid( implicit config: Config ): MaterialGrid = {
    List.fill( config.width, config.height ) {
      Random.nextInt( config.filling ) == 0
    }
  }

  /**
    * The main game loop
    */
  @tailrec
  def gameLoop( current: Grid )( implicit config: Config ): Grid  = {
    // Clean the screen and print
    println( "\033\143" )
    println( render(current) )
    Thread.sleep( (config.delay * 1000).toLong )

    // Apply the transformation to the whole grid and loop
    gameLoop( current.coflatMap(conway) )
  }

  /**
    * Renders and prints the grid
    */
  def render( plane: Grid )( implicit config: Config ): String = {
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
    *
    * This is the core of the comonadic job, it's a local function that works
    * on a Store focused on the current element and its neighbours
    */
  def conway( grid: Grid )( implicit config: Config ): Boolean = {
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
      case true  if alive < 2 || alive > 3 => false
      case false if alive == 3             => true
      case x                               => x
    }
  }
}
