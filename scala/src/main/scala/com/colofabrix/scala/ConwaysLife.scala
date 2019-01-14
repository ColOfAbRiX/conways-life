package com.colofabrix.scala

import cats.implicits._
import cats.data.Store

import scala.annotation.tailrec
import scala.util.Random

object ConwaysLife {

  val gridSize = Coord( 10, 10 )

  /** Coordinates in the field */
  case class Coord( x: Int, y: Int )

  /** Type of the field of the game */
  type Grid = Store[Coord, Boolean]

  /** Prints the field */
  def render( plane: Grid ): String = {
    val extent = 10

    val coords: List[Coord] = (for {
      x <- 0 until gridSize.x
      y <- 0 until gridSize.y
    } yield Coord(x, y)).toList

    def cellString(value: Boolean): String =
      if( value ) " X " else " . "

    val cells = plane.experiment[List]( _ => coords ) map cellString

    cells.grouped( extent )
      .map(_.mkString)
      .mkString("\n")
  }

  /** Find the coordinates of the 8 neighbours on an infinite plane */
  def neighbours( c: Coord ): List[Coord] =
    for {
      dx <- List(-1, 0, 1)
      dy <- List(-1, 0, 1)
      if dx != dy
    } yield {
      Coord( c.x + dx, c.y + dy )
    }

  /** Function to determine the state of a cell */
  def conway( plane: Grid ): Boolean = {
    var alive = plane
      .experiment(neighbours)
      .count(identity)
    println( s"${plane.index}: $alive" )
    false
  }

  /** The main game loop */
  @tailrec
  def gameLoop( current: Grid ): Grid  = {
    //println( "\033\143" + render(current) )
    Thread.sleep(1000)
    println(" LOOP ")
    val next = current.coflatMap( conway )
    //gameLoop( next )
    Store[Coord, Boolean](identity, Coord(0, 0))
  }

  /** Start here */
  def main( args: Array[String] ): Unit = {
    val initial = { _: Coord =>
      Random.nextInt(10) == 0
    }
    gameLoop( Store(initial, Coord(0, 0)) )
  }

}
