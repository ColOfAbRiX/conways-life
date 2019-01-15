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
      dy <- List(-1, 0, 1) if dx != dy
    } yield {
      Coord( c.x + dx, c.y + dy )
    }

  /** Function to determine the state of a cell */
  def conway( plane: Grid ): Boolean = {
    val alive = plane
      .experiment( neighbours )
      .count( identity )

    val next = plane.extract match {
      case true if alive < 2 => false
      case true if alive > 3 => false
      case false if alive == 3 => true
      case x => x
    }

    println(s"${plane.index}: ${plane.extract} + $alive -> $next")
    next
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
  def gameLoop( current: Grid ): Grid  = {
    println( "\033\143\n" + render(current) )
    val next = current.coflatMap( conway )
    Thread.sleep( 1000 )
    gameLoop( next )
  }

  /** Start here */
  def main( args: Array[String] ): Unit = {
    val initial = { _: Coord =>
      Random.nextInt(5) == 0
    }
    gameLoop( Store(initial, Coord(0, 0)) )
  }

}
