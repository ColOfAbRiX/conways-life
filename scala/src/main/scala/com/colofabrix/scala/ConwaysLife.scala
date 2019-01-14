package com.colofabrix.scala

import cats._
import cats.implicits._
import cats.data.Store

import scala.util.Random

object ConwaysLife {

  // Coordinates in the field
  case class Coord( x: Int, y: Int )

  // Type of the field of the game
  type Grid = Store[Coord, Boolean]

  // Prints the field
  def render( plane: Grid ): String = {
    val extent = 10

    val coords: List[Coord] = (for {
      x <- 0 until extent
      y <- 0 until extent
    } yield Coord(x, y)).toList

    def cellString(value: Boolean): String =
      if (value) " X " else " . "

    val cells = plane.experiment[List] {
      _ => coords
    } map cellString

    cells.grouped( extent )
      .map(_.mkString)
      .mkString("\n")
  }

  // Find the coordinates of the 8 neighbours on an infinite plane
  def neighbours( c: Coord ): List[Coord] =
    for {
      dx <- List(-1, 0, 1)
      dy <- List(-1, 0, 1)
      if dx != dy
    } yield {
      Coord( dx, dy )
    }

  // Function to determine the state of a cell
  def conway( plane: Grid ): Boolean = true

  // A single step in the simulation
  def gameLoop( current: Grid ): Grid  = {
    val rendered = render( current )
    println( "\033\143" )
    println( rendered )

    Thread.sleep(1000)

    gameLoop(
      current.coflatMap( conway )
    )
  }

  // Start here
  def main( args: Array[String] ): Unit = {
    val initial = Store(
      { _: Coord => Random.nextInt(10) == 0 }, Coord(0, 0)
    )
    gameLoop( initial )
  }

}
