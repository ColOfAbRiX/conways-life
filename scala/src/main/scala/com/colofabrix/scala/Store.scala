package com.colofabrix.scala

import cats._
import cats.implicits._

case class Store[S, A]( lookup: S => A, index: S ) {
  lazy val extract: A =
    lookup( index )

  def duplicate: Store[S, Store[S, A]] =
    Store( s => Store( lookup, s ), index )

  def map[B]( f: A => B ): Store[S, B] =
    Store( Store.memoize( lookup.andThen(f) ), index )

  def coflatMap[B]( f: Store[S, A] => B ): Store[S, B] =
    duplicate.map( f )

  def experiment[F[_]: Functor]( f: S => F[S] ): F[A] = {
    f( index ).map( lookup )
  }
}

object Store {

  import scala.collection.mutable

  // The memoization here works by 1) being a store of functions, 2) storing the
  // result of a function with its argument as key in the HashMap and 3) by
  // overriding the apply method so that it effectively becomes another S => A
  // function for who calls it.
  def memoize[S, A]( f: S => A ): S => A = new mutable.HashMap[S, A]() {
    override def apply( index: S ): A = getOrElseUpdate( index, f(index) )
  }

}
