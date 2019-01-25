package com.colofabrix.scala

import cats._
import cats.implicits._

case class Store[S, A]( peek: S => A, index: S ) {
  def seek( s: S ) =
    duplicate.peek( s )

  def extract: A =
    peek( index )

  def duplicate: Store[S, Store[S, A]] =
    coflatMap(x => x)

  def map[B]( f: A => B ): Store[S, B] =
    coflatMap( _ => f(extract) )

  def coflatMap[B]( f: Store[S, A] => B ): Store[S, B] =
    Store( s => f(Store(peek, s)), index )

  def experiment[F[_]: Functor]( f: S => F[S] ): F[A] = {
    f( index ).map( peek )
  }
}
