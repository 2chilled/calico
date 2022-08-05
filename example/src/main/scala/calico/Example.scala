/*
 * Copyright 2022 Arman Bilge
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package calico

import shapeless3.deriving.K0

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] = ???

trait IO[A]
trait Signal[F[_], A]

object Signal:
  given [F[_]]: Functor[Signal[F, _]] = ???

trait Modifier[F[_], E, A]
object Modifier:
  given [F[_], E]: Modifier[F, E, String] = ???
  given [F[_], E]: Modifier[F, E, Signal[F, Int]] = ???

def foo[F[_], E, M <: Tuple](modifiers: M)(
    using K0.ProductInstances[Modifier[F, E, _], M]): Unit = ???

@main def main =
  val n: Signal[IO, Int] = ???
  foo(
    "Count: ",
    n.map(_.toString),
  )