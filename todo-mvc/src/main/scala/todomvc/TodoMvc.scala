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

package todomvc

import shapeless3.deriving.K0

type IO[A] = Nothing

def children[E]: Children[IO, E] = ???

final class Children[F[_], E]:
  def <--(cs: Any): Children.Modified[F, E] = ???

object Children:
  final class Modified[F[_], E]

object TodoMvc:

  def render =
    HtmlTag("", false)(
      ???,
      children <-- ???
    )

trait Modifier[F[_], E, A]

final class HtmlTag[F[_], E] (name: String, void: Boolean):

  def apply[M <: Tuple](modifiers: M)(
      using K0.ProductInstances[Modifier[F, E, _], M]) = ???


      