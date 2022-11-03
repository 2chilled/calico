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
package dsl

import calico.syntax.*
import calico.util.DomHotswap
import cats.Foldable
import cats.Hash
import cats.Monad
import cats.effect.IO
import cats.effect.kernel.Async
import cats.effect.kernel.Concurrent
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.kernel.Spawn
import cats.effect.kernel.Sync
import cats.effect.std.Dispatcher
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.raquo.domtypes.generic.builders.EventPropBuilder
import com.raquo.domtypes.generic.builders.HtmlAttrBuilder
import com.raquo.domtypes.generic.builders.HtmlTagBuilder
import com.raquo.domtypes.generic.builders.PropBuilder
import com.raquo.domtypes.generic.builders.ReflectedHtmlAttrBuilder
import com.raquo.domtypes.generic.codecs.Codec
import com.raquo.domtypes.generic.defs.attrs.*
import com.raquo.domtypes.generic.defs.complex.*
import com.raquo.domtypes.generic.defs.props.*
import com.raquo.domtypes.generic.defs.reflectedAttrs.*
import com.raquo.domtypes.jsdom.defs.eventProps.*
import com.raquo.domtypes.jsdom.defs.tags.*
import fs2.Pipe
import fs2.Stream
import fs2.concurrent.Channel
import fs2.concurrent.Signal
import org.scalajs.dom
import shapeless3.deriving.K0

import scala.collection.mutable
import scala.scalajs.js
import scala.annotation.nowarn

object io extends Dsl[IO]

object Dsl:
  def apply[F[_]: Async]: Dsl[F] = new Dsl[F] {}

trait Dsl[F[_]]
    extends HtmlBuilders[F],
      DocumentTags[HtmlTagT[F]],
      GroupingTags[HtmlTagT[F]],
      TextTags[HtmlTagT[F]],
      FormTags[HtmlTagT[F]],
      SectionTags[HtmlTagT[F]],
      EmbedTags[HtmlTagT[F]],
      TableTags[HtmlTagT[F]],
      MiscTags[HtmlTagT[F]],
      HtmlAttrs[HtmlAttr[F, _]],
      ReflectedHtmlAttrs[Prop[F, _, _]],
      Props[Prop[F, _, _]],
      ClipboardEventProps[EventProp[F, _]],
      ErrorEventProps[EventProp[F, _]],
      FormEventProps[EventProp[F, _]],
      KeyboardEventProps[EventProp[F, _]],
      MediaEventProps[EventProp[F, _]],
      MiscellaneousEventProps[EventProp[F, _]],
      MouseEventProps[EventProp[F, _]],
      PointerEventProps[EventProp[F, _]]

trait HtmlBuilders[F[_]](using F: Async[F])
    extends HtmlTagBuilder[HtmlTagT[F], dom.HTMLElement],
      HtmlAttrBuilder[HtmlAttr[F, _]],
      ReflectedHtmlAttrBuilder[Prop[F, _, _]],
      PropBuilder[Prop[F, _, _]],
      EventPropBuilder[EventProp[F, _], dom.Event]:

  protected def htmlTag[E <: dom.HTMLElement](tagName: String, void: Boolean) =
    HtmlTag(tagName, void)

  protected def htmlAttr[V](key: String, codec: Codec[V, String]) =
    HtmlAttr(key, codec)

  protected def reflectedAttr[V, J](
      attrKey: String,
      propKey: String,
      attrCodec: Codec[V, String],
      propCodec: Codec[V, J]) =
    Prop(propKey, propCodec)

  protected def prop[V, J](name: String, codec: Codec[V, J]) =
    Prop(name, codec)

  def eventProp[V <: dom.Event](key: String): EventProp[F, V] =
    EventProp(key)

  def cls: ClassAttr[F] = ClassAttr[F]

  def children: Children[F] = Children[F]

  def children[K](f: K => Resource[F, dom.Node]): KeyedChildren[F, K] =
    KeyedChildren[F, K](f)

type HtmlTagT[F[_]] = [E <: dom.HTMLElement] =>> HtmlTag[F, E]
final class HtmlTag[F[_], E <: dom.HTMLElement] private[calico] (name: String, void: Boolean)(
    using F: Async[F]):

  def apply[M](modifier: M)(using Modifier[F, E, M]): Resource[F, E] =
    apply(Tuple1(modifier))

  def apply[M](mkModifier: E => M)(using Modifier[F, E, M]): Resource[F, E] =
    apply(e => Tuple1(mkModifier(e)))

  def apply[M <: Tuple](modifiers: M)(
      using K0.ProductInstances[Modifier[F, E, _], M]): Resource[F, E] =
    apply(_ => modifiers)

  def apply[M <: Tuple](mkModifiers: E => M)(
      using inst: K0.ProductInstances[Modifier[F, E, _], M]): Resource[F, E] =
    build.toResource.flatMap { e =>
      inst.foldLeft(mkModifiers(e))(Resource.pure(e)) {
        [a] => (r: Resource[F, E], m: Modifier[F, E, a], a: a) => r.flatTap(m.modify(a, _))
      }
    }

  private def build = F.delay(dom.document.createElement(name).asInstanceOf[E])

trait Modifier[F[_], E, A]:
  outer =>

  def modify(a: A, e: E): Resource[F, Unit]

  final def contramap[B](f: B => A): Modifier[F, E, B] =
    (b: B, e: E) => outer.modify(f(b), e)

@nowarn("cat=deprecation")
object Modifier:
  given forUnit[F[_], E]: Modifier[F, E, Unit] with
    def modify(unit: Unit, e: E) = Resource.unit

  given forString[F[_], E <: dom.Node](using F: Async[F]): Modifier[F, E, String] =
    forStringStream.contramap(Stream.emit(_))

  @deprecated
  given forStringStream[F[_], E <: dom.Node](
      using F: Async[F]): Modifier[F, E, Stream[F, String]] with
    def modify(s: Stream[F, String], e: E) = for
      n <- F
        .delay(dom.document.createTextNode(""))
        .flatTap(n => F.delay(e.appendChild(n)))
        .toResource
      _ <- s.foreach(t => F.delay(n.textContent = t)).compile.drain.background
    yield ()

  @deprecated
  given forOptionStringStream[F[_], E <: dom.Node](
      using F: Async[F]): Modifier[F, E, Stream[F, Option[String]]] with
    def modify(s: Stream[F, Option[String]], e: E) = for
      n <- F
        .delay(dom.document.createTextNode(""))
        .flatTap(n => F.delay(e.appendChild(n)))
        .toResource
      _ <- s.foreach(t => F.delay(n.textContent = t.getOrElse(""))).compile.drain.background
    yield ()

  given forResource[F[_], E <: dom.Node, A](
      using M: Modifier[F, E, A]): Modifier[F, E, Resource[F, A]] with
    def modify(a: Resource[F, A], e: E) = a.flatMap(M.modify(_, e))

  given forSignal[F[_], E <: dom.Node, A](
      using M: Modifier[F, E, Stream[F, A]]): Modifier[F, E, Signal[F, A]] =
    M.contramap(_.discrete)

  given forFoldable[F[_]: Monad, E <: dom.Node, G[_]: Foldable, A](
      using M: Modifier[F, E, A]): Modifier[F, E, G[A]] with
    def modify(ga: G[A], e: E) = ga.foldMapM(M.modify(_, e)).void

  given forElement[F[_], E <: dom.Node, E2 <: dom.Node](
      using F: Sync[F]): Modifier[F, E, Resource[F, E2]] with
    def modify(e2: Resource[F, E2], e: E) =
      e2.evalMap(e2 => F.delay(e.appendChild(e2)))

  @deprecated
  given forElementStream[F[_], E <: dom.Node, E2 <: dom.Node](
      using F: Async[F]): Modifier[F, E, Stream[F, Resource[F, E2]]] =
    forOptionElementStream.contramap(_.map(Some(_)))

  @deprecated
  given forOptionElementStream[F[_], E <: dom.Node, E2 <: dom.Node](
      using F: Async[F]): Modifier[F, E, Stream[F, Option[Resource[F, E2]]]] with
    def modify(e2s: Stream[F, Option[Resource[F, E2]]], e: E) =
      for
        sentinel <- Resource.eval(F.delay(dom.document.createComment("")))
        hs <- DomHotswap[F, dom.Node](sentinel.pure)
        _ <- F.delay(e.appendChild(sentinel)).toResource
        _ <- e2s
          .foreach { next =>
            hs.swap(next.getOrElse(sentinel.pure)) { (p, n) => F.delay(e.replaceChild(n, p)) }
          }
          .compile
          .drain
          .background
      yield ()

final class HtmlAttr[F[_], V] private[calico] (key: String, codec: Codec[V, String]):
  def :=(v: V)(using Spawn[F]): HtmlAttr.Modified[F, V] =
    this <-- Signal.constant(v)

  def <--(vs: Signal[F, V]): HtmlAttr.Modified[F, V] =
    this <-- Resource.pure(vs)

  private def <--(vs: Resource[F, Signal[F, V]]): HtmlAttr.Modified[F, V] =
    HtmlAttr.Modified(key, codec, vs)

  @deprecated
  def <--(vs: Stream[F, V])(using Concurrent[F]): HtmlAttr.Modified[F, V] =
    this <-- vs.hold1Resource

  @deprecated
  def <--(vs: Resource[F, Stream[F, V]])(using Concurrent[F]): HtmlAttr.Modified[F, V] =
    this <-- vs.flatMap(_.hold1Resource)

object HtmlAttr:
  final class Modified[F[_], V] private[HtmlAttr] (
      val key: String,
      val codec: Codec[V, String],
      val values: Resource[F, Signal[F, V]]
  )

  given [F[_], E <: dom.Element, V](using F: Async[F]): Modifier[F, E, Modified[F, V]] with
    def modify(attr: Modified[F, V], e: E) =
      attr.values.flatMap { vs =>
        def setAttr(v: V) = F.delay(e.setAttribute(attr.key, attr.codec.encode(v)))
        Resource.eval(vs.getAndUpdates.flatTap((v, _) => setAttr(v))).flatMap { (_, vs) =>
          vs.foreach(setAttr(_)).compile.drain.cedeBackground.void
        }
      }

sealed class Prop[F[_], V, J] private[calico] (name: String, codec: Codec[V, J]):
  def :=(v: V)(using Spawn[F]): Prop.Modified[F, V, J] =
    this <-- Signal.constant(v)

  def <--(vs: Signal[F, V]): Prop.Modified[F, V, J] =
    this <-- Resource.pure(vs)

  private def <--(vs: Resource[F, Signal[F, V]]): Prop.Modified[F, V, J] =
    Prop.Modified(name, codec, vs)

  @deprecated
  def <--(vs: Stream[F, V])(using Concurrent[F]): Prop.Modified[F, V, J] =
    this <-- vs.hold1Resource

  @deprecated
  def <--(vs: Resource[F, Stream[F, V]])(using Concurrent[F]): Prop.Modified[F, V, J] =
    this <-- vs.flatMap(_.hold1Resource)

object Prop:
  final class Modified[F[_], V, J] private[Prop] (
      val name: String,
      val codec: Codec[V, J],
      val values: Resource[F, Signal[F, V]]
  )

  given [F[_], E, V, J](using F: Async[F]): Modifier[F, E, Modified[F, V, J]] with
    def modify(prop: Modified[F, V, J], e: E) =
      prop.values.flatMap { vs =>
        def setProp(v: V) =
          F.delay(e.asInstanceOf[js.Dictionary[J]](prop.name) = prop.codec.encode(v))
        Resource.eval(vs.getAndUpdates.flatTap((v, _) => setProp(v))).flatMap { (_, vs) =>
          vs.foreach(setProp(_)).compile.drain.cedeBackground.void
        }
      }

final class EventProp[F[_], E] private[calico] (key: String):
  def -->(sink: Pipe[F, E, Nothing]): EventProp.Modified[F, E] = EventProp.Modified(key, sink)

object EventProp:
  final class Modified[F[_], E] private[calico] (val key: String, val sink: Pipe[F, E, Nothing])

  given [F[_], E <: dom.EventTarget, V](using F: Async[F]): Modifier[F, E, Modified[F, V]] with
    def modify(prop: Modified[F, V], e: E) =
      fs2.dom.events(e, prop.key).through(prop.sink).compile.drain.cedeBackground.void

final class ClassAttr[F[_]] private[calico]
    extends Prop[F, List[String], String](
      "className",
      new:
        def decode(domValue: String) = domValue.split(" ").toList
        def encode(scalaValue: List[String]) = scalaValue.mkString(" ")
    ):

  def :=(cls: String)(using Spawn[F]): Prop.Modified[F, List[String], String] =
    this := List(cls)

final class Children[F[_]] private[calico]:
  def <--(cs: Signal[F, List[Resource[F, dom.Node]]])(using Monad[F]): Children.Modified[F] =
    this <-- cs.map(_.sequence)

  @deprecated
  def <--(cs: Stream[F, List[Resource[F, dom.Node]]])(
      using Concurrent[F]): Children.Modified[F] =
    this <-- cs.map(_.sequence)

  def <--(cs: Signal[F, Resource[F, List[dom.Node]]]): Children.Modified[F] =
    Children.Modified(Resource.pure(cs))

  @deprecated
  def <--(cs: Stream[F, Resource[F, List[dom.Node]]])(
      using Concurrent[F],
      DummyImplicit): Children.Modified[F] =
    Children.Modified(cs.hold1Resource)

object Children:
  final class Modified[F[_]] private[calico] (
      val cs: Resource[F, Signal[F, Resource[F, List[dom.Node]]]])

  given [F[_], E <: dom.Element](using F: Async[F]): Modifier[F, E, Modified[F]] with
    def modify(children: Modified[F], e: E) =
      for
        hs <- DomHotswap[F, List[dom.Node]](Resource.pure(Nil))
        placeholder <- Resource.eval(
          F.delay(e.appendChild(dom.document.createComment("")))
        )
        _ <- children.cs.evalMap(_.getAndUpdates).flatMap { (head, tail) =>
          def install(children: Resource[F, List[dom.Node]]) =
            hs.swap(children) { (prev, next) =>
              F.delay {
                prev.foreach(e.removeChild)
                next.foreach(e.insertBefore(_, placeholder))
              }
            }

          Resource.eval(install(head)) *>
            tail.foreach(install(_)).compile.drain.cedeBackground
        }
      yield ()

final class KeyedChildren[F[_], K] private[calico] (f: K => Resource[F, dom.Node]):
  def <--(ks: Signal[F, List[K]]): KeyedChildren.Modified[F, K] =
    KeyedChildren.Modified(f, Resource.pure(ks))

  @deprecated
  def <--(ks: Stream[F, List[K]])(using Concurrent[F]): KeyedChildren.Modified[F, K] =
    KeyedChildren.Modified(f, ks.hold1Resource)

object KeyedChildren:
  final class Modified[F[_], K] private[calico] (
      val f: K => Resource[F, dom.Node],
      val ks: Resource[F, Signal[F, List[K]]])

  given [F[_], E <: dom.Element, K: Hash](using F: Async[F]): Modifier[F, E, Modified[F, K]]
    with
    def modify(children: Modified[F, K], e: E) =
      for
        active <- Resource.make(Ref[F].of(mutable.Map.empty[K, (dom.Node, F[Unit])]))(
          _.get.flatMap(_.values.toList.traverse_(_._2))
        )
        _ <- children.ks.evalMap(_.getAndUpdates).flatMap { (head, tail) =>

          def update(ks: List[K]) = active.get.flatMap { currentNodes =>

            def traverse_[A, U](it: Iterable[A])(f: A => F[U]): F[Unit] =
              it.foldLeft(F.unit)(_ <* f(_))

            val releaseOldNodes = F.delay(currentNodes.values).flatMap(traverse_(_)(_._2))

            F.delay((mutable.Map.empty[K, (dom.Node, F[Unit])], new js.Array[dom.Node]))
              .flatMap { (nextNodes, nextChildren) =>
                active
                  .set(nextNodes)
                  .bracket { _ =>
                    traverse_(ks) { k =>
                      F.delay(currentNodes.remove(k)).flatMap { v =>
                        v.fold {
                          F.uncancelable { poll =>
                            poll(children.f(k).allocated).flatMap { v =>
                              F.delay {
                                nextNodes += k -> v
                                nextChildren.push(v._1)
                              }
                            }
                          }
                        } { v =>
                          F.delay {
                            nextNodes += (k -> v)
                            nextChildren.push(v._1)
                          }
                        }
                      }
                    } *> F.delay {
                      import scala.scalajs.runtime.*
                      e.replaceChildren(toScalaVarArgs(nextChildren)*)
                    }
                  }(_ => releaseOldNodes.evalOn(unsafe.MacrotaskExecutor))
              }
          }

          Resource.eval(update(head)) *>
            tail.foreach(update(_)).compile.drain.cedeBackground
        }
      yield ()
