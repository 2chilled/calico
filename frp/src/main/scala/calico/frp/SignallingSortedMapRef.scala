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

package calico.frp

import cats.effect.kernel.Concurrent
import cats.effect.kernel.Deferred
import cats.effect.kernel.Ref
import cats.kernel.Order
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.concurrent.SignallingMapRef

import scala.collection.immutable.LongMap
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

abstract class SignallingSortedMapRef[F[_], K, V]
    extends SignallingRef[F, SortedMap[K, V]]
    with SignallingMapRef[F, K, Option[V]]:
  override def apply(k: K): SignallingRef[F, Option[V]]

  def keys: Signal[F, SortedSet[K]]

object SignallingSortedMapRef:
  def apply[F[_], K, V](
      using F: Concurrent[F],
      K: Order[K]): F[SignallingSortedMapRef[F, K, V]] =

    case class State(
        value: SortedMap[K, V],
        lastUpdate: Long,
        listeners: LongMap[Deferred[F, (SortedMap[K, V], Long)]],
        keyListeners: Map[K, LongMap[Deferred[F, (Option[V], Long)]]]
    )

    given Ordering[K] = K.toOrdering

    F.ref(State(SortedMap.empty, 0L, LongMap.empty, SortedMap.empty)).product(F.ref(1L)).map {
      case (state, ids) =>
        val newId = ids.getAndUpdate(_ + 1)

        def traverse_[A, U](it: Iterable[A])(f: A => F[U]): F[Unit] =
          it.foldLeft(F.unit)(_ <* f(_))

        def updateMapAndNotify[O](
            state: State,
            f: SortedMap[K, V] => (SortedMap[K, V], O)): (State, F[O]) =
          val (newValue, result) = f(state.value)

          val lastUpdate = state.lastUpdate + 1
          val newState = State(newValue, lastUpdate, LongMap.empty, SortedMap.empty)

          val notifyListeners =
            traverse_(state.listeners.values)(_.complete(newValue -> lastUpdate))
          val notifyKeyListeners = traverse_(state.keyListeners) { (k, listeners) =>
            val v = newValue.get(k)
            traverse_(listeners.values)(_.complete(v -> lastUpdate))
          }

          newState -> (notifyListeners *> notifyKeyListeners).as(result)

        def updateKeyAndNotify[U](state: State, k: K, f: Option[V] => (Option[V], U))
            : (State, F[U]) =
          val (newValue, result) = f(state.value.get(k))

          val newMap = newValue.fold(state.value - k)(v => state.value + (k -> v))
          val lastUpdate = state.lastUpdate + 1
          val newKeyListeners = state.keyListeners - k
          val newState = State(newMap, lastUpdate, LongMap.empty, newKeyListeners)

          val notifyListeners =
            traverse_(state.listeners.values)(_.complete(newMap -> lastUpdate))
          val notifyKeyListeners = state.keyListeners.get(k).fold(F.unit) { listeners =>
            traverse_(listeners.values)(_.complete(newValue -> lastUpdate))
          }

          newState -> (notifyListeners *> notifyKeyListeners).as(result)

        new SignallingSortedMapRef[F, K, V]
          with AbstractSignallingRef[F, State, SortedMap[K, V]](newId, state):
          outer =>
          def getValue(s: State) = s.value

          def getLastUpdate(s: State) = s.lastUpdate

          def withListener(s: State, id: Long, wait: Deferred[F, (SortedMap[K, V], Long)]) =
            s.copy(listeners = s.listeners.updated(id, wait))

          def withoutListener(s: State, id: Long) = s.copy(listeners = s.listeners.removed(id))

          def updateAndNotify[O](s: State, f: SortedMap[K, V] => (SortedMap[K, V], O)) =
            updateMapAndNotify(s, f)

          def keys = new Signal[F, SortedSet[K]]:
            def get = outer.get.map(_.keySet)
            def continuous = outer.continuous.map(_.keySet)
            def discrete = outer.discrete.map(_.keySet).changes

          def apply(k: K) = new AbstractSignallingRef[F, State, Option[V]](newId, state):

            def getValue(s: State) = s.value.get(k)

            def getLastUpdate(s: State) = s.lastUpdate

            def withListener(s: State, id: Long, wait: Deferred[F, (Option[V], Long)]) =
              s.copy(keyListeners = s
                .keyListeners
                .updatedWith(k)(_.getOrElse(LongMap.empty).updated(id, wait).some))

            def withoutListener(s: State, id: Long) =
              s.copy(keyListeners =
                s.keyListeners.updatedWith(k)(_.map(_.removed(id)).filterNot(_.isEmpty)))

            def updateAndNotify[O](s: State, f: Option[V] => (Option[V], O)) =
              updateKeyAndNotify(s, k, f)

    }

  private trait AbstractSignallingRef[F[_], S, A](newId: F[Long], state: Ref[F, S])(
      using F: Concurrent[F])
      extends SignallingRef[F, A]:

    def getValue(s: S): A

    def getLastUpdate(s: S): Long

    def withListener(s: S, id: Long, wait: Deferred[F, (A, Long)]): S

    def withoutListener(s: S, id: Long): S

    def updateAndNotify[B](s: S, f: A => (A, B)): (S, F[B])

    def get: F[A] = state.get.map(getValue(_))

    def continuous: Stream[F, A] = Stream.repeatEval(get)

    def discrete: Stream[F, A] = {
      def go(id: Long, lastSeen: Long): Stream[F, A] =
        def getNext: F[(A, Long)] =
          F.deferred[(A, Long)].flatMap { wait =>
            state.modify { state =>
              val lastUpdate = getLastUpdate(state)
              if lastUpdate != lastSeen then state -> (getValue(state) -> lastUpdate).pure[F]
              else withListener(state, id, wait) -> wait.get

            }.flatten
          }

        Stream.eval(getNext).flatMap {
          case (v, lastUpdate) =>
            Stream.emit(v) ++ go(id, lastSeen = lastUpdate)
        }

      def cleanup(id: Long): F[Unit] = state.update(withoutListener(_, id))

      Stream.bracket(newId)(cleanup).flatMap { id =>
        Stream.eval(state.get).flatMap { state =>
          Stream.emit(getValue(state)) ++ go(id, getLastUpdate(state))
        }
      }
    }

    def set(a: A): F[Unit] = update(_ => a)

    def update(f: A => A): F[Unit] = modify(v => (f(v), ()))

    def modify[B](f: A => (A, B)): F[B] =
      state.modify(updateAndNotify(_, f)).flatten

    def tryModify[B](f: A => (A, B)): F[Option[B]] =
      state.tryModify(updateAndNotify(_, f)).flatMap(_.sequence)

    def tryUpdate(f: A => A): F[Boolean] =
      tryModify(a => (f(a), ())).map(_.isDefined)

    def access: F[(A, A => F[Boolean])] =
      state.access.map {
        case (state, set) =>
          val setter = { (newValue: A) =>
            val (newState, notifyListeners) =
              updateAndNotify(state, _ => (newValue, ()))

            set(newState).flatTap { succeeded => notifyListeners.whenA(succeeded) }
          }

          (getValue(state), setter)
      }

    def tryModifyState[B](state: cats.data.State[A, B]): F[Option[B]] = {
      val f = state.runF.value
      tryModify(f(_).value)
    }

    def modifyState[B](state: cats.data.State[A, B]): F[B] = {
      val f = state.runF.value
      modify(f(_).value)
    }
