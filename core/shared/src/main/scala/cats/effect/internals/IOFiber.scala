/*
 * Copyright (c) 2017-2018 The Typelevel Cats-effect Project Developers
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

package cats.effect
package internals

import scala.concurrent.Promise
import cats.effect.internals.Callback.Extensions

/**
 * INTERNAL API - [[Fiber]] instantiated for [[IO]].
 *
 * Not exposed, the `IO` implementation exposes [[Fiber]] directly.
 */
private[effect] final case class IOFiber[E, A](join: IO[E, A])
  extends Fiber[IO[E, ?], A] {

  def cancel: IO[E, Unit] =
    IOCancel.signal(join)
}

private[effect] object IOFiber {
  /** Internal API */
  def build[E, A](p: Promise[Either[E, A]], conn: IOConnection): Fiber[IO[E, ?], A] =
    IOFiber(IO.Async[E, A] { (ctx, cb) =>
      implicit val ec = TrampolineEC.immediate

      // Short-circuit for already completed `Future`
      p.future.value match {
        case Some(value) =>
          cb.async(value.get)
        case None =>
          // Cancellation needs to be linked to the active task
          ctx.push(conn.cancel)
          p.future.onComplete { r =>
            ctx.pop()
            cb(r.get)
          }
      }
    })
}
