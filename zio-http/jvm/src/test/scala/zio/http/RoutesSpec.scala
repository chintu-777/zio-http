/*
 * Copyright Sporta Technologies PVT LTD & the ZIO HTTP contributors.
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

package zio.http

import zio.test._

object RoutesSpec extends ZIOHttpSpec {
  def extractStatus(response: Response): Status = response.status

  def spec = suite("HttpAppSpec")(
    test("empty not found") {
      val app = Routes.empty

      for {
        result <- app.run()
      } yield assertTrue(extractStatus(result) == Status.NotFound)
    },
    test("compose empty not found") {
      val app = Routes.empty ++ Routes.empty

      for {
        result <- app.run()
      } yield assertTrue(extractStatus(result) == Status.NotFound)
    },
    test("run identity") {
      val body = Body.fromString("foo")

      val app = handler { (req: Request) =>
        Response(body = req.body)
      }

      for {
        result <- app.runZIO(Request(body = body))
      } yield assertTrue(result.body == body)
    },
    test("anyOf method matches correct route") {
      val handler = handler[Clock] {
        case req @ GET -> Root / "test1" => Response.ok.withEntity("Handler for test1")
        case req @ GET -> Root / "test2" => Response.ok.withEntity("Handler for test2")
        case _ => Response.notFound
      }

      val routes = Routes(
        GET / anyOf("test1", "test2") -> handler
      )

      for {
        result1 <- routes.run(Request(Method.GET, path"/test1"))
        result2 <- routes.run(Request(Method.GET, path"/test2"))
        result3 <- routes.run(Request(Method.GET, path"/unknown"))
      } yield {
        assert(extractStatus(result1))(equalTo(Status.OK)) &&
        assert(extractStatus(result2))(equalTo(Status.OK)) &&
        assert(extractStatus(result3))(equalTo(Status.NotFound))
      }
    }
  )
}
