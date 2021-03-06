package bifrost.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import bifrost.http.api.routes.ProgramApiRoute
import bifrost.modifier.box.Box
import io.circe.parser.parse
import scorex.crypto.encode.Base58
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ProgramMethodExecutionSpec extends AnyWordSpec
  with Matchers
  with ScalatestRouteTest
  with ProgramMockState {

  val route: Route = ProgramApiRoute(settings, nodeViewHolderRef).route

  "executeProgramMethod" should {

    val boxState: Set[Box] = Set(stateBox, codeBox, executionBox)

    manuallyApplyBoxes(boxState, 1)

    "Update mutable state in a Program and return the updated state" in {

      val requestBody = ByteString(
        s"""{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "executeProgramMethod",
           |  "params": [{
           |    "owner": "$publicKey",
           |    "signatures": {
           |      "$publicKey": ""
           |    },
           |    "methodName": "add",
           |    "methodParams": {
           |      "x": 2,
           |      "y": 2
           |    },
           |    "programId": "${Base58.encode(executionBox.id)}",
           |    "preFeeBoxes": {
           |      "$publicKey": [[${polyBoxes.head.box.nonce}, ${polyBoxes.head.box.value}]]
           |     },
           |     "fees": {
           |      "$publicKey": 0
           |     },
           |     "timestamp": ${System.currentTimeMillis},
           |     "data": ""
           |  }]
           |}
           |""".stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get

        println(res)

        (res \\ "result").head.isObject shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }
  }
}
