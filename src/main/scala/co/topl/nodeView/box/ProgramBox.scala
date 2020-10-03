package co.topl.nodeView.box

import co.topl.nodeView.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.ProgramId
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

class ProgramBox(override val proposition: PublicKey25519Proposition,
                 override val nonce: Long,
                 override val value: ProgramId
                ) extends Box(proposition, nonce, value) {

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val typeOfBox: String = "ProgramBox"

  lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value" -> value.toString.asJson,
    "nonce" -> nonce.toString.asJson
  ).asJson
}