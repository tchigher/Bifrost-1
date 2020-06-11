package bifrost.modifier.transaction.bifrostTransaction

import bifrost.crypto.{FastCryptographicHash, Signature25519}
import bifrost.modifier.box.{BoxUnlocker, PublicKeyNoncedBox}
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.modifier.transaction.serialization.TransferTransactionCompanion
import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

abstract class mTransferTransaction(val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                                   val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                                   val signatures: IndexedSeq[Signature25519],
                                   override val fee: Long,
                                   override val timestamp: Long) extends Transaction {

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Signature25519 = signature
      }
  }

  lazy val hashNoNonces = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "nonce" -> s._2.toString.asJson
      ).asJson
    }.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "value" -> s._2.toString.asJson
      ).asJson
    }.asJson,
    "signatures" -> signatures
      .map(s => Base58.encode(s.signature).asJson)
      .asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  def commonMessageToSign: Array[Byte] = (if (newBoxes.nonEmpty) {
    newBoxes
      .map(_.bytes)
      .reduce(_ ++ _)
  } else {
    Array[Byte]()
  }) ++
    unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)
}

