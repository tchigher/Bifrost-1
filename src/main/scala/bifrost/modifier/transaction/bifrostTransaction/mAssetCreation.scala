package bifrost.modifier.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.{FastCryptographicHash, PrivateKey25519, PrivateKey25519Companion, Signature25519}
import bifrost.modifier.box.{AssetBox, Box, BoxUnlocker}
import bifrost.modifier.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.modifier.transaction.serialization.{AssetCreationCompanion, mAssetCreationCompanion}
import bifrost.wallet.Wallet
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class mAssetCreation(val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                         val signatures: IndexedSeq[Signature25519],
                         val assetCode: String,
                         val hub: PublicKey25519Proposition,
                         override val fee: Long,
                         override val timestamp: Long,
                         val data: String) extends Transaction {


  override type M = mAssetCreation

  lazy val serializer = mAssetCreationCompanion

  override def toString: String = s"AssetCreation(${json.noSpaces})"

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  lazy val hashNoNonces = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )

  override lazy val newBoxes: Traversable[Box] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = AssetCreation.nonceFromDigest(FastCryptographicHash(
        "AssetCreation".getBytes ++
          prop.pubKeyBytes ++
          hub.pubKeyBytes ++
          assetCode.getBytes ++
          hashNoNonces ++
          Ints.toByteArray(idx)
      ))

      //TODO assetBoxes elsewhere do not subtract fee from box value
      //TODO no check that amount >= fee
      //AssetBox(prop, nonce, value, assetCode, hub)
      AssetBox(prop, nonce, value - fee, assetCode, hub, data)
  }

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "value" -> s._2.asJson
      ).asJson
    }.asJson,
    "hub" -> Base58.encode(hub.pubKeyBytes).asJson,
    "assetCode" -> assetCode.asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson

  def commonMessageToSign: Array[Byte] = (if (newBoxes.nonEmpty) {
    newBoxes
      .map(_.bytes)
      .reduce(_ ++ _)
  } else {
    Array[Byte]()
  }) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "AssetCreation".getBytes(),
    commonMessageToSign,
    hub.pubKeyBytes,
    assetCode.getBytes,
    data.getBytes
  )

}

object mAssetCreation {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def validate(tx: mAssetCreation): Try[Unit] = Try {
    //require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0L))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.forall({ case (signature) =>
      //println(signature.isValid(tx.hub, tx.messageToSign))
      signature.isValid(tx.hub, tx.messageToSign)
    }), "Invalid signatures")
  }

  /**
    * Route here from AssetApiRoute
    * Assumes that the Wallet contains the hub's key information
    * Takes Wallet from current view, and generates signature from hub's public key
    * Forms corresponding AssetCreation transaction
    */
  def createAndApply(w: Wallet,
                     to: IndexedSeq[(PublicKey25519Proposition, Long)],
                     fee: Long,
                     hub: PublicKey25519Proposition,
                     assetCode: String,
                     data: String): Try[mAssetCreation] = Try {

    val selectedSecret = w.secretByPublicImage(hub).get
    val fakeSigs = IndexedSeq(Signature25519(Array()))
    val timestamp = Instant.now.toEpochMilli
    val messageToSign = mAssetCreation(to, fakeSigs, assetCode, hub, fee, timestamp, data).messageToSign

    val signatures = IndexedSeq(PrivateKey25519Companion.sign(selectedSecret, messageToSign))

    mAssetCreation(to, signatures, assetCode, hub, fee, timestamp, data)
  }
}
