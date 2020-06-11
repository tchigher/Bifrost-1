package bifrost.modifier.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.{FastCryptographicHash, PrivateKey25519, Signature25519}
import bifrost.modifier.box.{AssetBox, Box}
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.Transaction.{Nonce, Value}
import bifrost.modifier.transaction.serialization.{AssetTransferCompanion, mAssetTransferCompanion}
import bifrost.wallet.Wallet
import com.google.common.primitives.{Bytes, Ints}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class mAssetTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                         override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                         override val signatures: IndexedSeq[Signature25519],
                         hub: PublicKey25519Proposition,
                         assetCode: String,
                         override val fee: Long,
                         override val timestamp: Long,
                         val data: String)
  extends mTransferTransaction(from, to, signatures, fee, timestamp) {

  override type M = mAssetTransfer

  override lazy val serializer = mAssetTransferCompanion

  override def toString: String = s"AssetTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[Box] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = AssetTransfer.nonceFromDigest(FastCryptographicHash(
        "AssetTransfer".getBytes ++
          prop.pubKeyBytes ++
          hub.pubKeyBytes ++
          assetCode.getBytes ++
          hashNoNonces ++
          Ints.toByteArray(idx)
      ))
      AssetBox(prop, nonce, value, assetCode, hub, data)
  }

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "nonce" -> s._2.asJson
      ).asJson
    }.asJson,
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

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "AssetTransfer".getBytes(),
    super.commonMessageToSign,
    hub.pubKeyBytes,
    assetCode.getBytes,
    data.getBytes
  )
}

object mAssetTransfer extends mTransferUtil {

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            hub: PublicKey25519Proposition,
            assetCode: String,
            fee: Long,
            timestamp: Long,
            data: String): mAssetTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "AssetTransfer", hub, assetCode, data).get
    mAssetTransfer(params._1, to, params._2, hub, assetCode, fee, timestamp, data)
  }

  def create(w: Wallet,
             toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
             fee: Long,
             hub: PublicKey25519Proposition,
             assetCode: String,
             data: String,
             publicKeyToSendFrom: Vector[String] = Vector(),
             publicKeyToSendChangeTo: String = ""): Try[mAssetTransfer] = Try {

    val params = parametersForCreate(w, toReceive, fee, "AssetTransfer", publicKeyToSendFrom, publicKeyToSendChangeTo, hub, assetCode)
    val timestamp = Instant.now.toEpochMilli
    mAssetTransfer(params._1.map(t => t._1 -> t._2), params._2, hub, assetCode, fee, timestamp, data)
  }


  def validate(tx: mAssetTransfer): Try[Unit] = validateTx(tx)
}

