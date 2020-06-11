package bifrost.modifier.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.{FastCryptographicHash, PrivateKey25519, Signature25519}
import bifrost.modifier.box.{ArbitBox, Box}
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.Transaction.{Nonce, Value}
import bifrost.modifier.transaction.serialization.mArbitTransferCompanion
import bifrost.wallet.Wallet
import com.google.common.primitives.Ints

import scala.util.Try

case class mArbitTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                         override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                         override val signatures: IndexedSeq[Signature25519],
                         override val fee: Long,
                         override val timestamp: Long,
                         val data: String)
  extends mTransferTransaction(from, to, signatures, fee, timestamp) {

  override type M = mArbitTransfer

  override lazy val serializer = mArbitTransferCompanion

  override def toString: String = s"ArbitTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[Box] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = ArbitTransfer
        .nonceFromDigest(FastCryptographicHash("ArbitTransfer".getBytes
          ++ prop.pubKeyBytes
          ++ hashNoNonces
          ++ Ints.toByteArray(idx)))

      ArbitBox(prop, nonce, value)
  }

  override lazy val messageToSign: Array[Byte] = "ArbitTransfer".getBytes() ++ super.commonMessageToSign ++ data.getBytes
}

object mArbitTransfer extends mTransferUtil {

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long,
            data: String): mArbitTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "ArbitTransfer", data).get
    mArbitTransfer(params._1, to, params._2, fee, timestamp, data)
  }

  def create(w: Wallet, toRecieve: IndexedSeq[(PublicKey25519Proposition, Long)], fee: Long, data: String, publicKeyToSendFrom: Vector[String] = Vector(), publicKeyToSendChangeTo: String = ""): Try[mArbitTransfer] = Try
  {

    val params = parametersForCreate(w, toRecieve, fee, "ArbitTransfer", publicKeyToSendFrom, publicKeyToSendChangeTo)
    val timestamp = Instant.now.toEpochMilli
    mArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }

  def validate(tx: mArbitTransfer): Try[Unit] = validateTx(tx)
}


