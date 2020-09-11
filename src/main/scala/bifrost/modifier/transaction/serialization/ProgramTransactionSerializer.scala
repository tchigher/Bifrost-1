package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import com.google.common.primitives.{Ints, Longs}
import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.modifier.transaction.bifrostTransaction.ProgramTransaction
import scorex.crypto.signatures.Curve25519
import scala.util.Try

//TODO: Jing - remove
object ProgramTransactionSerializer {

  override def decode(bytes: Array[Byte]): Try[ProgramTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    require(typeStr == "ProgramTransaction")

    /* Grab the rest of the bytes, which should begin similarly (with sub-type) */
    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.take(Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES, Ints.BYTES + newTypeLength))

    newTypeStr match {
      case "ProgramCreation" => ProgramCreationSerializer.decode(newBytes).get
      case "ProgramMethodExecution" => ProgramMethodExecutionSerializer.decode(newBytes).get
    }
  }

  //noinspection ScalaStyle
  def commonDecode(bytes: Array[Byte]): (
    PublicKey25519Proposition,
      Map[PublicKey25519Proposition, Signature25519],
      Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
      Map[PublicKey25519Proposition, Long],
      Long
    ) = {

    var numReadBytes = 0

    val timestamp: Long = Longs.fromByteArray(bytes.slice(0, Longs.BYTES))

    numReadBytes += Longs.BYTES

    val owner = PublicKey25519Proposition(bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val signatures: Map[PublicKey25519Proposition, Signature25519] = {
      val sig = Signature25519(bytes.slice(numReadBytes, numReadBytes + Curve25519.SignatureLength))
      Map(owner -> sig)
    }

    numReadBytes += Curve25519.SignatureLength

    val feePreBoxesLength = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    val preBoxes: IndexedSeq[(Nonce, Long)] = (0 until feePreBoxesLength).map { _ =>
      val nonce: Nonce = Longs.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Longs.BYTES))
      numReadBytes += Longs.BYTES
      val amount = Longs.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Longs.BYTES))
      numReadBytes += Longs.BYTES

      nonce -> amount
    }

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = Map(owner -> preBoxes)

    val fees: Map[PublicKey25519Proposition, Long] = Map(owner -> Longs.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Longs.BYTES)))

    (owner, signatures, feePreBoxes, fees, timestamp)
  }
}
