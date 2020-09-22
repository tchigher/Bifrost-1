package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.crypto.serialization.Signature25519Serializer
import bifrost.modifier.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.transaction.bifrostTransaction.AssetCreation
import bifrost.utils.Extensions._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.util.Try
import com.google.common.primitives.{Ints, Longs}
import scorex.crypto.signatures.Curve25519
import bifrost.modifier.box.proposition.{PublicKey25519Proposition, Constants25519}

object AssetCreationSerializer extends BifrostSerializer[AssetCreation] {

  override def serialize(obj: AssetCreation, w: Writer): Unit = {
    /* to: IndexedSeq[(PublicKey25519Proposition, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (prop, value) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      w.putULong(value)
    }

    /* signatures: Map[PublicKey25519Proposition, Signature25519] */
    w.putUInt(obj.signatures.size)
    obj.signatures.foreach { case (prop, sig) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      Signature25519Serializer.serialize(sig, w)
    }

    /* assetCode: String */
    w.putIntString(obj.assetCode)

    /* issuer: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.issuer, w)

    /* fee: Long */
    w.putULong(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): AssetCreation = {
    val toLength: Int = r.getUInt().toIntExact
    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until toLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val value = r.getULong()
      prop -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures: Map[PublicKey25519Proposition, Signature25519] = (0 until signaturesLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val sig = Signature25519Serializer.parse(r)
      prop -> sig
    }.toMap

    val assetCode: String = r.getIntString()
    val issuer: PublicKey25519Proposition = PublicKey25519PropositionSerializer.parse(r)
    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()

    AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
  }

  //TODO: Jing - remove
  //noinspection ScalaStyle
  def decode(bytes: Array[Byte]): Try[AssetCreation] = Try {
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
    )
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    numReadBytes = 2 * Longs.BYTES

    val sigLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    val toLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    val assetCodeLen: Int = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    val assetCode: String = new String(
      bytesWithoutType.slice(numReadBytes, numReadBytes + assetCodeLen)
    )

    numReadBytes += assetCodeLen

    val hub = PublicKey25519Proposition(bytesWithoutType.slice(numReadBytes,
      numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val signatures: Map[PublicKey25519Proposition, Signature25519] = (0 until sigLength).map { i =>
      val sig: Signature25519 = Signature25519(bytesWithoutType.slice(numReadBytes + i * Curve25519.SignatureLength,
        numReadBytes + (i + 1) * Curve25519.SignatureLength))

      hub -> sig
    }.toMap

    numReadBytes += sigLength * Curve25519.SignatureLength

    val elementLength = Longs.BYTES + Curve25519.KeyLength

    val to = (0 until toLength) map { i =>
      val pk = bytesWithoutType.slice(numReadBytes + i * elementLength, numReadBytes + (i + 1) * elementLength - Longs.BYTES)
      val v = Longs.fromByteArray(
        bytesWithoutType.slice(numReadBytes + (i + 1) * elementLength - Longs.BYTES, numReadBytes + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), v)
    }



    //    val dataLen: Int = Ints.fromByteArray(bytesWithoutType.slice(bytesWithoutType.length - Ints.BYTES, bytesWithoutType.length))
    //    val data: String = new String(
    //      bytes.slice(bytesWithoutType.length - Ints.BYTES - dataLen, bytesWithoutType.length - Ints.BYTES)
    //    )

    // TODO: Jing - see if the signature and proposition checks out
    val assetCreation: AssetCreation = AssetCreation(to, signatures, assetCode, hub, fee, timestamp, data)
    AssetCreation.validate(assetCreation)
    assetCreation
  }
}
