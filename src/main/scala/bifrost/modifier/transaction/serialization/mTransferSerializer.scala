package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.{TransferTransaction, mTransferTransaction}
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

trait mTransferSerializer {
  def transferToBytes(tx: mTransferTransaction, txType: String): Array[Byte] = {
    val typeBytes = txType.getBytes

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(tx.fee),
      Longs.toByteArray(tx.timestamp),
      Ints.toByteArray(tx.signatures.length),
      Ints.toByteArray(tx.from.length),
      Ints.toByteArray(tx.to.length),
      tx.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
      tx.from.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)),
      tx.to.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
    )
  }

  /*def conversionToBytes(ct: ConversionTransaction, txType: String): Array[Byte] = {
    val typeBytes = txType.getBytes

    // concatenate map keys to reduce size when assetCodes are the same
    val keySeq = (ct.totalAssetBoxes.keySet ++ ct.assetsToReturn.keySet ++
      ct.assetTokensToRedeem.keySet ++ ct.conversionSignatures.keySet).toSeq.zipWithIndex
    val keyMapping: Map[(String, PublicKey25519Proposition), Int] = keySeq.toMap

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(ct.data.length),
      ct.data.getBytes,
      Longs.toByteArray(ct.fee),
      Longs.toByteArray(ct.timestamp),
      Ints.toByteArray(ct.totalAssetBoxes.size),
      Ints.toByteArray(ct.assetsToReturn.size),
      Ints.toByteArray(ct.assetTokensToRedeem.size),
      Ints.toByteArray(ct.conversionSignatures.size),
      Ints.toByteArray(keyMapping.size),
      keySeq.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(b._1._1.getBytes.length) ++ b._1._1.getBytes ++ b
        ._1._2.pubKeyBytes),
      ct.totalAssetBoxes.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(totAssets => totAssets._1.pubKeyBytes ++ Longs.toByteArray(
        totAssets._2))
      ),
      ct.assetsToReturn.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(assetReturn => assetReturn._1.pubKeyBytes ++ Longs.toByteArray(
        assetReturn._2))
      ),
      ct.assetTokensToRedeem.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(assetRedeem => assetRedeem._1.pubKeyBytes ++ Longs.toByteArray(
        assetRedeem._2))
      ),
      ct.conversionSignatures.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(_.signature)
      )
    )
  }*/

  def parametersParseBytes(bytes: Array[Byte]): (IndexedSeq[(PublicKey25519Proposition, Long)],
    IndexedSeq[(PublicKey25519Proposition, Long)],
    IndexedSeq[Signature25519], Long, Long) = {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    var numBytesRead = Ints.BYTES + typeLength

    val fee = Longs.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Longs.BYTES))
    val timestamp = Longs.fromByteArray(bytes.slice(numBytesRead + Longs.BYTES, numBytesRead + 2 * Longs.BYTES))
    val sigLength = Ints.fromByteArray(bytes.slice(numBytesRead + 2 * Longs.BYTES,
      numBytesRead + 2 * Longs.BYTES + Ints.BYTES))

    numBytesRead += 2 * Longs.BYTES + Ints.BYTES

    val fromLength = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    val toLength = Ints.fromByteArray(bytes.slice(numBytesRead + Ints.BYTES, numBytesRead + 2 * Ints.BYTES))

    numBytesRead += 2 * Ints.BYTES

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytes.slice(numBytesRead + i * Curve25519.SignatureLength,
        numBytesRead + (i + 1) * Curve25519.SignatureLength))
    }

    numBytesRead += sigLength * Curve25519.SignatureLength

    val elementLength = Longs.BYTES + Curve25519.KeyLength

    val from = (0 until fromLength) map { i =>
      val pk = bytes.slice(numBytesRead + i * elementLength, numBytesRead + (i + 1) * elementLength - Longs.BYTES)
      val nonce = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1) * elementLength - Longs.BYTES, numBytesRead + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), nonce)
    }

    numBytesRead += fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = bytes.slice(numBytesRead + i * elementLength, numBytesRead + (i + 1) * elementLength - Longs.BYTES)
      val v = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1) * elementLength - Longs.BYTES, numBytesRead + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), v)
    }
    (from, to, signatures, fee, timestamp)
  }
}
