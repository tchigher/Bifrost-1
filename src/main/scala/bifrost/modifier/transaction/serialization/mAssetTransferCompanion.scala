package bifrost.modifier.transaction.serialization

import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.modifier.transaction.bifrostTransaction.{AssetTransfer, mAssetTransfer}
import bifrost.serialization.Serializer
import com.google.common.primitives.Ints

import scala.util.Try

object mAssetTransferCompanion extends Serializer[mAssetTransfer] with mTransferSerializer {

  override def toBytes(at: mAssetTransfer): Array[Byte] = {
    TransferTransactionCompanion.prefixBytes ++ toChildBytes(at)
  }

  def toChildBytes(at: mAssetTransfer): Array[Byte] = {
    transferToBytes(at, "AssetTransfer") ++
      at.hub.pubKeyBytes ++
      at.assetCode.getBytes ++
      Ints.toByteArray(at.assetCode.getBytes.length)++
      at.data.getBytes++
      Ints.toByteArray(at.data.getBytes.length)
  }

  override def parseBytes(bytes: Array[Byte]): Try[mAssetTransfer] = Try {
    val params = parametersParseBytes(bytes)

    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
    )

    val assetCodeLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES - dataLen - Ints.BYTES, bytes.length - Ints.BYTES - dataLen))
    val assetCode: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - assetCodeLen - Ints.BYTES - dataLen, bytes.length - Ints.BYTES - dataLen - Ints.BYTES)
    )

    val hub: PublicKey25519Proposition = PublicKey25519Proposition(
      bytes.slice(bytes.length - Ints.BYTES - assetCodeLen - Ints.BYTES - dataLen - Constants25519.PubKeyLength,
        bytes.length - Ints.BYTES - assetCodeLen - Ints.BYTES - dataLen)
    )

    mAssetTransfer(params._1, params._2, params._3, hub, assetCode, params._4, params._5, data)
  }
}

