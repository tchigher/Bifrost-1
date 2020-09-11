package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction.TransferTransaction
import scala.util.Try
import com.google.common.primitives.Ints

//TODO: Jing - remove
object TransferTransactionSerializer {
  def decode(bytes: Array[Byte]): Try[TransferTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    require(typeStr == "TransferTransaction")

    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.slice(0, Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES, Ints.BYTES + newTypeLength))

    newTypeStr match {
      case "PolyTransfer" => PolyTransferSerializer.parseBytes(newBytes).get
      case "ArbitTransfer" => ArbitTransferSerializer.parseBytes(newBytes).get
      case "AssetTransfer" => AssetTransferSerializer.parseBytes(newBytes).get
    }
  }
}
