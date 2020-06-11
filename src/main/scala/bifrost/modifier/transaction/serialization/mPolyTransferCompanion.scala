package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction.{PolyTransfer, mPolyTransfer}
import bifrost.serialization.Serializer
import com.google.common.primitives.Ints

import scala.util.Try

object mPolyTransferCompanion extends Serializer[mPolyTransfer] with mTransferSerializer {

  override def toBytes(sc: mPolyTransfer): Array[Byte] = {
    TransferTransactionCompanion.prefixBytes ++ toChildBytes(sc)
  }

  def toChildBytes(sc: mPolyTransfer): Array[Byte] = {
    transferToBytes(sc, "PolyTransfer") ++
      sc.data.getBytes ++
      Ints.toByteArray(sc.data.getBytes.length)
  }

  override def parseBytes(bytes: Array[Byte]): Try[mPolyTransfer] = Try {
    val params = parametersParseBytes(bytes)
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
    )
    mPolyTransfer(params._1, params._2, params._3, params._4, params._5, data)
  }
}

