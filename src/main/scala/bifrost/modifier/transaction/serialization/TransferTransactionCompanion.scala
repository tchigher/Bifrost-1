package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction.{ArbitTransfer, AssetTransfer, PolyTransfer, TransferTransaction, mArbitTransfer, mAssetTransfer, mPolyTransfer, mTransferTransaction}
import bifrost.serialization.Serializer
import com.google.common.primitives.Ints

import scala.util.Try

object TransferTransactionCompanion extends Serializer[mTransferTransaction] {
  val typeBytes = "TransferTransaction".getBytes

  val prefixBytes = Ints.toByteArray(typeBytes.length) ++ typeBytes

  def toBytes(m: TransferTransaction): Array[Byte] = {
    prefixBytes ++
      (m match {
        case sc: PolyTransfer => PolyTransferCompanion.toChildBytes(sc)
        case ac: ArbitTransfer => ArbitTransferCompanion.toChildBytes(ac)
        case at: AssetTransfer => {
          val sigs = at.signatures.map(_._2).toIndexedSeq
          val asset = new mAssetTransfer(at.from, at.to, sigs, at.issuer, at.assetCode, at.fee, at.timestamp, at.data)
          mAssetTransferCompanion.toChildBytes(asset)
        }
        case mpt: mPolyTransfer => mPolyTransferCompanion.toChildBytes(mpt)
        case mat: mArbitTransfer => mArbitTransferCompanion.toChildBytes(mat)
        case mast: mAssetTransfer => mAssetTransferCompanion.toChildBytes(mast)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[mTransferTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    require(typeStr == "TransferTransaction")

    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.slice(0, Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES, Ints.BYTES + newTypeLength))

    newTypeStr match {
      case "PolyTransfer" => mPolyTransferCompanion.parseBytes(newBytes).get
      case "ArbitTransfer" => mArbitTransferCompanion.parseBytes(newBytes).get
      case "AssetTransfer" => mAssetTransferCompanion.parseBytes(newBytes).get
    }
  }

  override def toBytes(obj: mTransferTransaction): Array[Byte] = ???
}
