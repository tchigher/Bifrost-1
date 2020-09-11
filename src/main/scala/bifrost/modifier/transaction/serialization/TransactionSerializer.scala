package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.util.Try
import com.google.common.primitives.Ints

object TransactionSerializer extends BifrostSerializer[Transaction] {

  override def serialize(obj: Transaction, w: Writer): Unit = {
    obj match {
      case obj: ProgramCreation =>
        w.putByteString("ProgramCreation")
        ProgramCreationSerializer.serialize(obj, w)
      case obj: ProgramMethodExecution =>
        w.putByteString("ProgramMethodExecution")
        ProgramMethodExecutionSerializer.serialize(obj, w)

      case obj: ProgramTransfer =>
        w.putByteString("ProgramTransfer")
        ProgramTransferSerializer.serialize(obj, w)

      case obj: PolyTransfer =>
        w.putByteString("PolyTransfer")
        PolyTransferSerializer.serialize(obj, w)
      case obj: ArbitTransfer =>
        w.putByteString("ArbitTransfer")
        ArbitTransferSerializer.serialize(obj, w)
      case obj: AssetTransfer =>
        w.putByteString("AssetTransfer")
        AssetTransferSerializer.serialize(obj, w)

      case obj: AssetCreation =>
        w.putByteString("AssetCreation")
        AssetCreationSerializer.serialize(obj, w)
      case obj: CoinbaseTransaction =>
        w.putByteString("CoinbaseTransaction")
        CoinbaseTransactionSerializer.serialize(obj, w)
    }
  }

  override def parse(r: Reader): Transaction = {
    r.getByteString() match {
      case "ProgramCreation" => ProgramCreationSerializer.parse(r)
      case "ProgramMethodExecution" => ProgramMethodExecutionSerializer.parse(r)

      case "ProgramTransfer" => ProgramTransferSerializer.parse(r)

      case "PolyTransfer" => PolyTransferSerializer.parse(r)
      case "ArbitTransfer" => ArbitTransferSerializer.parse(r)
      case "AssetTransfer" => AssetTransferSerializer.parse(r)

      case "AssetCreation" => AssetCreationSerializer.parse(r)
      case "CoinbaseTransaction" => CoinbaseTransactionSerializer.parse(r)
    }
  }

  //TODO: Jing - remove
  def decode(bytes: Array[Byte]): Try[Transaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.slice(0, Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    typeStr match {
      case "ProgramTransaction" => ProgramTransactionSerializer.decode(bytes).get
      case "ProgramTransfer" => ProgramTransferSerializer.decode(bytes).get
      case "TransferTransaction" => TransferTransactionSerializer.decode(bytes).get
      case "AssetCreation" => AssetCreationSerializer.decode(bytes).get
      case "CoinbaseTransaction" => CoinbaseTransactionSerializer.decode(bytes).get
    }
  }
}
