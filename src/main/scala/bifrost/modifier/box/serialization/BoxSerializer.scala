package bifrost.modifier.box.serialization

import bifrost.modifier.box._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.util.Try
import com.google.common.primitives.Ints

object BoxSerializer extends BifrostSerializer[Box] {

  override def serialize(obj: Box, w: Writer): Unit = {
    obj match {
      case obj: PolyBox =>
        w.putByteString("PolyBox")
        PolyBoxSerializer.serialize(obj, w)
      case obj: ArbitBox =>
        w.putByteString("ArbitBox")
        ArbitBoxSerializer.serialize(obj, w)
      case obj: AssetBox =>
        w.putByteString("AssetBox")
        AssetBoxSerializer.serialize(obj, w)
      case obj: StateBox =>
        w.putByteString("StateBox")
        StateBoxSerializer.serialize(obj, w)
      case obj: CodeBox =>
        w.putByteString("CodeBox")
        CodeBoxSerializer.serialize(obj, w)
      case obj: ExecutionBox =>
        w.putByteString("ExecutionBox")
        ExecutionBoxSerializer.serialize(obj, w)
      case _ => throw new Exception("Unanticipated Box type")
    }
  }

  override def parse(r: Reader): Box = {
    r.getByteString() match {
      case "ArbitBox" => ArbitBoxSerializer.parse(r)
      case "AssetBox" => AssetBoxSerializer.parse(r)
      case "PolyBox" => PolyBoxSerializer.parse(r)
      case "StateBox" => StateBoxSerializer.parse(r)
      case "CodeBox" => CodeBoxSerializer.parse(r)
      case "ExecutionBox" => ExecutionBoxSerializer.parse(r)
      case _ => throw new Exception("Unanticipated Box Type")
    }
  }


  // TODO: Jing - remove
  def decode(bytes: Array[Byte]): Try[Box] = {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    typeStr match {
      case "ArbitBox" => ArbitBoxSerializer.decode(bytes)
      case "AssetBox" => AssetBoxSerializer.decode(bytes)
      case "PolyBox" => PolyBoxSerializer.decode(bytes)
      case "StateBox" => StateBoxSerializer.decode(bytes)
      case "CodeBox" => CodeBoxSerializer.decode(bytes)
      case "ExecutionBox" => ExecutionBoxSerializer.decode(bytes)
      case _ => throw new Exception("Unanticipated Box Type")
    }
  }
}
