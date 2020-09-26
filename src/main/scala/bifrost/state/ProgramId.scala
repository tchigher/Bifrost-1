package bifrost.state

import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.ModifierId
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.Ints
import scorex.crypto.encode.Base58

import scala.util.{ Failure, Success, Try }

case class ProgramId (hashBytes: Array[Byte]) {

  require(hashBytes.length == ProgramId.size)

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[ModifierId] &&
      java.util.Arrays.equals(hashBytes, o.asInstanceOf[ModifierId].hashBytes)
  }

  override def toString: String = Base58.encode(hashBytes)
}


object ProgramId extends BifrostSerializer[ProgramId] {

  val size = 32; // number of bytes in identifier,

  def apply(id: String): Try[ProgramId] = {
    Try {
      Base58.decode(id) match {
        case Success(id) => new ProgramId(id)
        case Failure(ex) => throw ex
      }
    }
  }

  def create(): ProgramId = {
    val key = UUID.randomUUID()
    val keyBytes = BigInt(key.getMostSignificantBits).toByteArray ++ BigInt(key.getLeastSignificantBits).toByteArray

    new ProgramId(FastCryptographicHash(keyBytes))

  }

  override def serialize(obj: ProgramId, w: Writer): Unit = {
    w.putBytes(obj.hashBytes)
  }

  override def parse(r: Reader): ProgramId = {
    ProgramId(r.getBytes(size))
  }
}