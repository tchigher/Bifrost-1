package bifrost.modifier.box.serialization

import bifrost.modifier.box.TokenBox
import bifrost.modifier.box.proposition.PublicKey25519PropositionSerializer
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

import bifrost.modifier.box.proposition.{PublicKey25519Proposition, Constants25519}
import com.google.common.primitives.{Ints, Longs}


object TokenBoxSerializer extends BifrostSerializer[TokenBox] {

  override def serialize(obj: TokenBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    w.putULong(obj.value)
  }

  override def parse(r: Reader): TokenBox = {
    new TokenBox(
      PublicKey25519PropositionSerializer.parse(r),
      r.getLong(),
      r.getULong()
    )
  }

  //TODO: Jing - remove
  def tokenBoxdecode(bytes: Array[Byte]): (PublicKey25519Proposition, Long, Long) = {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    require(typeStr.nonEmpty)

    val numReadBytes = Ints.BYTES + typeLen

    val pk = PublicKey25519Proposition(bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))
    val nonce = Longs.fromByteArray(bytes.slice(numReadBytes + Constants25519.PubKeyLength,
                                                numReadBytes + Constants25519.PubKeyLength + Longs.BYTES))

    val curReadBytes = numReadBytes + Constants25519.PubKeyLength + Longs.BYTES

    val value = Longs.fromByteArray(bytes.slice(curReadBytes, curReadBytes + Longs.BYTES))
    (pk, nonce, value)
  }
}
