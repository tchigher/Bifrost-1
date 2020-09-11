package bifrost.modifier.box.serialization

import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.box.{AssetBox, TokenBox}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.util.Try
import bifrost.modifier.box.serialization.TokenBoxSerializer.tokenBoxdecode
import com.google.common.primitives.Ints

object AssetBoxSerializer extends BifrostSerializer[AssetBox] {

  override def serialize(obj: AssetBox, w: Writer): Unit = {

    TokenBoxSerializer.serialize(obj, w)

    /* assetCode: String */
    w.putIntString(obj.assetCode)

    /* issuer: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.issuer, w)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): AssetBox = {
    val tokenBox: TokenBox = TokenBoxSerializer.parse(r)

    /* putIntString encode String that is shorter than 2147483647 bytes */
    val asset: String = r.getIntString()

    val issuer: PublicKey25519Proposition = PublicKey25519PropositionSerializer.parse(r)
    val data: String = r.getIntString()

    AssetBox(tokenBox.proposition, tokenBox.nonce, tokenBox.value, asset, issuer, data)
  }

  //TODO: Jing - remove
  def decode(bytes: Array[Byte]): Try[AssetBox] = Try {

    val params = tokenBoxdecode(bytes)

    val dataLen = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES))

    val assetLen = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES - Ints.BYTES - dataLen,
      bytes.length - Ints.BYTES - dataLen))
    val asset: String = new String(bytes.slice(bytes.length - (2 * Ints.BYTES) - dataLen - assetLen,
      bytes.length - (2 * Ints.BYTES) - dataLen))

    val issuer: PublicKey25519Proposition = PublicKey25519Proposition(
      bytes.slice(bytes.length - (2 * Ints.BYTES) - dataLen - assetLen - Constants25519.PubKeyLength,
        bytes.length - (2 * Ints.BYTES) - dataLen - assetLen)
    )

    AssetBox(params._1, params._2, params._3, asset, issuer, data)
  }
}