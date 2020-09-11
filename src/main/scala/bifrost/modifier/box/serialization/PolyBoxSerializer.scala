package bifrost.modifier.box.serialization

import bifrost.modifier.box.{TokenBox, PolyBox}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.util.Try
import bifrost.modifier.box.serialization.TokenBoxSerializer.tokenBoxdecode

object PolyBoxSerializer extends BifrostSerializer[PolyBox] {

  override def serialize(obj: PolyBox, w: Writer): Unit = {
    TokenBoxSerializer.serialize(obj, w)
  }

  override def parse(r: Reader): PolyBox = {
    val tokenBox: TokenBox = TokenBoxSerializer.parse(r)
    PolyBox(tokenBox.proposition, tokenBox.nonce, tokenBox.value)
  }

  //TODO: Jing - remove
  def decode(bytes: Array[Byte]): Try[PolyBox] = Try {
    val params = tokenBoxdecode(bytes)
    PolyBox(params._1, params._2, params._3)
  }
}
