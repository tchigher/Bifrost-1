package bifrost.modifier.box.serialization

import bifrost.modifier.box.{ArbitBox, TokenBox}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.util.Try
import bifrost.modifier.box.serialization.TokenBoxSerializer.tokenBoxdecode

object ArbitBoxSerializer extends BifrostSerializer[ArbitBox] {

  override def serialize(obj: ArbitBox, w: Writer): Unit = {
    TokenBoxSerializer.serialize(obj, w)
  }

  override def parse(r: Reader): ArbitBox = {
    val tokenBox: TokenBox = TokenBoxSerializer.parse(r)
    ArbitBox(tokenBox.proposition, tokenBox.nonce, tokenBox.value)
  }

  //TODO: Jing - remove
  def decode(bytes: Array[Byte]): Try[ArbitBox] = Try {
    val params = tokenBoxdecode(bytes)
    ArbitBox(params._1, params._2, params._3)
  }
}
