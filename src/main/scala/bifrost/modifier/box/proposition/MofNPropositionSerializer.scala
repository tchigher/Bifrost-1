package bifrost.modifier.box.proposition

import bifrost.utils.Extensions._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object MofNPropositionSerializer extends BifrostSerializer[MofNProposition] {

  override def serialize(obj: MofNProposition, w: Writer): Unit = {
    /* m: Int */
    w.putUInt(obj.m)

    /* setOfPubKeyBytes: Set[Array[Byte]] */
    w.putUInt(obj.setOfPubKeyBytes.size)
    obj.setOfPubKeyBytes.foreach(b => w.putBytes(b))
  }

  override def parse(r: Reader): MofNProposition = {
    val m: Int = r.getUInt().toIntExact
    val n: Int = r.getUInt().toIntExact
    val setOfPubKeyBytes: Set[Array[Byte]] = (0 until n).map(_ => r.getBytes(Constants25519.PubKeyLength)).toSet

    MofNProposition(m, setOfPubKeyBytes)
  }
}
