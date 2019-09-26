package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.bifrostTransaction
import bifrost.transaction.bifrostTransaction.ProgramMethodExecution
import bifrost.transaction.box.ExecutionBoxSerializer
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints}
import io.circe.Json
import io.circe.parser.parse

import scala.util.Try

object ProgramMethodExecutionCompanion extends Serializer[ProgramMethodExecution] {

  override def toBytes(cme: ProgramMethodExecution): Array[Byte] = {
    ProgramTransactionCompanion.prefixBytes ++ toChildBytes(cme)
  }

  def toChildBytes(cme: ProgramMethodExecution): Array[Byte] = {
    val typeBytes = "ProgramMethodExecution".getBytes

    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(cme.executionBox.bytes.length),
      Ints.toByteArray(cme.methodName.getBytes.length),
      Ints.toByteArray(cme.methodParams.noSpaces.getBytes.length),
      cme.executionBox.bytes,
      cme.methodName.getBytes,
      cme.methodParams.noSpaces.getBytes,
      Ints.toByteArray(cme.data.getBytes.length),
      cme.data.getBytes,
      ProgramTransactionCompanion.commonToBytes(cme)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ProgramMethodExecution] = Try {
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    var numReadBytes = Ints.BYTES + typeLength

    val Array(executionBoxLength: Int, methodNameLength: Int, parametersLength: Int) =
      (0 until 3).map { i =>
        Ints.fromByteArray(bytes.slice(numReadBytes + i * Ints.BYTES, numReadBytes + (i + 1) * Ints.BYTES))
      }.toArray

    numReadBytes += 3 * Ints.BYTES

    val executionBox = ExecutionBoxSerializer.parseBytes(bytes.slice(numReadBytes,
      numReadBytes + executionBoxLength))
      .get

    numReadBytes += executionBoxLength

    val methodName = new String(bytes.slice(numReadBytes, numReadBytes + methodNameLength))

    numReadBytes += methodNameLength

    val methodParams: Json = parse(new String(bytes.slice(numReadBytes,
      numReadBytes + parametersLength))) match {
      case Left(f) => throw f
      case Right(j: Json) => j
    }

    numReadBytes += parametersLength

    val dataLen: Int = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    val data: String = new String(bytes.slice(numReadBytes, numReadBytes + dataLen))

    numReadBytes += dataLen

    val (owner: PublicKey25519Proposition,
    signatures: Map[PublicKey25519Proposition, Signature25519],
    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
    fees: Map[PublicKey25519Proposition, Long],
    timestamp: Long) = ProgramTransactionCompanion.commonParseBytes(bytes.slice(numReadBytes,
      bytes.length))

    bifrostTransaction.ProgramMethodExecution(executionBox, methodName,
      methodParams, owner, signatures, feePreBoxes, fees, timestamp, data)
  }

}
