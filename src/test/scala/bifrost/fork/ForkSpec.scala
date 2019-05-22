
package bifrost.fork

import bifrost.BifrostNodeViewHolder
import bifrost.BifrostNodeViewHolder.{HIS, MP, MS, VL}
import bifrost.blocks.BifrostBlock
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.transaction.box.ArbitBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.validation.DifficultyBlockValidator
import io.circe
import io.circe.syntax._
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.{Try, Success, Failure}

class ForkSpec extends PropSpec
  with Matchers
{

  val path: Path = Path("/tmp/scorex/test-data")
  Try(path.deleteRecursively())

  val settingsFilename = "testSettings.json"
  lazy val testSettings_version3: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  lazy val testSettings_version0: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename) +
      ("version" -> (List(0,0,0).asJson)) +
      ("forkHeight" -> 3.asJson)

  }

  val gs: (HIS, MS, VL, MP) = BifrostNodeViewHolder.initializeGenesis(testSettings_version0)
  var history: HIS = gs._1
  var genesisState: MS = gs._2
  var gw: VL = gs._3

  var test_height: Long = 0L
  var first_version3_block: BifrostBlock = null


  property("Appending version3 blocks after height = forkHeight should work") {

    for(i <- 2L to testSettings_version0.forkHeight) {
      val tempBlock = BifrostBlock(history.bestBlockId,
        System.currentTimeMillis(),
        ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
        Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
        Seq(),
        0L,
        testSettings_version0.version)

      history = history.append(tempBlock).get._1
      assert(history.modifierById(tempBlock.id).isDefined)
    }

    val tempBlock_version3_1 = BifrostBlock(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version3.version)

    first_version3_block = tempBlock_version3_1

    history = history.append(tempBlock_version3_1).get._1
    assert(history.modifierById(tempBlock_version3_1.id).isDefined)

    val tempBlock_version3_2 = BifrostBlock(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version3.version)

    history = history.append(tempBlock_version3_2).get._1
    assert(history.modifierById(tempBlock_version3_2.id).isDefined)

    history.height shouldEqual testSettings_version0.forkHeight + 2

    test_height = history.height

  }

  property("Appending version3 blocks after height = forkHeight and then appending a version0 block should fail") {

    val tempBlock_version0 = BifrostBlock(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version0.version)

    val heightBeforeAppendAttempt = history.height

//    history = history.append(tempBlock_version0).get._1

    val appendResult = history.append(tempBlock_version0).get
    appendResult match {
      case (historyInstance, progressInfo) => history = historyInstance
      case _ =>
    }
    

    history.modifierById(tempBlock_version0.id).isDefined shouldBe false

    val heightAfterAppendAttempt = history.height

    //Since block validation does not exist block is still appended to history, failure only pops up
    //when trying to recreate a block from id when updating difficulty in DifficultyBlockValidator

    //Hence failure pops up when trying to append a new block on top of an incorrect block

    //heightBeforeAppendAttempt shouldEqual heightAfterAppendAttempt, but is not the case due to above reason
    //manually rolling back storage, recreating history, and then checking height

    //    heightBeforeAppendAttempt shouldEqual heightAfterAppendAttempt

    history.storage.rollback(tempBlock_version0.parentId)
    history = new BifrostHistory(history.storage,
      testSettings_version3,
      Seq(
        new DifficultyBlockValidator(history.storage)
        //new ParentBlockValidator(storage),
        //new SemanticBlockValidator(FastCryptographicHash)
      ))

    history.height shouldEqual test_height

  }

  property("Appending version0 blocks after height = forkHeight should fail") {

    history.storage.rollback(first_version3_block.parentId)
    history = new BifrostHistory(history.storage,
      testSettings_version3,
      Seq(
        new DifficultyBlockValidator(history.storage)
        //new ParentBlockValidator(storage),
        //new SemanticBlockValidator(FastCryptographicHash)
      ))

    //Even though two blocks are being removed, bestBlockId is the topmost block once history is recreated so
    //it's height now represents the actual number of blocks in the chain
    history.height shouldEqual testSettings_version0.forkHeight

    assert(history.height == testSettings_version0.forkHeight)

    val tempBlock_version0 = BifrostBlock(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version0.version)

    history = history.append(tempBlock_version0).get._1

    history.modifierById(tempBlock_version0.id).isDefined shouldBe false
  }

}

