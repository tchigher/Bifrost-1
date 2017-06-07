package bifrost.state

import java.io.File
import java.time.Instant

import bifrost.{BifrostGenerators, BifrostNodeViewHolder, ValidGenerators}
import com.google.common.primitives.{Ints, Longs}
import bifrost.blocks.BifrostBlock
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.state.BifrostState
import bifrost.transaction._
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.MofNPropositionSerializer
import bifrost.wallet.{BWallet, PolyTransferGenerator}
import io.circe
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalacheck.Gen
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.{Failure, Try}

class BifrostStateSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators
  with BeforeAndAfterAll{

  val settingsFilename = "settings.json"
  lazy val testSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  val gs = BifrostNodeViewHolder.initializeGenesis(testSettings)
  val history = gs._1; var genesisState = gs._2; var gw = gs._3
  // Generate new secret
  gw.generateNewSecret(); gw.generateNewSecret()
  val genesisBlockId = genesisState.version

  property("A block with valid contract creation will result in a contract entry and updated poly boxes in the LSMStore") {
    // Create block with contract creation
    forAll(validContractCreationGen) {
      cc: ContractCreation =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cc)
        )

        val preExistingPolyBoxes: Set[BifrostBox] = cc.feePreBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val box = cc.newBoxes.head.asInstanceOf[ContractBox]
        val deductedFeeBoxes: Traversable[PolyBox] = cc.newBoxes.tail.map {
          case p: PolyBox => p
          case _ => throw new Exception("Was expecting PolyBoxes but found something else")
        }

        val boxBytes = ContractBoxSerializer.toBytes(box)

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes,
          Instant.now.toEpochMilli
        )

        val preparedState = genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.applyChanges(preparedState.changes(block).get, Ints.toByteArray(2)).get

        require(newState.storage.get(ByteArrayWrapper(box.id)) match {
          case Some(wrapper) => wrapper.data sameElements boxBytes
          case None => false
        })

        require(deductedFeeBoxes.forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)) match {
          case Some(wrapper) => wrapper.data sameElements PolyBoxSerializer.toBytes(pb)
          case None => false
        }))

        /* Checks that the total sum of polys returned is total amount submitted minus total fees */
        require(deductedFeeBoxes.map(_.value).sum == preExistingPolyBoxes.map { case pb: PolyBox => pb.value }.sum - cc.fee)

        /* Checks that the amount returned in polys is equal to amount sent in less fees */
        require(cc.fees.forall(p => deductedFeeBoxes.filter(_.proposition equals p._1).map(_.value).sum == cc.feePreBoxes(p._1).map(_._2).sum - cc.fees(p._1)))

        /* Expect none of the prexisting boxes to still be around */
        require(preExistingPolyBoxes.forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)).isEmpty))

        genesisState = newState.rollbackTo(genesisBlockId).get

    }
  }

  //noinspection ScalaStyle
  property("A block with valid PolyTransfer should result in more funds for receiver, less for transferrer") {
    // Create genesis block, add to state
    // Create new block with PolyTransfer
    // send new block to state
    // check updated state
    val beforePolyBoxes = gw.boxes().filter(_.box match {
      case a: PolyBox => genesisState.closedBox(a.id).isDefined
      case _ => false
    }).map(_.box.asInstanceOf[PolyBox])
    val beforeBoxKeys = beforePolyBoxes.flatMap(b => gw.secretByPublicImage(b.proposition).map(s => (b, s)))
    assert(beforeBoxKeys.map(_._1.value).sum == 100000000L)

    forAll(Gen.choose(0, 500)) { num: Int =>
      val poT = PolyTransferGenerator.generateStatic(gw).get
      val block = BifrostBlock(
        Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
        Instant.now().toEpochMilli,
        ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
        Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
        Seq(poT)
      )

      require(genesisState.validate(poT).isSuccess)

      val newState = genesisState.applyChanges(genesisState.changes(block).get, Ints.toByteArray(2)).get
      val newWallet = gw.scanPersistent(block)

      val arbitBoxes = newWallet.boxes().filter(_.box match {
        case a: ArbitBox => newState.closedBox(a.id).isDefined
        case _ => false
      }).map(_.box.asInstanceOf[ArbitBox])

      val boxKeys = arbitBoxes.flatMap(b => newWallet.secretByPublicImage(b.proposition).map(s => (b, s)))
      require(boxKeys.map(_._1.value).sum == 100000000L)

      val polyBoxes = newWallet.boxes().filter(_.box match {
        case a: PolyBox => newState.closedBox(a.id).isDefined
        case _ => false
    }).map(_.box.asInstanceOf[PolyBox])
      val polyBoxKeys = polyBoxes.flatMap(b => newWallet.secretByPublicImage(b.proposition).map(s => (b, s)))
      // The resulting poly balance = genesis amount - fee, because the transaction is sent to self
      // TODO: No fee is actually collected due to the reward box is an ArbitBox
      require(polyBoxKeys.map(_._1.value).sum == 100000000L - poT.fee)

      genesisState = newState.rollbackTo(genesisBlockId).get
      gw = newWallet.rollback(genesisBlockId).get
    }
  }

  property("A block with valid ProfileTransaction should result in a ProfileBox") {
    val timestamp = System.currentTimeMillis()
    val signature = PrivateKey25519Companion.sign(gw.secrets.head,
      ProfileTransaction.messageToSign(timestamp, gw.secrets.head.publicImage,
        Map("role" -> "investor")))
    val tx = ProfileTransaction(gw.secrets.head.publicImage, signature, Map("role" -> "investor"), 0L, timestamp)

    val block = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(tx)
    )

    require(genesisState.validate(tx).isSuccess)

    val newState = genesisState.applyChanges(genesisState.changes(block).get, Ints.toByteArray(4)).get
    val box = newState.closedBox(FastCryptographicHash(gw.secrets.head.publicKeyBytes ++ "role".getBytes)).get.asInstanceOf[ProfileBox]

    box.key shouldBe "role"
    box.value shouldBe "investor"
  }

  property("Attempting to validate a contract creation tx without valid signatures should error") {
    forAll(validContractCreationGen) {
      cc: ContractCreation =>

        val wrongSig: Array[Byte] = (cc.signatures.head._2.bytes.head + 1).toByte +: cc.signatures.head._2.bytes.tail
        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = cc.signatures + (cc.signatures.head._1 -> Signature25519(wrongSig))
        val invalidCC = cc.copy(signatures = wrongSigs)

        val preExistingPolyBoxes: Set[BifrostBox] = cc.feePreBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          Instant.now.toEpochMilli
        )

        val preparedState = genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(invalidCC)

        newState shouldBe a[Failure[_]]

        genesisState = preparedState.rollbackTo(genesisBlockId).get
    }
  }

  property("Attempting to validate a contract creation tx with a timestamp that is before the last block timestamp should error") {

  }

  property("Attempting to validate a contract creation tx with a timestamp that is in the future should error") {

  }

  property("Attempting to validate a contract creation tx with the same signature as an existing contract should error") {
    // Create invalid contract creation
    // send tx to state
  }

  property("Attempting to validate a contract creation tx with a timestamp too far in the future should error") {
    // Create invalid contract creation
    // send tx to state
  }

  property("Attempting to validate a PolyTransfer for amount you do not have should error") {
    // Create invalid PolyTransfer
    // send tx to state
  }

  override def afterAll() {
    history.storage.storage.close()
    val path: Path = Path ("/tmp")
    Try(path.deleteRecursively())
  }
}