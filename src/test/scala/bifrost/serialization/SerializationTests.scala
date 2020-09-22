package bifrost.serialization

import bifrost.crypto.{PrivateKey25519, PrivateKey25519Companion, Signature25519}
import bifrost.history.History
import bifrost.modifier.ModifierId
import bifrost.modifier.block.{Block, BlockSerializer}
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{MofNProposition, MofNPropositionSerializer, PublicKey25519Proposition}
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.modifier.transaction.serialization._
import bifrost.nodeView.NodeViewHolder.log
import bifrost.program.{ExecutionBuilder, ExecutionBuilderSerializer}
import bifrost.{BifrostGenerators, ValidGenerators}
import io.iohk.iodb.ByteArrayWrapper
import serializer.BloomTopics

import scala.collection.BitSet
import scala.util.{Failure, Success}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.encode.Base58

/**
  * Created by cykoz on 4/12/17.
  */
class SerializationTests extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Serialize a block two times and see if they are the same") {
    val GenesisBalance = 100000000L

    //propositions with wallet seed genesisoo, genesiso1, ..., genesis48, genesis49
    val icoMembers: IndexedSeq[PublicKey25519Proposition] =
      IndexedSeq(
        "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ", "7BDhJv6Wh2MekgJLvQ98ot9xiw5x3N4b3KipURdrW8Ge",
        "Ei8oY3eg5vM26QUBhyFiAdPN1C23RJEV9irrykNmSAFV", "8LNhm5QagL88sWggvJKGDiZ5bBCG4ajV7R6vAKz4czA9",
        "EakiCSw1rfmL5DFTPNmSJZEEAEGtTp3DN12wVPJVsURS", "AEQ8bZRuAxAp8DV9VZnTrSudGPdNyzY2HXjPBCGy8igf",
        "DSL6bvb6j1v6SnvKjqc6fJWdsRjZ85YboH8FkzonUPiT", "419sTmWKAXb5526naQ93xJZL4YAYtpVkbLmzMb6k5X9m",
        "GydWCS1GwExoDNuEiW6fBLYr7cs4vwdLpk1kzDeKHq6A", "G8xVDYow1YcSb4cuAHwcpYSEKxFpYwC9GqYChMvbCWn5",
        "9E4F53GSXMPqwuPWEVoUQe9B1z4A8v9Y6tAQdKK779km", "5XtHBDxXCudA38FJnoWm1BVG8aV67AiQKnPuwYbWZCb3",
        "8Sp3v5vtYtkM9Z2K2B7PuZbWmWQE9bfiUFCvkmsdauGj", "8XTUXeLiHPbMNXedWQh5xHQtq4xUHU3pZZGqRQzC2eyj",
        "ftqJXjSXrWQXmumNVVaRiNB7TZuCy4GCvz9V4GJGhAv", "GMAYWvbBmssCr55m9bcq8cKzfczSKKxidtVrukBM1KFN",
        "3nFprwUuqGH9BpvJMQeCb5AwHdaXuxKin1WSxWc9PTkY", "HfYNA96cGebFGgAhGUbxvRJYyLFchQJZpJTQMXztE6gZ",
        "EPbo8xRWARg2znJAqevKnQMskxnemmCdimPiVFhr8eLd", "4pygr1SPEe5KbU1R8XgMmYaW7YfTH818wd113mF6bhsP",
        "52gwahUytUXv7wfKs4j6YeKeepc38sYsUi4jp4z4jVym", "Hi3Q1ZQbD2zztq6ajm5yUKfFccxmj3yZn79GUjhFvPSW",
        "G1yK5iwPQKNXnqU4Drg83et3gKhRW5CogqiekKEYDcrt", "Hf8XcEAVMCiWbu376rGS48FhwH5NgteivfsTsvX1XpbA",
        "3FAskwxrbqiX2KGEnFPuD3z89aubJvvdxZTKHCrMFjxQ", "GgahaaNBaHRnyUtvEu3k7N5BnW3dvhVCXyxMP6uijdhh",
        "7R9waVeAKuHKNQY5uTYBp6zNLNo6wSDvj9XfQCyRWmDF", "E4AoFDANgDFL83gTS6A7kjWbLmqWcPr6DqEgMG7cqU18",
        "AEkuiLFdudYmUwZ9dSa64rakqUgJZf6pKFFwwm6CZFQz", "3QzGZvvTQbcUdhd5BL9ofEK3GdzbmqUnYA1pYTAdVY44",
        "EjpGvdZETt3SuZpcuwKvZS4jgWCockDHzFQLoeYNW4R", "C85c1uMAiHKDgcqxF6EaiCGQyWgQEYATbpo8M7XEnx3R",
        "8V5y1CSC1gCGD1jai3ns5FJNW7tAzf7BGd4iwmBv7V44", "CJ9udTDT61ckSHMd6YNpjeNdsN2fGwmJ6Ry6YERXmGa7",
        "7eboeRCeeBCFwtzPtB4vKPnaYMPL52BjfiEpqSRWfkgx", "E3JJCTMouTys5BSwFyHTV3Ht55mYWfNUAverrNaVo4jE",
        "9PLHPwnHyA5jf6GPGRjJt7HNd93rw4gWTBi7LBNL4Wwt", "2YM2FQ4HfMiV3LFkiwop2xFznbPVEHbhahVvcrhfZtXq",
        "3oTzYXjwdr684FUzaJEVVuXBztysNgR8M8iV9QykaM9C", "J6bgGpwDMqKFrde2mpdS6dasRyn9WFV6jKgWAkHSN91q",
        "4wtQpa1BVgAt9CA4FUuHZHCYGBYtvudPqa1sAddfAPii", "DaSXwzkAU2WfH39zxMfuXpExsVfKk6JzeYbdW9RLiXr4",
        "6BtXEZE6GcxtEtSLAHXkE3mkcTG1u8WuoQxZG7R8BR5X", "39Z9VaCAeqoWajHyku29argf7zmVqs2vVJM8zYe7YLXy",
        "7focbpSdsNNE4x9h7eyXSkvXE6dtxsoVyZMpTpuThLoH", "CBdnTL6C4A7nsacxCP3VL3TqUokEraFy49ckQ196KU46",
        "CfvbDC8dxGeLXzYhDpNpCF2Ar9Q5LKs8QrfcMYAV59Lt", "GFseSi5squ8GRRkj6RknbGj9Hyz82HxKkcn8NKW1e5CF",
        "FuTHJNKaPTneEYRkjKAC3MkSttvAC7NtBeb2uNGS8mg3", "5hhPGEFCZM2HL6DNKs8KvUZAH3wC47rvMXBGftw9CCA5"
      ).map(s => PublicKey25519Proposition(Base58.decode(s).get))

    val fstGenesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)
    val fstGenesisAccountPriv = fstGenesisAccount._1

    val secGenesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)
    val secGenesisAccountPriv = secGenesisAccount._1

    val fstGenesisTxs = Seq(ArbitTransfer(
      IndexedSeq(fstGenesisAccountPriv -> 0),
      icoMembers.map(_ -> GenesisBalance),
      0L,
      0L,
      "")) ++ Seq(PolyTransfer(
      IndexedSeq(fstGenesisAccountPriv -> 0),
      icoMembers.map(_ -> GenesisBalance),
      0L,
      0L,
      ""))
    log.debug(s"Initialize state with transaction ${fstGenesisTxs.head} with boxes ${fstGenesisTxs.head.newBoxes}")

    val secGenesisTxs = Seq(ArbitTransfer(
      IndexedSeq(secGenesisAccountPriv -> 0),
      icoMembers.map(_ -> GenesisBalance),
      0L,
      0L,
      "")) ++ Seq(PolyTransfer(
      IndexedSeq(secGenesisAccountPriv -> 0),
      icoMembers.map(_ -> GenesisBalance),
      0L,
      0L,
      ""))
    log.debug(s"Initialize state with transaction ${secGenesisTxs.head} with boxes ${secGenesisTxs.head.newBoxes}")
    //YT - commented out below assertion since transaction id generation was changed
    //assert(Base58.encode(genesisTxs.head.id) == "5dJRukdd7sw7cmc8vwSnwbVggWLPV4VHYsZt7AQcFW3B", Base58.encode(genesisTxs.head.id))

    val fstGenesisBox = ArbitBox(fstGenesisAccountPriv.publicImage, 0, GenesisBalance)
    val secGenesisBox = ArbitBox(secGenesisAccountPriv.publicImage, 0, GenesisBalance)

    val version = settings.forgingSettings.version
    val fstBlock = Block.create(ModifierId(History.GenesisParentId), 0L, fstGenesisTxs, fstGenesisBox, fstGenesisAccountPriv, version)
    val secBlock = Block.create(ModifierId(History.GenesisParentId), 0L, secGenesisTxs, secGenesisBox, secGenesisAccountPriv, version)
    /* history = history.append(oneBlock).get._1 */
    println(s"fstblock========${Base58.encode(fstBlock.bytes)}")
    println(s"secblock========${Base58.encode(secBlock.bytes)}")
    fstBlock.bytes sameElements secBlock.bytes shouldBe true
  }

  property("oneOfNProposition Serialization") {
    forAll(oneOfNPropositionGen) {
      case (_, mn: MofNProposition) =>
        val parsed = MofNPropositionSerializer
          .parseBytes(MofNPropositionSerializer.toBytes(mn))
          .get

        parsed.m shouldBe mn.m
        parsed.setOfPubKeyBytes should contain theSameElementsAs mn.setOfPubKeyBytes
    }
  }

  property("PolyBox Serialization") {
    forAll(polyBoxGen) {
      b: PolyBox =>
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("ArbitBox Serialization") {
    forAll(arbitBoxGen) {
      b: ArbitBox =>
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("AssetBox Serialization") {
    forAll(assetBoxGen) {
      b: AssetBox =>
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("StateBox Serialization") {
    forAll(stateBoxGen) {
      b: StateBox =>
        val json = b.json
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        json.as[StateBox].right.get.bytes sameElements BoxSerializer.toBytes(b) shouldBe true
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("CodeBox Serialization") {
    forAll(codeBoxGen) {
      b: CodeBox =>
        val json = b.json
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        json.as[CodeBox].right.get.bytes sameElements BoxSerializer.toBytes(b) shouldBe true
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("ExecutionBox Serialization") {
    forAll(executionBoxGen) {
      b: ExecutionBox =>
        val json = b.json
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        json.as[ExecutionBox].right.get.bytes sameElements BoxSerializer.toBytes(b) shouldBe true
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("ExecutionBuilder Serialization") {
    forAll(validExecutionBuilderGen()) {
      a: ExecutionBuilder =>
        val parsed = ExecutionBuilderSerializer
          .parseBytes(ExecutionBuilderSerializer.toBytes(a))
          .get

        ExecutionBuilderSerializer.toBytes(parsed) sameElements ExecutionBuilderSerializer.toBytes(a) shouldBe true
    }
  }

  property("PolyTransfer Serialization") {
    forAll(polyTransferGen) {
      sc: PolyTransfer =>
        val parsed = PolyTransferSerializer
          .parseBytes(PolyTransferSerializer.toBytes(sc))
          .get

        PolyTransferSerializer.toBytes(parsed) sameElements
          PolyTransferSerializer.toBytes(sc) shouldBe true
    }
  }

  property("ArbitTransfer Serialization") {
    forAll(arbitTransferGen) {
      ac: ArbitTransfer =>
        val parsed = ArbitTransferSerializer
          .parseBytes(ArbitTransferSerializer.toBytes(ac))
          .get

        ArbitTransferSerializer.toBytes(parsed) sameElements
          ArbitTransferSerializer.toBytes(ac) shouldBe true
    }
  }

  property("AssetTransfer Serialization") {
    forAll(assetTransferGen) {
      at: AssetTransfer =>
        val parsed = AssetTransferSerializer
          .parseBytes(AssetTransferSerializer.toBytes(at))
          .get

        AssetTransferSerializer.toBytes(parsed) sameElements
          AssetTransferSerializer.toBytes(at) shouldBe true
    }
  }

  property("ProgramCreation Serialization") {
    forAll(programCreationGen) {
      c: ProgramCreation =>
        val parsed = ProgramCreationSerializer
          .parseBytes(ProgramCreationSerializer.toBytes(c))
          .get

        val parsedBytes = ProgramCreationSerializer.toBytes(parsed)
        val directParsedBytes = ProgramCreationSerializer.toBytes(c)

        c.executionBuilder shouldEqual parsed.asInstanceOf[ProgramCreation].executionBuilder
        c.json shouldEqual parsed.asInstanceOf[ProgramCreation].json

        parsedBytes sameElements directParsedBytes shouldBe true
    }
  }

  property("ProgramMethodExecution Serialization") {
    forAll(programMethodExecutionGen) {
      c: ProgramMethodExecution =>
        val parsed = ProgramMethodExecutionSerializer
          .parseBytes(ProgramMethodExecutionSerializer.toBytes(c))
          .get

        ProgramMethodExecutionSerializer.toBytes(parsed) sameElements
          ProgramMethodExecutionSerializer.toBytes(c) shouldBe true
    }
  }

  property("AssetCreation Serialization") {
    forAll(assetCreationGen) {
      ac: AssetCreation =>
        val parsed: AssetCreation = AssetCreationSerializer
          .parseBytes(AssetCreationSerializer.toBytes(ac))
          .get

        AssetCreationSerializer.toBytes(parsed) sameElements
          AssetCreationSerializer.toBytes(ac) shouldBe true
    }
  }

  property("CodeCreation Serialization") {
    forAll(codeBoxCreationGen) {
      ccc: CodeCreation =>
        val parsed = CodeBoxCreationSerializer
          .parseBytes(CodeBoxCreationSerializer.toBytes(ccc))
          .get

        CodeBoxCreationSerializer.toBytes(parsed) sameElements
          CodeBoxCreationSerializer.toBytes(ccc) shouldBe true
    }
  }

  property("ProgramTransfer Serialization") {
    forAll(programTransferGen) {
      pt: ProgramTransfer =>
        val parsed = ProgramTransferSerializer
          .parseBytes(ProgramTransferSerializer.toBytes(pt))
          .get

        ProgramTransferSerializer.toBytes(parsed) sameElements
          ProgramTransferSerializer.toBytes(pt) shouldBe true
    }
  }

  //TODO Test after all txs and state tests work
  property("Block Serialization") {
    forAll(BlockGen) {
      bb: Block =>
        val parsed = BlockSerializer.parseBytes(BlockSerializer.toBytes(bb))

        parsed match {
          case Success(p) => BlockSerializer.toBytes(p) sameElements
            BlockSerializer.toBytes(bb) shouldBe true
          case Failure(e) => throw e
        }
    }
  }


  // todo: JAA - 2020.08.02 - this was removed as SyncInfo uses the standard message parsing pattern now.
  // todo:       We should be sure to move this test to where ever message serialization is tested
//  property("BifrostSyncInfo Serialization") {
//    forAll(bifrostSyncInfoGen) {
//      syncInfo: BifrostSyncInfo =>
//        val parsed = BifrostSyncInfoSerializer
//          .parseBytes(BifrostSyncInfoSerializer.toBytes(syncInfo))
//          .get
//
//        BifrostSyncInfoSerializer.toBytes(parsed) sameElements
//          BifrostSyncInfoSerializer.toBytes(syncInfo) shouldBe true
//    }
//  }

  property("Bloom Serialization") {
    forAll(intSeqGen) {
      intSeq: Seq[Int] =>
        val parsed = BitSet() ++ BloomTopics
          .parseFrom(BloomTopics(intSeq).toByteArray)
          .topics

        BloomTopics(parsed.toSeq).toByteArray sameElements
          BloomTopics((BitSet() ++ intSeq).toSeq).toByteArray shouldBe true
    }
  }
}
