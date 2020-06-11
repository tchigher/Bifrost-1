package bifrost.modifier.transaction.bifrostTransaction

import bifrost.crypto.{PrivateKey25519, PrivateKey25519Companion, Signature25519}
import bifrost.modifier.box.{ArbitBox, AssetBox, NoncedBox, PolyBox}
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.Transaction.{Nonce, Value}
import bifrost.wallet.Wallet
import com.google.common.primitives.Longs
import scorex.crypto.encode.Base58

import scala.util.Try

trait mTransferUtil {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def parametersForApply(from: IndexedSeq[(PrivateKey25519, Nonce)],
                         to: IndexedSeq[(PublicKey25519Proposition, Value)],
                         fee: Long,
                         timestamp: Long,
                         txType: String,
                         extraArgs: Any*):
  Try[(IndexedSeq[(PublicKey25519Proposition, Nonce)], IndexedSeq[Signature25519])] = Try {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Array()))

    val undersigned = txType match {
      case "PolyTransfer" => mPolyTransfer(fromPub, to, fakeSigs, fee, timestamp, extraArgs(0).asInstanceOf[String])
      case "ArbitTransfer" => mArbitTransfer(fromPub, to, fakeSigs, fee, timestamp, extraArgs(0).asInstanceOf[String])
      case "AssetTransfer" => mAssetTransfer(
        fromPub,
        to,
        fakeSigs,
        extraArgs(0).asInstanceOf[PublicKey25519Proposition],
        extraArgs(1).asInstanceOf[String],
        fee,
        timestamp,
        extraArgs(2).asInstanceOf[String]
      )
    }

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }
    (fromPub, sigs)
  }

  //noinspection ScalaStyle
  def parametersForCreate(w: Wallet,
                          toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
                          fee: Long,
                          txType: String,
                          publicKeysToSendFrom: Vector[String],
                          publicKeyToSendChangeTo: String,
                          extraArgs: Any*):
  (IndexedSeq[(PrivateKey25519, Long, Long)], IndexedSeq[(PublicKey25519Proposition, Long)]) = {

    toReceive
      .foldLeft((IndexedSeq[(PrivateKey25519, Long, Long)](), IndexedSeq[(PublicKey25519Proposition, Long)]())) {
        case (a, (recipient, amount)) =>
          // Match only the type of boxes specified by txType
          val filteredBoxes: Seq[NoncedBox] = txType match {
            case "PolyTransfer" =>
              if(publicKeysToSendFrom.isEmpty){
                w.boxes().flatMap(_.box match {
                  case p: PolyBox => Some(p)
                  case _ => None
                })}
              else {
                publicKeysToSendFrom.flatMap(p => w.boxesByKey(p).flatMap(_.box match {
                  case p: PolyBox => Some(p)
                  case _ => None
                }))
              }
            case "ArbitTransfer" =>
              if(publicKeysToSendFrom.isEmpty){
                w.boxes().flatMap(_.box match {
                  case a: ArbitBox => Some(a)
                  case _ => None
                })}
              else {
                publicKeysToSendFrom.flatMap(p => w.boxesByKey(p).flatMap(_.box match {
                  case a: ArbitBox => Some(a)
                  case _ => None
                }))
              }
            case "AssetTransfer" =>
              if(publicKeysToSendFrom.isEmpty) {
                w.boxes().flatMap(_.box match {
                  case a: AssetBox
                    if (a.assetCode equals extraArgs(1).asInstanceOf[String]) &&
                      (a.issuer equals extraArgs(0)
                        .asInstanceOf[PublicKey25519Proposition]) =>
                    Some(a)
                  case _ => None
                })
              }
              else {
                publicKeysToSendFrom.flatMap(p => w.boxesByKey(p).flatMap(_.box match {
                  case a: AssetBox
                    if (a.assetCode equals extraArgs(1).asInstanceOf[String]) &&
                      (a.issuer equals extraArgs(0)
                        .asInstanceOf[PublicKey25519Proposition]) =>
                    Some(a)
                  case _ => None
                }))
              }
          }

          val from: IndexedSeq[(PrivateKey25519, Long, Long)] = filteredBoxes
            .flatMap {
              b: NoncedBox =>
                w.secretByPublicImage(b.proposition)
                  .map((_, b.nonce, b.value))
            }
            .toIndexedSeq

          val canSend = from.map(_._3).sum

          var to: IndexedSeq[(PublicKey25519Proposition, Long)] = null

          //Added fix to prevent zero-value change box from being created when entire
          //amount is to be transferred
          if(canSend - amount - fee != 0) {

            var updatedBalance: (PublicKey25519Proposition, Long) = null
            if (publicKeyToSendChangeTo == "") {
              updatedBalance = filteredBoxes.head match {
                case b: NoncedBox =>
                  (b.proposition, canSend - amount - fee)
                case _ => null
              }
            }
            else {
              updatedBalance =
                (PublicKey25519Proposition(Base58.decode(publicKeyToSendChangeTo).get), canSend - amount - fee)
            }
            to = IndexedSeq(updatedBalance, (recipient, amount))

          }
          else {
            to = IndexedSeq((recipient, amount))
          }

          //          val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(updatedBalance, (recipient, amount))

          require(from.map(_._3).sum - to.map(_._2).sum == fee)

          (a._1 ++ from, a._2 ++ to)
      }
  }

  def validateTx(tx: mTransferTransaction): Try[Unit] = Try {
    require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0L))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, tx.messageToSign)
    })

  }
}
