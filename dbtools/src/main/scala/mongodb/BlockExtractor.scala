package mongodb

import java.io.{File, PrintWriter}

import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import io.circe


class BlockExtractor {

  val settings: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("toplnet.json")
  }
  val history: BifrostHistory = BifrostHistory.readOrGenerate(settings)

  val jsonDir = new File(s"${settings.dataDirOpt.get}/json")
  jsonDir.mkdirs()
  var tempFile = new File(s"$jsonDir/data0.json")
  var pw = new PrintWriter(tempFile)
  var block = history.bestBlock
  var height = history.height


  while(height >= 0) {
    if(height % 100000 == 0 && height / 100000 != 0) {
      pw.close()
      tempFile = new File(s"$jsonDir/data${height / 100000}.json")
      pw = new PrintWriter(tempFile)
    }

    pw.write(block.json.toString)

    println(s"${block.json}")

    block = history.modifierById(block.parentId).get
    height -= 1
  }

  pw.close()
}
