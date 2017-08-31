package op.assement.aptp

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.io.Source

object Main extends App {
  import Merging.RowSeries
  import Series.SeriesIteratorOps

  val datePattern = "^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])"
  val numberPattern = "[0-9]+"
  val rowRegex = s"$datePattern:$numberPattern".r
  val colonRegex = s"(.*):(.*)".r

  if (args.length < 2) {
    System.err.println("Wrong arguments. Usage: <input file1> [<input file2>, [...]] <result file>")
    System.exit(1)
  }

  val inputFiles = args.init
  val resultFile = args.last

  val allSeries = inputFiles map { fileName =>
    val lines = Source.fromFile(fileName, "ASCII").getLines()
    lines filter isValid map toRow
  }
  allSeries
    .reduce(_ :+: _)
    .map(r => s"${r._1}:${r._2}")
    .outToFile(Paths.get(resultFile))

  private def isValid(s: String): Boolean = {
    rowRegex.pattern.matcher(s).matches
  }

  private def toRow(s: String): RowSeries = {
    val colonRegex(d, n) = s
    (d, n.toInt)
  }

  private implicit class IteratorToFileOps[T](
      val items: Iterator[T]
    ) extends AnyVal {

    def outToFile(path: Path): Unit = {
      val writer = Files.newBufferedWriter(path, StandardCharsets.US_ASCII)
      items foreach { item =>
        writer.write(item.toString)
        writer.newLine()
      }
      writer.close()
    }
  }
}


