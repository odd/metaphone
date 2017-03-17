package oddco

import java.io.{File, FilenameFilter}

object MetaphoneTest extends App {
  def loadTestData(variant: String): Iterator[(String, String, String)] = {
    val path = "/" + variant + "-data.txt"
    io.Source.fromInputStream(getClass.getResourceAsStream(path)).getLines.flatMap { line =>
      val tab = line.indexOf('\t')
      val comment = line.indexOf('#')
      if (tab == -1) Seq.empty
      else if (comment > -1) Seq((path, line.substring(0, tab), line.substring(tab + 1, comment).trim.toUpperCase))
      else Seq((path, line.substring(0, tab), line.substring(tab + 1).trim.toUpperCase))
    }
  }
  val variants = if (args.nonEmpty) args else {
    new File("./src/test/resources").listFiles(new FilenameFilter() {
      override def accept(dir: File, name: String): Boolean = name.endsWith("-data.txt")
    }).map { f =>
      f.getName.substring(0, f.getName.length - "-data.txt".length)
    }
  }
  val errors = variants.flatMap(variant => loadTestData(variant)).map {
    case t @ (file, word, expected) => (file, word, expected, Metaphone.translate(word))
  }.collect {
    case t @ (file, word, expected, actual) if actual != expected => t
  }
  if (errors.isEmpty) println("All tests passed for: " + variants.mkString(", ")) else {
    println("Errors found:")
    errors.foreach(println)
  }
}