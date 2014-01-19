package tk.monnef.mcmapper

import java.io.File
import tk.monnef.mcmapper.Utils._

object McMapper {
  implicit val fNames = DefaultInputFileNames

  private def prepareFile(path: String): File = {
    val f = new File(path)
    if (!f.exists()) throw new McMapperException(s"Cannot find '$path'; current path '${getCurrentPath()}'.")
    if (!f.canRead) throw new McMapperException(s"Cannot read '$path'; current path '${getCurrentPath()}'.")
    f
  }

  def load(path: String)(implicit filesNames: InputFilesNames): MappingDatabase = {
    var f: File = null
    try {
      f = new File(path)
    } catch {
      case e: Throwable => throw new McMapperException(s"Cannot open target directory '$path'", e)
    }
    load(f)(filesNames)
  }

  def load(path: File)(implicit filesNames: InputFilesNames): MappingDatabase = {
    val pathString = path.getAbsolutePath + "/"
    val fieldsRaw = CSVReader read prepareFile(pathString + fNames.fieldsCSV)
    val methodsRaw = CSVReader read prepareFile(pathString + fNames.methodsCSV)
    val srgRaw = SrgReader read prepareFile(pathString + fNames.combinedSrg)
    RawDataMerger.merge(srgRaw, fieldsRaw, methodsRaw)
  }
}
