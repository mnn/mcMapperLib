package tk.monnef.mcmapper

import java.io.File

object McMapper {
  implicit val fNames = DefaultInputFileNames

  private def prepareFile(path: String): File = {
    val f = new File(path)
    if (!f.exists()) throw new McMapperException(s"Cannot find '$path'.")
    if (!f.canRead) throw new McMapperException(s"Cannot read '$path'.")
    f
  }

  def load(path: File)(implicit filesNames: InputFilesNames): MappingDatabase = {
    val fieldsRaw = CSVReader read prepareFile(fNames.fieldsCSV)
    val methodsRaw = CSVReader read prepareFile(fNames.methodsCSV)
    val srgRaw = SrgReader read prepareFile(fNames.combinedSrg)
    RawDataMerger.merge(srgRaw, fieldsRaw, methodsRaw)
  }
}
