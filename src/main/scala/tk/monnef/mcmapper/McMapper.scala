package tk.monnef.mcmapper

import java.io.File

object McMapper {
  implicit val fNames = DefaultInputFileNames

  def load(path: File)(implicit filesNames: InputFilesNames): MappingDatabase = {
    // TODO
    null
  }
}
