package tk.monnef.mcmapper

abstract class InputFilesNames {
  def combinedSrg: String

  def fieldsCSV: String

  def methodsCSV: String
}

object DefaultInputFileNames extends InputFilesNames {
  def combinedSrg: String = "packaged.srg"

  def fieldsCSV: String = "fields.csv"

  def methodsCSV: String = "methods.csv"
}
