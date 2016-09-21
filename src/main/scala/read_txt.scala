package tsp

class getCitiesFrom {
  def textFile(file: String, sep: String, skiprows: Int = 1): Array[ACity] = {
    var cities = Array.empty[ACity]
    val bufferedSource = io.Source.fromFile(file)
    for (line <- bufferedSource.getLines.drop(skiprows)) {
      val cols = line.split(" ").map(_.trim.toDouble)
      cities = cities :+ new DCity(cols(1), cols(2))

    }
    bufferedSource.close
    println(cities.length+" cities read from file "+file)
    cities
  }

}