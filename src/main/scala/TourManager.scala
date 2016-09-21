package tsp

class TourManager(cities: IndexedSeq[ACity]){
  var destinationCities = cities

  def addCity(c: ACity) = {
    destinationCities = destinationCities :+ c
  }

  def getCity(i: Int): ACity = {
    return destinationCities(i)
  }

  def numberOfCities = destinationCities.length

  def generateTour(): ATour = {
    return new DTour(this)
  }


  override def toString: String = {
    var s: String = ""
    for(x <- destinationCities) {
      s += x + " | "
    }
    s += destinationCities(0)
    return s
  }
}
