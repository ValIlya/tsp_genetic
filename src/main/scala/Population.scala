package tsp

class Population(popSize: Int, tourManager: TourManager, initialize: Boolean) {
  var tours: IndexedSeq[ATour] = IndexedSeq.empty[ATour]
  var newTour: ATour = _
  if (initialize) {
    for (a <- 1 to popSize) {
      newTour = tourManager.generateTour()
      tours = tours :+ newTour
    }
  } else {
    tours = IndexedSeq.fill[ATour](popSize)(null)
  }

  def setTour(i: Int, tour: ATour) = {
    tours = tours.updated(i, tour)
  }

  def getTour(i: Int) = tours(i)

  def getFittest(): ATour = {
    var fittest: ATour = getTour(0)
    for (i <- 0 until populationSize) {
      fittest = fittest.compareWith(getTour(i))
    }
    return fittest
  }

  def populationSize(): Int = tours.length

  override def toString: String = {
    var s: String = "0 " + getTour(0).toString()
    for(i <- 1 until populationSize()) {
      s += "\n" + i + " " + getTour(i)
    }
    return s
  }

}
