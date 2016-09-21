package tsp

import java.util

import scala.util.Random

abstract class ATour {
  var tour: IndexedSeq[ACity]
  val isEmpty = false
  private var fitness: Double = 0
  private var distance: Double = 0

  def getCity(i: Int): ACity = tour(i)

  def setCity(i: Int, city: ACity) = {
    tour = tour.updated(i, city)
  }

  def getDistance(): Double = {
    if (distance == 0) {
      var fromCity: ACity = getCity(0)
      var toCity: ACity = getCity(1)
      var dist_counter: Double = 0
      for (i <- 1 until tourSize()) {
        fromCity = getCity(i-1)
        toCity = getCity(i)
        dist_counter += fromCity.DistanceTo(toCity)
      }
      distance = dist_counter
    }
    return distance
  }

  def getFitness(): Double = {
    if (fitness == 0) fitness = 1/getDistance()
    return fitness
  }

  def compareWith(that: ATour): ATour = if (this.getFitness() > that.getFitness()) this else that

  def tourSize() = tour.length

  def containsCity(city: ACity): Boolean = {
    tour.contains(city)
  }

  def toPairIterable(): Iterable[(Double, Double)] = {
    tour.map(city => (city.getX(), city.getY()))
  }

  override def toString: String = {
    var s: String = ""
    for(x <- tour) {
      s += x + " | "
    }
    s += tour(0) + ", distance: " + getDistance()
    return s
  }

}

class NullTour(numberOfCities: Int) extends ATour {
  var tour = IndexedSeq.fill[ACity](numberOfCities)(new ECity())

}

class DTour(tourManager: TourManager) extends ATour {
  var tour = tourManager.destinationCities(0) +: Random.shuffle(tourManager.destinationCities.drop(1))
}

class EmptyTour() extends ATour {
  override val isEmpty = true
  var tour = IndexedSeq.empty[ACity]
}