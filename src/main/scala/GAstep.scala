package tsp

import scala.util.Random

class GAstep(mutationRate: Double,
             populationSize: Int,
             tournamentSize: Int = 10,
             tourManager: TourManager,
             elitism: Boolean) {

  def evolvePopulation(population: Population): Population = {
    val newPopulation = new Population(populationSize, tourManager, initialize = false)

    var elitismOffset = 0
    if (elitism) {
      elitismOffset = 1
      newPopulation.setTour(0, population.getFittest())
    }

    for (i <- elitismOffset until newPopulation.populationSize()) {
      val parent1 = tournamentSelection(population)
      val parent2 = tournamentSelection(population)

      val child = crossover(parent1, parent2)
      newPopulation.setTour(i, child)
    }

    var mutated: ATour = null
    for (i <- 0 until newPopulation.populationSize()) {
      mutated = mutate(newPopulation.getTour(i))
      newPopulation.setTour(i, mutated)
    }

    newPopulation
  }



  def tournamentSelection(population: Population): ATour = {
    val tournament = new Population(tournamentSize, tourManager, initialize = false)
    var randomId: Int = 0
    for (i <- 1 to tournamentSize) {
      randomId = Random.nextInt(population.populationSize())
      tournament.setTour(i-1, population.getTour(randomId))
    }
    tournament.getFittest()
  }

  def crossover(a: ATour, b: ATour): ATour = {
    val child = new NullTour(tourManager.numberOfCities)

    val tmpList = List.fill[Int](2)(Random.nextInt(tourManager.numberOfCities))
    val startPos = tmpList.min
    val endPos = tmpList.max
    
    // println("from " + startPos + " to " + endPos)

    for (i <- startPos to endPos) {
      child.setCity(i, a.getCity(i))
    }
    var i = 0
    var x = b.getCity(i)
    var j = 0
    var y = child.getCity(j)

    while ((i < b.tourSize()) && (j < child.tourSize())) {
      if ((!child.containsCity(x)) && (y.isEmpty)){
        child.setCity(j, x)
        // println("set "+ i+ " at "+ j+ " position")
        j+=1; if (j<child.tourSize()) y = child.getCity(j)
        i+=1; if (i<b.tourSize()) x = b.getCity(i)
      } else if (child.containsCity(x)){
        // println("already have " + i + " position: " + x)
        i+=1
        if (i<b.tourSize()) x = b.getCity(i)
      } else {
        // println("not empty " + j + " position: " + y)
        j+=1
        if (j<child.tourSize()) y = child.getCity(j)
      }
    }
    
    assert(child.tour.forall(!_.isEmpty))
    child
  }


  def mutate(tour: ATour): ATour = {
    var j: Int = 0
    var tmp: ACity = new ECity()
    for(i <- 0 until tour.tourSize()){
      if (Random.nextDouble() < mutationRate) {
        j = Random.nextInt(tour.tourSize())
        assert((j >= 0) && (j < tour.tourSize()))
        tmp = tour.getCity(i)
        tour.setCity(i, tour.getCity(j))
        tour.setCity(j, tmp)
      }
    }
    assert(!tour.tour.forall(_.isEmpty))
    tour
  }



}
