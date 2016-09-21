import tsp._
import com.quantifind.charts.Highcharts._
import com.quantifind.charts.highcharts.Highchart


object Sample {
  def main(args: Array[String]): Unit = {
//    println(System.getProperty("user.dir"))
    val cities = new getCitiesFrom().textFile("src/main/resources/data29.txt", " ")

    val tourManager = new TourManager(cities.toIndexedSeq)

    var pop = new Population(50, tourManager, true)
    println("Initial distance: " + pop.getFittest().getDistance())
    val GA = new GAstep(mutationRate=0.15,
                        populationSize=50,
                        tournamentSize=5,
                        tourManager=tourManager,
                        elitism=true)

    for (i <- 1 to 1000) {
      pop = GA.evolvePopulation(pop)
    }

    println("Final distance: " + pop.getFittest().getDistance())
    val points = pop.getFittest().toPairIterable()
    var prev_pair = points.head
    for (pair <- points.tail) {
      line(Iterable(prev_pair, pair))
      hold
      prev_pair = pair
    }

    line(Iterable(points.head, prev_pair))
    hold
    scatter(points)

  }
}
