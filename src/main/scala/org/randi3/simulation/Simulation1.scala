package org.randi3.simulation

import scala.collection.mutable.ListBuffer
import org.joda.time.LocalDate
import org.randi3.model._
import criterion.constraint.Constraint
import criterion.{IntegerCriterion, OrdinalCriterion, Criterion}
import org.apache.commons.math3.random.MersenneTwister
import scala.collection.immutable.HashMap
import scala._
import org.apache.commons.math3.distribution.{IntegerDistribution, RealDistribution, ExponentialDistribution, UniformIntegerDistribution, PascalDistribution}
import org.randi3.randomization.{AdaptiveBayesianRandomization, Outcome}

class Simulation1 extends SimulationScenario {

  def arm1 = TreatmentArm(Int.MinValue, 0, "treatment", "vaildDescription", new ListBuffer[TrialSubject], 177).toOption.get

  def arm2 = TreatmentArm(Int.MinValue, 0, "control", "vaildDescription", new ListBuffer[TrialSubject], 177).toOption.get

  def treatmentArms: List[TreatmentArm] = List(arm1, arm2)


  val site = TrialSite(name = "name", country = "country", street = "street", postCode = "postcode", city = "city", password = "abcdefg").toOption.get

  val randomizationDay: Criterion[Any, Constraint[Any]] = {
    IntegerCriterion(name = "randomDay", description = "day of randomisation", inclusionConstraint = None, strata = Nil).toOption.get.asInstanceOf[Criterion[Any, Constraint[Any]]]
  }


  val pfsDay: Criterion[Any, Constraint[Any]] = {

    IntegerCriterion(name = "PFS", description = "day of randomisation", inclusionConstraint = None, strata = Nil).toOption.get.asInstanceOf[Criterion[Any, Constraint[Any]]]

  }


  val osDay: Criterion[Any, Constraint[Any]] = {

    IntegerCriterion(name = "PPT", description = "day of randomisation", inclusionConstraint = None, strata = Nil).toOption.get.asInstanceOf[Criterion[Any, Constraint[Any]]]

  }

  val qol: Criterion[Any, Constraint[Any]] = {

    IntegerCriterion(name = "QoL", description = "Quality of life", inclusionConstraint = None, strata = Nil).toOption.get.asInstanceOf[Criterion[Any, Constraint[Any]]]

  }

  val tt: Criterion[Any, Constraint[Any]] = {

    IntegerCriterion(name = "TT", description = "treatmetnToxisitrialsubty", inclusionConstraint = None, strata = Nil).toOption.get.asInstanceOf[Criterion[Any, Constraint[Any]]]

  }

  val criterions: List[Criterion[Any, Constraint[Any]]] = {
    List(randomizationDay, qol, tt)
  }

  def trial: Trial = {
    val treatments = treatmentArms

    val my = Map(
      Outcome.R -> 1.0,
      Outcome.F -> 1.0)

    val alpha = Map(
      Outcome.R -> 1.0,
      Outcome.F -> 1.0)

    // gamma_1_y = 0.0
    val gamma = Map(
      1 -> Map(
        Outcome.R -> 0.0,
        Outcome.F -> 0.0))

    val beta = Map(
      qol.asInstanceOf[IntegerCriterion] -> Map(
        Outcome.R -> 1.0,
        Outcome.F -> 1.0),
      tt.asInstanceOf[IntegerCriterion] -> Map(
        Outcome.R -> 1.0,
        Outcome.F -> 1.0)
    )

    val tau = Map(
      qol.asInstanceOf[IntegerCriterion] -> Map(
        Outcome.R -> 1.0,
        Outcome.F -> 1.0),
      tt.asInstanceOf[IntegerCriterion] -> Map(
        Outcome.R -> 1.0,
        Outcome.F -> 1.0)
    )



    val treatmentValues: Map[TreatmentArm, Double] = HashMap((treatmentArms.head, -1.0), (treatmentArms.tail.head, 1.0))

    val algorithm = new AdaptiveBayesianRandomization(0, 0, random = new MersenneTwister(), pfsLimit = 90, pptLimit = 120)

    val stages = Map("Response" -> List(pfsDay, osDay), "tmp" -> List(pfsDay, osDay))

    val actTrial = Trial(Int.MinValue, 0, "trial", "trial", "Description", new LocalDate(2012, 6, 1), new LocalDate(2014, 6, 1), StratifiedTrialSite.NO, TrialStatus.ACTIVE, treatments, criterions, List(site), Some(algorithm), stages).toOption.get

    actTrial
  }


  def stageProbabilities(trial: Trial): Map[String, List[(String, Map[Criterion[Any, Constraint[Any]], PascalDistribution], Double)]] = {
    Map("control" -> List(("Response",
      Map(pfsDay.asInstanceOf[Criterion[Any, Constraint[Any]]] -> new PascalDistribution(1, 1.0 / 120),
        osDay.asInstanceOf[Criterion[Any, Constraint[Any]]] -> new PascalDistribution(1, 1.0 / 120)
      ),
      1
      )),
      "treatment" -> List(("Response",
        Map(pfsDay.asInstanceOf[Criterion[Any, Constraint[Any]]] -> new PascalDistribution(1, 1.0 / 180),
          osDay.asInstanceOf[Criterion[Any, Constraint[Any]]] -> new PascalDistribution(1, 1.0 / 180)
        ),
        1
        ))
    )
  }
//
//      def stageProbabilities(trial: Trial): Map[String, List[(String, Map[Criterion[Any, Constraint[Any]], PascalDistribution], Double)]] = {
//        Map("control" -> List(("Response",
//          Map(pfsDay.asInstanceOf[Criterion[Any, Constraint[Any]]] -> new PascalDistribution(1, 1.0/180),
//            osDay.asInstanceOf[Criterion[Any, Constraint[Any]]] -> new PascalDistribution(1, 01.0/180)
//          ),
//          1
//          )),
//          "treatment" -> List(("Response",
//            Map(pfsDay.asInstanceOf[Criterion[Any, Constraint[Any]]] -> new PascalDistribution(1, 1.0/180),
//              osDay.asInstanceOf[Criterion[Any, Constraint[Any]]] -> new PascalDistribution(1, 1.0/180)
//            ),
//            1
//            ))
//        )
//      }

  def criterionProbability: Map[Criterion[Any, Constraint[Any]], IntegerDistribution] = {
    Map(
      qol.asInstanceOf[Criterion[Any, Constraint[Any]]] -> new UniformIntegerDistribution(0, 200)
    )
  }
}

object Simulation1 {
  def main(args: Array[String]): Unit = {

    //    val simTrials = simulateMulti(2, new Simulation1, 10, Int.MinValue)
    val simTrials = SimulationUtil.simulate1(new Simulation1, 1000, new MersenneTwister())

    for (simTrial <- simTrials) {
      println("---------------")
      for (arm <- simTrial.treatmentArms) {
        println(arm.name + ": " + arm.subjects.size)
      }
    }

    println("runs: " + simTrials.size)

    //    val value1: Array[Double] = new Array(100)
    //    val value2: Array[Double] = new Array(100)
    //    val generator = new MersenneTwister()
    //
    //    for (i <- 1 to 100) {
    //    	value1(i-1) = generator.nextDouble()
    //    	    	value2(i-1) = generator.nextDouble()
    //    }
    //      val number = 10
    //      val dataset: HistogramDataset = new HistogramDataset()
    //      dataset.setType(HistogramType.RELATIVE_FREQUENCY)
    //      dataset.addSeries("Histogram1", value1, number)
    //       dataset.addSeries("Histogram2", value2, number)
    //      val plotTitle = "Histogram";
    //      val xaxis = "number";
    //      val yaxis = "value";
    //      val orientation: PlotOrientation = PlotOrientation.VERTICAL;
    //      val show = true;
    //      val toolTips = false;
    //      val urls = false;
    //      val chart: JFreeChart = ChartFactory.createHistogram(plotTitle, xaxis, yaxis,
    //        dataset, orientation, show, toolTips, urls);
    //      val width = 500;
    //      val height = 300;
    //      try {
    //        ChartUtilities.saveChartAsPNG(new File("/home/schrimpf/histogram.PNG"), chart, width, height);
    //      } catch {
    //        case ioe: IOException => println(ioe.getMessage)
    //      }
    //
  }
}