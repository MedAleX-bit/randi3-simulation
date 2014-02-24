package org.randi3.simulation.model

import org.randi3.model.Trial
import org.randi3.model.TreatmentArm
import scala.collection.mutable.ListBuffer

case class SimulationResult(runs: Int, scenario: SimulationScenario, results: List[SimulationResultMethod]) {

   val duration = results.map(_.duration).sum
}

case class SimulationResultMethod(method: String, duration: Long, result: List[Trial]) {

  val simResultsArms = {
    result.head.treatmentArms.map(arm => {
      new SimulationResultArm(arm.name, result)
    })
  }

  val marginaBalances = {
    result.map(getMarginalBalace(_))
  }

  private def getMarginalBalace(trial: Trial): Double = {
    var marginalBalance = 0.0
    var numerator = 0.0
    val subjectsPerArms = trial.treatmentArms.map(_.subjects.size)
    val plannedSubjectsPerArm = trial.treatmentArms.map(_.plannedSize)
    for (i <- 0 until subjectsPerArms.size - 1) {
      for (j <- i + 1 until subjectsPerArms.size) {
        marginalBalance += Math
          .abs(((subjectsPerArms(i) * 1.0) / (plannedSubjectsPerArm(i) * 1.0))
            - ((subjectsPerArms(j) * 1.0) / (plannedSubjectsPerArm(j) * 1.0)));
      }
      numerator += ((subjectsPerArms(i) * 1.0) / (plannedSubjectsPerArm(i) * 1.0))
    }
    numerator += subjectsPerArms(subjectsPerArms.size - 1) / (plannedSubjectsPerArm(subjectsPerArms.length - 1) * 1.0)
    numerator = (subjectsPerArms.length - 1.0) * numerator
    marginalBalance = marginalBalance / numerator
    marginalBalance
  }

  val marginalBalanceMin = marginaBalances.min
  val marginalBalanceMax = marginaBalances.max
  val marginalBalanceMean = marginaBalances.sum / marginaBalances.size

}

case class SimulationResultArm(armName: String, result: List[Trial]) {

  private val armSubjects = result.flatMap(trial => trial.treatmentArms.filter(_.name == armName).map(_.subjects.size))

  val min = armSubjects.min
  val max = armSubjects.max
  val mean = armSubjects.sum / armSubjects.size
  val median = {
    val (lower, upper) = armSubjects.sorted.splitAt(armSubjects.size / 2)
    if (armSubjects.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
  }

  //	Map<String,String> strataIdsNames;
  //	Map<String, Integer> strataCountsPerArmMin;
  //	Map<String, Integer> strataCountsPerArmMax;
  //	Map<String, Double> strataCountsPerArmMean;
}