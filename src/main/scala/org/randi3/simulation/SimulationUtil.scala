package org.randi3.simulation

import org.randi3.model._
import org.randi3.model.criterion._
import constraint.Constraint
import org.apache.commons.math3.random._
import scala.actors.Actor
import scala.collection.mutable.{HashMap, ListBuffer}
import org.joda.time.Days
import scala._
import org.apache.commons.math3.distribution.{ExponentialDistribution, IntegerDistribution}
import org.apache.commons.math3.distribution.PascalDistribution

class Simulation {


}

case object SimulationUtil {

  private def generateRandomizationsPerDay(maxDays: Int, subjectCount: Int): Map[Int, Int] = {
    val result = HashMap[Int, Int]()
    val random = new MersenneTwister()
    for (i <- 1.to(subjectCount)) {
      val randomVar = random.nextInt(maxDays) + 1
      result.get(randomVar) match {
        case None => result.put(randomVar, 1)
        case Some(x) => result.put(randomVar, x + 1)
      }
    }
    result.toMap
  }

  private def generateSubject(trial: Trial, subjectId: String, randomizationDay: Int, stageProbabilities: Map[String, List[(String, Map[Criterion[Any, Constraint[Any]], PascalDistribution], Double)]]): TrialSubject = {

    val properties = ListBuffer[SubjectProperty[Any]]()

    for (criterion <- trial.criterions) {
      if (criterion.name == "QoL") {
        properties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = 1).toOption.get)
      } else if (criterion.name == "TT") {
        properties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = 1).toOption.get)
      } else if (criterion.name == "randomDay") {
        properties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = randomizationDay).toOption.get)
      }
    }



    TrialSubject(identifier = subjectId, investigatorUserName = "user", trialSite = trial.participatingSites.head, properties = properties.toList, stages = Nil).toOption.get

  }

  def generateStagesTMP(trial: Trial, arm: TreatmentArm, subject: TrialSubject, stageProbabilities: Map[String, List[(String, Map[Criterion[Any, Constraint[Any]], PascalDistribution], Double)]]): TrialSubject = {

    val stageProperties = ListBuffer[SubjectProperty[Any]]()


    val randomMap = stageProbabilities.get(arm.name).get.head._2

    for (criterion <- trial.stages.head._2) {
      if (criterion.name == "PFS") {
        stageProperties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = randomMap.get(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]).get.sample()).toOption.get)
      } else if (criterion.name == "PPT") {
        stageProperties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = randomMap.get(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]).get.sample()).toOption.get)
      }
    }

    val stages = List[List[SubjectProperty[Any]]](stageProperties.toList)

    subject.copy(stages = stages)
  }

  def generateStages(trial: Trial, arm: TreatmentArm, subject: TrialSubject, stageProbabilities: Map[String, List[(String, Map[Criterion[Any, Constraint[Any]], PascalDistribution], Double)]], withPPT: Boolean): TrialSubject = {

    val stageTMP = subject.stages(0)
    val stageProperties = ListBuffer[SubjectProperty[Any]]()

    for (criterion <- stageTMP.map(prop => prop.criterion)) {
      val value = stageTMP.find(prop => prop.criterion.name == criterion.name).get.value.asInstanceOf[Int]
      if (criterion.name == "PFS") {
        stageProperties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = value).toOption.get)
      } else if (criterion.name == "PPT" && withPPT) {
        stageProperties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = value).toOption.get)
      }
    }

    val stages = List[List[SubjectProperty[Any]]](stageTMP.asInstanceOf[List[SubjectProperty[Any]]], stageProperties.toList)

    subject.copy(stages = stages)

  }


  def simulateOneTrial(trial: Trial, stageProbabilities: Map[String, List[(String, Map[Criterion[Any, Constraint[Any]], PascalDistribution], Double)]], criterionProbability: Map[Criterion[Any, Constraint[Any]], IntegerDistribution], random: RandomGenerator): Trial = {
    val daysOfRandomization = Days.daysBetween(trial.startDate, trial.endDate).getDays
    val randomizationsPerDay = generateRandomizationsPerDay(daysOfRandomization, trial.plannedSubjects)

    for (day <- 1.to(daysOfRandomization)) {

      val changedSubjects = ListBuffer[TrialSubject]()
      val dropedSubjects = ListBuffer[String]()

      for (arm <- trial.treatmentArms) {
        changedSubjects.clear()
        dropedSubjects.clear()

        for (i <- arm.subjects.indices) {
          val subject = arm.subjects(i)
          if (!subject.stages.isEmpty) {
            val randomDay = subject.properties.find(prop => prop.criterion.name == "randomDay").get.value.asInstanceOf[Int]
            val pfsDay = subject.stages.head.find(prop => prop.criterion.name == "PFS").get.value.asInstanceOf[Int]
            val pptDay = subject.stages.head.find(prop => prop.criterion.name == "PPT").get.value.asInstanceOf[Int]


            if ((randomDay + pfsDay) <= day || (randomDay + 90) <= day) {

              val newSubject = generateStages(trial, arm, subject, stageProbabilities, if ((randomDay + pfsDay + pptDay) <= day || (randomDay + pfsDay + 120) <= day) true else false)

              dropedSubjects.append(subject.identifier)
              changedSubjects.append(newSubject)

            }
          }
        }

        var newSubjects = arm.subjects.toList



        dropedSubjects.foreach(identifier => {
          newSubjects = newSubjects.filterNot((sub => sub.identifier == identifier))

        })
        arm.subjects.clear()

        newSubjects.foreach(sub => arm.subjects.append(sub))

        changedSubjects.foreach(sub => arm.subjects.append(sub))

      }

      randomizationsPerDay.get(day) match {
        case None =>
        case Some(randomizations) => {
          for (i <- 1.to(randomizations)) {

            val subject = generateSubject(trial, day + "_" + i, day, stageProbabilities)

            val randArm = trial.randomize(subject).toOption.get

            if (randArm != null){
            val subject2 = generateStagesTMP(trial, randArm, subject, stageProbabilities)

            for (arm <- trial.treatmentArms) {
              if (arm.subjects.find(sub => sub.identifier == subject.identifier).isDefined) {
                val oldSubjects = arm.subjects.toList.filterNot(sub => sub.identifier == subject.identifier)
                val newSubjects = subject2 :: oldSubjects
                arm.subjects.clear()
                newSubjects.foreach(sub => arm.subjects.append(sub))
              }
            }
            }


          }
        }
      }
    }

    trial
  }

  def simulate(scenario: SimulationScenario, runs: Int, seed: Int): List[Trial] = {
    val random: RandomGenerator =
      if (seed == Int.MinValue)
        new MersenneTwister()
      else
        new MersenneTwister(seed)
    simResults(scenario: SimulationScenario, runs, random)
  }

  def simulate1(scenario: SimulationScenario, runs: Int, random: RandomGenerator): List[Trial] = {
    simResults(scenario: SimulationScenario, runs, random)
  }

  def simResults(scenario: SimulationScenario, runs: Int, random: RandomGenerator): List[Trial] = {
    if (runs == 0) return List()
    val trial = scenario.trial
    List(simulateOneTrial(trial, scenario.stageProbabilities(trial), scenario.criterionProbability, random)) ::: simResults(scenario, runs - 1, random)
  }

  def simulateMulti(threadCount: Int, scenario: SimulationScenario, runs: Int, seed: Int): List[Trial] = {

    waitForThreadsAndGetResults(createThreads(threadCount, scenario, runs, seed))
  }

  def simulateMultiMore(threadCount: Int, scenarios: List[SimulationScenario], runs: Int, seed: Int): List[(SimulationScenario, List[Trial])] = {
    if (scenarios.isEmpty) return List()

    List((scenarios.head, simulateMulti(threadCount, scenarios.head, runs, seed))) ::: simulateMultiMore(threadCount, scenarios.tail, runs, seed)
  }


  def createThreads(threadCount: Int, scenario: SimulationScenario, runs: Int, seed: Int): List[(Thread, SimulationRuns)] = {
    if (threadCount == 0) return List()
    val random: RandomGenerator =
      if (seed == Int.MinValue)
        new MersenneTwister()
      else
        new MersenneTwister(seed)
    val actRuns = runs / threadCount
    val simRun = new SimulationRuns(scenario, actRuns, random)
    val thread = new Thread(simRun)
    thread.start()
    List((thread, simRun)) ::: createThreads(threadCount - 1, scenario, runs - actRuns, random.nextInt)
  }

  def waitForThreadsAndGetResults(simThreads: List[(Thread, SimulationRuns)]): List[Trial] = {
    if (simThreads.isEmpty) return List()
    simThreads.head._1.join()
    simThreads.head._2.result ::: waitForThreadsAndGetResults(simThreads.tail)
  }
}

case class SimulationRuns(scenario: SimulationScenario, runs: Int, random: RandomGenerator) extends Runnable {
  var result: List[Trial] = null

  override def run() = {
    result = SimulationUtil.simulate1(scenario, runs, random)
  }
}
