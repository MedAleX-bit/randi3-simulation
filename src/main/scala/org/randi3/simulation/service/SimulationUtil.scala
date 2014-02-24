package org.randi3.simulation.service

import org.randi3.model._
import org.randi3.model.criterion._
import constraint.Constraint
import org.randi3.randomization.RandomizationMethod
import org.randi3.simulation.distributions._
import org.apache.commons.math3.random._
import scala.collection.mutable.{HashMap, ListBuffer}
import org.joda.time.{DateTime, Days}
import scala._
import collection.mutable
import org.apache.commons.math3.distribution.{ExponentialDistribution, IntegerDistribution, RealDistribution}
import org.apache.commons.math3.distribution.PascalDistribution
import org.randi3.model.criterion.constraint.IntegerConstraint
import org.randi3.simulation.model._

class Simulation {


}

case object SimulationUtil {

  private val randomDayCriterion = {
    IntegerCriterion(id = Int.MaxValue, name = "randomDay", description ="Day of randomization", inclusionConstraint = None, strata = Nil).toOption.get
  }
  
  private def generateRandomizationsPerDay(maxDays: Int, subjectCount: Int, seed: Long): Map[Int, Int] = {
    val result = HashMap[Int, Int]()
    val random = new MersenneTwister(seed)
    for (i <- 1.to(subjectCount)) {
      val randomVar = random.nextInt(maxDays) + 1
      result.get(randomVar) match {
        case None => result.put(randomVar, 1)
        case Some(x) => result.put(randomVar, x + 1)
      }
    }
    result.toMap
  }

  
  private def generateSubject(trial: Trial, subjectId: String, randomizationDay: Int, criterionProbability: List[CriterionDistribution[Any]], site: TrialSiteDistribution): TrialSubject = {

    val properties = ListBuffer[SubjectProperty[Any]]()  

    criterionProbability.foreach(criterionDistribution => {
      val value = criterionDistribution.sample
      properties.append(SubjectProperty(criterion = criterionDistribution.criterion, value = value).toOption.get)
    })
     val randomDay = SubjectProperty(criterion = randomDayCriterion, value = randomizationDay).getOrElse(throw new Exception("Not possible to add randomization day"))
     properties.append(randomDay.asInstanceOf[SubjectProperty[Any]])
     TrialSubject(identifier = subjectId, investigatorUserName = "simulationUser", trialSite = site.sample, properties = properties.toList, stages = Map()).toOption.get
  }

  private def generateStagesTMP(trial: Trial, arm: TreatmentArm, subject: TrialSubject, stageProbabilities: List[(String, List[CriterionDistribution[Any]], PascalDistribution)]): TrialSubject = {

    val stageProperties = ListBuffer[SubjectProperty[Any]]()

    if(!stageProbabilities.isEmpty){
    val randomMap = stageProbabilities.head._2

    for (criterion <- trial.stages.head._2) {
      if (criterion.name == "PFS") {
     //   stageProperties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = randomMap.get(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]).get.sample()).toOption.get)
      } else if (criterion.name == "PPT") {
      //  stageProperties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = randomMap.get(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]).get.sample()).toOption.get)
      }
    }

    val stages =stageProperties.toList

    subject.copy(stages = Map("TMPResponse" -> stages))
    } else
    subject
  }

 private def generateStages(trial: Trial, arm: TreatmentArm, subject: TrialSubject): TrialSubject = {

    val stageTMP = subject.stages.get("TMPResponse").get

    val stageProperties = ListBuffer[SubjectProperty[Any]]()

    for (criterion <- stageTMP.map(prop => prop.criterion)) {

      val value = stageTMP.find(prop => prop.criterion.name == criterion.name).get.value.asInstanceOf[Int]

      if (criterion.name == "PFS") {
        stageProperties.append(SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = value).toOption.get)
      }    
    }

    val stages = stageProperties.toList

    subject.copy(stages =  Map("TMPResponse" ->  stageTMP,"Response" -> stages))
  }


  def simulateOneTrial(scMethod: RandomizationMethod, scTrial: Trial, stageProbabilities: (List[(String, List[CriterionDistribution[Any]], PascalDistribution)]), criterionProbability: List[CriterionDistribution[Any]], siteRatio: TrialSiteDistribution, random: RandomGenerator): Trial = {
    val arms = scTrial.treatmentArms.map(arm => arm.copy(subjects = new ListBuffer[TrialSubject]()))
    val trial = scTrial.copy(criterions = randomDayCriterion :: scTrial.criterions, treatmentArms = arms, randomizationMethod = Some(scMethod))
    
    val daysOfRandomization = Days.daysBetween(trial.startDate, trial.endDate).getDays
    val randomizationsPerDay = generateRandomizationsPerDay(daysOfRandomization, trial.plannedSubjects, random.nextLong())
    for (day <- 1.to(daysOfRandomization)) {
      val changedSubjects = ListBuffer[TrialSubject]()
      val dropedSubjects = ListBuffer[String]()
      
      randomizationsPerDay.get(day) match {
        case None =>
        case Some(randomizations) => {
          for (i <- 1.to(randomizations)) {

            val subject = generateSubject(trial, day + "_" + i, day, criterionProbability, siteRatio)
            val randArm = trial.randomize(subject)
            if (randArm.isSuccess && randArm.toOption.get != null){
            val subject2 = generateStagesTMP(trial, randArm.toOption.get, subject, stageProbabilities)
            
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

  def simulate(scenario: SimulationScenario, runs: Int, seed: Long): SimulationResult = {
    val random: RandomGenerator = new MersenneTwister(seed)
    val res = scenario.randomizationMethods.map(method => {
      val before = System.currentTimeMillis()
      val simRes = simResults(method._2, scenario: SimulationScenario, runs, random)
       val after = System.currentTimeMillis()
      new SimulationResultMethod(method._1, after- before,simRes)
    })   
    new SimulationResult(runs, scenario, res)
  }

 private def simResults(method: RandomizationMethod,scenario: SimulationScenario, runs: Int, random: RandomGenerator): List[Trial] = {
    if (runs == 0) return List()
    List(simulateOneTrial(method, scenario.trial, scenario.stageProbabilities, scenario.criterionProbability, scenario.siteRatio, random)) ::: simResults(method, scenario, runs - 1, random)
 }

//  def simulate1(scenario: SimulationScenario, runs: Int, random: RandomGenerator): List[Trial] = {
//    simResults(scenario: SimulationScenario, runs, random)
//  }

 
//  def simulateMulti(threadCount: Int, scenario: SimulationScenario, runs: Int, seed: Int): List[Trial] = {
//    waitForThreadsAndGetResults(createThreads(threadCount, scenario, runs, seed))
//  }
//
//  def simulateMultiMore(threadCount: Int, scenarios: List[SimulationScenario], runs: Int, seed: Int): List[(SimulationScenario, List[Trial])] = {
//    if (scenarios.isEmpty) return List()
//
//    List((scenarios.head, simulateMulti(threadCount, scenarios.head, runs, seed))) ::: simulateMultiMore(threadCount, scenarios.tail, runs, seed)
//  }
//
//
//  private def createThreads(threadCount: Int, scenario: SimulationScenario, runs: Int, seed: Int): List[(Thread, SimulationRuns)] = {
//    if (threadCount == 0) return List()
//    val random: RandomGenerator =
//      if (seed == Int.MinValue)
//        new MersenneTwister()
//      else
//        new MersenneTwister(seed)
//    val actRuns = runs / threadCount
//    val simRun = new SimulationRuns(scenario, actRuns, random)
//    val thread = new Thread(simRun)
//    thread.start()
//    List((thread, simRun)) ::: createThreads(threadCount - 1, scenario, runs - actRuns, random.nextInt)
//  }
//
//  private def waitForThreadsAndGetResults(simThreads: List[(Thread, SimulationRuns)]): List[Trial] = {
//    if (simThreads.isEmpty) return List()
//    simThreads.head._1.join()
//    simThreads.head._2.result ::: waitForThreadsAndGetResults(simThreads.tail)
//  }
}

case class SimulationRuns(scenario: SimulationScenario, runs: Int, random: RandomGenerator) extends Runnable {
  var result: List[Trial] = null

  override def run() = {
  //  result = SimulationUtil.simulate1(scenario, runs, random)
  }
}
