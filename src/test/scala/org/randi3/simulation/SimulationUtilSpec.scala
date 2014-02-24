package org.randi3.simulation
import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.randi3.model.criterion.OrdinalCriterion
import org.randi3.simulation.distributions.OrdinalCriterionEqualDistributed
import org.randi3.simulation.distributions.OrdinalCriterionFixedRatio
import org.randi3.simulation.utility.TestingEnvironmentSimulation._
import org.randi3.simulation.service.SimulationUtil._
import org.randi3.randomization.CompleteRandomization
import org.apache.commons.math3.random.MersenneTwister
import org.randi3.simulation.distributions.CriterionDistribution
import org.apache.commons.math3.distribution.IntegerDistribution
import org.randi3.simulation.distributions.TrialSiteDistribution
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.randi3.simulation.distributions.TrialSiteDistribution
import org.apache.commons.math3.distribution.PascalDistribution
import org.randi3.simulation.model._

@RunWith(classOf[JUnitRunner])
class SimulationUtilSpec extends FunSpec with MustMatchers {
 
  
  describe("The SimulationUtilSpec simulate method") {

    it("should be able to simulate a trial without stages") {
      val ordinalCriterion = OrdinalCriterion(id=1, name = "ordinalCrit", description = "criterion", values = Set("m", "w"), inclusionConstraint = None, strata = List()).toOption.get
      val ordinalCriterionEqualD = new OrdinalCriterionEqualDistributed(ordinalCriterion, System.currentTimeMillis)
      val method = randomMethod.toOption.get 
      val plainTrial = createTrial.copy(criterions = List(ordinalCriterion), randomizationMethod = Some(method))
      val trialSiteDistribution = new TrialSiteDistribution(System.currentTimeMillis(), Map(plainTrial.participatingSites.head -> 1))
      val simulationScenario = new SimulationScenario {
        val trial = plainTrial
        val criterionProbability = List(ordinalCriterionEqualD).asInstanceOf[List[CriterionDistribution[Any]]]
        def stageProbabilities: List[(String, List[CriterionDistribution[Any]], PascalDistribution)] = Nil
        def siteRatio: TrialSiteDistribution = trialSiteDistribution
        def randomizationMethods = List(("complete", method))
      }
      val res = simulate(simulationScenario, 100, System.currentTimeMillis())
//      println(res.duration / 1000)
//      println(res.results.head.marginaBalances)
//      val aus = res.map(trial => (trial.treatmentArms.head.name + "-" + trial.treatmentArms.head.subjects.size, trial.treatmentArms.last.name + "-" + trial.treatmentArms.last.subjects.size))
//       aus.foreach(println(_))
    }
  }
}