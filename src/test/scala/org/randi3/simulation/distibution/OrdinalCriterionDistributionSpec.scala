package org.randi3.simulation.distibution

import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.randi3.model.criterion.OrdinalCriterion
import org.randi3.simulation.distributions.OrdinalCriterionEqualDistributed
import org.randi3.simulation.distributions.OrdinalCriterionFixedRatio

@RunWith(classOf[JUnitRunner])
class OrdinalCriterionDistributionSpec extends FunSpec with MustMatchers {

  describe("The OrdinalCriterionEqualDistribution sample method") {

    it("should be able to create equal distributed characteristics (two values)") {
      val ordinalCriterion = OrdinalCriterion(name = "ordinalCrit", description = "criterion", values = Set("m", "w"), inclusionConstraint = None, strata = List()).toOption.get

      val ordinalCriterionEqualD = new OrdinalCriterionEqualDistributed(ordinalCriterion, System.currentTimeMillis)

      val test = (1 to 10000).map(i => ordinalCriterionEqualD.sample).toList.groupBy(a => a).map(a => (a._1, a._2.size))

      println(test)
    }

    it("should be able to create equal distributed characteristics (three values)") {
      val ordinalCriterion = OrdinalCriterion(name = "ordinalCrit", description = "criterion", values = Set("m", "w", "na"), inclusionConstraint = None, strata = List()).toOption.get

      val ordinalCriterionEqualD = new OrdinalCriterionEqualDistributed(ordinalCriterion, System.currentTimeMillis)

      val test = (1 to 10000).map(i => ordinalCriterionEqualD.sample).toList.groupBy(a => a).map(a => (a._1, a._2.size))

      println(test)
    }
  }
  
  describe("The OrdinalCriterionFixedRatios sample method") {

    it("should be able to create equal distributed characteristics (two values)") {
      val ordinalCriterion = OrdinalCriterion(name = "ordinalCrit", description = "criterion", values = Set("m", "w"), inclusionConstraint = None, strata = List()).toOption.get

      val ordinalCriterionFixedD = new OrdinalCriterionFixedRatio(ordinalCriterion, System.currentTimeMillis, Map("m" -> 2, "w" -> 1))

      val test = (1 to 10000).map(i => ordinalCriterionFixedD.sample).toList.groupBy(a => a).map(a => (a._1, a._2.size))

      println(test)
    }

    it("should be able to create equal distributed characteristics (three values)") {
      val ordinalCriterion = OrdinalCriterion(name = "ordinalCrit", description = "criterion", values = Set("m", "w", "na"), inclusionConstraint = None, strata = List()).toOption.get

      val ordinalCriterionFixedD = new OrdinalCriterionFixedRatio(ordinalCriterion, System.currentTimeMillis, Map("m" -> 2, "w" -> 1, "na" -> 1))

      val test = (1 to 10000).map(i => ordinalCriterionFixedD.sample).toList.groupBy(a => a).map(a => (a._1, a._2.size))

      println(test)
    }
  }
}