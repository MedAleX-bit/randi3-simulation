package org.randi3.simulation.model

import org.randi3.model._
import org.randi3.model.criterion._
import org.randi3.randomization.RandomizationMethod
import constraint.Constraint
import org.randi3.simulation.distributions.CriterionDistribution
import org.apache.commons.math3.distribution.IntegerDistribution
import org.randi3.simulation.distributions.TrialSiteDistribution
import org.apache.commons.math3.distribution.PascalDistribution

trait SimulationScenario {

  def trial: Trial

  //only for continuous criterions
  def criterionProbability: List[CriterionDistribution[Any]]

  //StageName
  def stageProbabilities: (List[(String, List[CriterionDistribution[Any]], PascalDistribution)])
  
  def siteRatio: TrialSiteDistribution
  
  def randomizationMethods: List[(String, RandomizationMethod)]
  
}