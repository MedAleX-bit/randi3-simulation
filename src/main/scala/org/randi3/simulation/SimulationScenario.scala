package org.randi3.simulation

import org.randi3.model._
import org.randi3.model.criterion._
import constraint.Constraint
import java.rmi.server.ExportException
import org.apache.commons.math3.distribution.{ExponentialDistribution, AbstractRealDistribution, IntegerDistribution, PascalDistribution}

trait SimulationScenario {

  def trial: Trial

  //only for continuous criterions
  def criterionProbability: Map[Criterion[Any, Constraint[Any]], IntegerDistribution]

  //StageName
  def stageProbabilities(trial: Trial): Map[String, List[(String, Map[Criterion[Any, Constraint[Any]], PascalDistribution], Double)]]

}