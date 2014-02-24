package org.randi3.simulation.utility

import org.randi3.schema.{DatabaseSchema, LiquibaseUtil}
import org.randi3.utility.TestingEnvironment
import org.randi3.model.criterion.constraint.{Constraint, DoubleConstraint}
import org.randi3.model.criterion.{Criterion, IntegerCriterion, DoubleCriterion, OrdinalCriterion}
import scala.Some

object TestingEnvironmentSimulation extends TestingEnvironment {

  def criterion1(name: String): DoubleCriterion = DoubleCriterion(name = name, description = "descritpion", inclusionConstraint = None , strata = Nil).toOption.get
  def criterion2(name: String): IntegerCriterion = IntegerCriterion(name = name, description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get
  
}