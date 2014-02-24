package org.randi3.simulation.distributions

import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint

trait CriterionDistribution[T] {

   def name: String
   val seed: Long
   val criterion: Criterion[T, Constraint[T]] 
   def sample: T
  
}