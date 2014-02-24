package org.randi3.simulation.distributions

import org.apache.commons.math3.random.MersenneTwister
import org.randi3.model.criterion.OrdinalCriterion
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet

trait OrdinalCriterionDistibution extends CriterionDistribution[String] {

  val criterion: OrdinalCriterion

  def sample: String

}

case class OrdinalCriterionEqualDistributed(criterion: OrdinalCriterion, seed: Long) extends OrdinalCriterionDistibution {

  private val random = new MersenneTwister(seed)

  val name = "Equal distributed"

  def sample: String = {
    val values = criterion.values.toList
    values(random.nextInt(values.size))
  }

}

case class OrdinalCriterionFixedRatio(criterion: OrdinalCriterion, seed: Long, ratio: Map[String, Int]) extends OrdinalCriterionDistibution {

  private val random = new MersenneTwister(seed)

  val name = "Fixed ratio"

  private val ratioSum = ratio.values.sum
  private val ratioList = ratio.toList
  
  def sample: String = {
    val randomNumber = random.nextInt(ratioSum)
    var found = false
    var i = 0
    var sum = 0;

    while (!found && i < ratioList.size) {
      sum  = sum + ratioList(i)._2
      if (sum > randomNumber) {
        found = true
      } else {
        i = i + 1
      }
    }
    ratioList(i)._1
  }

}