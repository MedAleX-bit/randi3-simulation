package org.randi3.simulation.distributions

import org.apache.commons.math3.random.MersenneTwister
import org.randi3.model.TrialSite

case class TrialSiteDistribution(seed: Long, ratio: Map[TrialSite, Int]) {

  private val random = new MersenneTwister(seed)

  val name = "Fixed ratio"

  private val ratioSum = ratio.values.sum
  private val ratioList = ratio.toList

  def sample: TrialSite = {
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