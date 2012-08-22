package samples

import org.junit._
import Assert._
import org.apache.commons.math3.random.MersenneTwister
import org.randi3.simulation.{Simulation1, SimulationUtil}

@Test
class AppTest {

    @Test
    def testOK() = {

      val simTrials = SimulationUtil.simulate1(new Simulation1, 10, new MersenneTwister())

      for (simTrial <- simTrials) {
        println("---------------")
        for (arm <- simTrial.treatmentArms) {
          println(arm.name + ": " + arm.subjects.size)
        }
      }

      println("runs: " + simTrials.size)
      assertTrue(true)
    }

//    @Test
//    def testKO() = assertTrue(false)

}


