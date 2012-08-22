/*
 * Copyright 2001-2009 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package samples

/*
ScalaTest facilitates different styles of testing by providing traits you can mix
together to get the behavior and syntax you prefer.  A few examples are
included here.  For more information, visit:

http://www.scalatest.org/

One way to use ScalaTest is to help make JUnit or TestNG tests more
clear and concise. Here's an example:
*/

import scala.collection.mutable.Stack
import org.scalatest.Assertions
import org.junit.Test
import org.randi3.simulation.{Simulation1, SimulationUtil}
import org.apache.commons.math3.random.MersenneTwister
import io.Codec
import org.specs.io.Output
import java.io.{PrintWriter, File}

class StackSuite extends Assertions {

  @Test def stackShouldPopValuesIinLastInFirstOutOrder() {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    assert(stack.pop() === 2)
    assert(stack.pop() === 1)
  }

  @Test def stackShouldThrowNoSuchElementExceptionIfAnEmptyStackIsPopped() {
    val emptyStack = new Stack[String]
    intercept[NoSuchElementException] {
      emptyStack.pop()
    }
  }
}


/*
ScalaTest also supports the behavior-driven development style, in which you
combine tests with text that specifies the behavior being tested. Here's
an example whose text output when run looks like:

A Map
- should only contain keys and values that were added to it
- should report its size as the number of key/value pairs it contains
*/

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers

class MapSpec extends Spec with MustMatchers {


  describe("test") {
    it("test") {
      val simTrials = SimulationUtil.simulate1(new Simulation1, 1000, new MersenneTwister())


      val writer = new PrintWriter(new File("/home/schrimpf/test.txt"))




      for (simTrial <- simTrials) {

//              for (subject <- simTrial.getSubjects) {
//                val randomDay = subject.properties.find(prop => prop.criterion.name == "randomDay").get.value.asInstanceOf[Int]
//                val pfs = subject.stages.last.find(prop => prop.criterion.name == "PFS")
//                val pfsDay = if(pfs.isDefined) pfs.get.value.asInstanceOf[Int] else -1
//                val ppt = subject.stages.last.find(prop => prop.criterion.name == "PPT")
//                val pptDay =if(ppt.isDefined) ppt.get.value.asInstanceOf[Int] else -1
//                writer.println(randomDay + ";" + pfsDay +";" + pptDay)
//               }
       writer.println(simTrial.getSubjects.size)
      }

      writer.close()

      println("runs: " + simTrials.size)

      1 must be(1)
    }
  }
}

