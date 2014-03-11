package org.scalastyle

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import com.typesafe.config.ConfigFactory

class ConfigTest extends AssertionsForJUnit{

  @Test
  def testHeaderOutputMsg(): Unit = {
    val messageHelper = new MessageHelper(ConfigFactory.load())
    val msg = Output.findMessage(messageHelper, "header.matches", List(), None)
    assert(msg.equals("Header does not match expected text"))
  }

}
