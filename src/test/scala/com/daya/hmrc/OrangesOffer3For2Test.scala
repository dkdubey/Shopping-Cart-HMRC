package com.daya.hmrc

import org.scalatest.{FlatSpec, Matchers}

class OrangesOffer3For2Test extends FlatSpec with Matchers {

  val offerUnderTest = Oranges3For2Offer

  "Oranges3For2Offer" should "calcAdjustment for min eligible quantity" in {
    // Given
    val oranges = Seq(Orange, Orange, Orange)

    // When
    val adjustment = Oranges3For2Offer.calcAdjustment(oranges)

    // Then
    adjustment should be(-0.25)
  }

  it should "Apply offer multiple times when there are multiple eligible quantities" in {
    // Given
    val oranges = Seq(Orange, Orange, Orange, Orange, Orange, Orange, Orange)

    // When
    val adjustment = Oranges3For2Offer.calcAdjustment(oranges)

    // Then
    adjustment should be(-0.5)
  }

  it should "Apply offer correctly for a mixed list of items" in {
    // Given
    val oranges = Seq(Orange,Apple, Orange,Apple, Orange)

    // When
    val adjustment = Oranges3For2Offer.calcAdjustment(oranges)

    // Then
    adjustment should be(-0.25)
  }

}
