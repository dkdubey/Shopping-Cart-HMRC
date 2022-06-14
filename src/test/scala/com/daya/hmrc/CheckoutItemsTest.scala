package com.daya.hmrc

import org.scalatest.{FlatSpec, Matchers}

class CheckoutItemsTest extends FlatSpec with Matchers {


  "Checkout" should "correctly calculate the value of a shopping cart" in {
    // Given
    val checkoutUnderTest = new ItemCheckout
    val inputProducts = Seq(Apple, Apple, Orange, Apple)

    // When
    val checkoutSummary: CheckoutSummary = checkoutUnderTest.calculate(inputProducts)

    // The
    checkoutSummary.subTotal should be(2.05)
  }

  it should "handle an empty shop item seq" in {
    // Given
    val checkoutUnderTest = new ItemCheckout
    val inputProducts = Nil

    // When
    val checkoutSummary: CheckoutSummary = checkoutUnderTest.calculate(inputProducts)

    // Then
    checkoutSummary.subTotal should be(0.0)
  }

  it should "apply bogof offers to apples in the shopping cart when there is an eligible quantity" in {
    // Given
    val checkout = new ItemCheckout(Seq(AppleBogofOffer))
    val inputItems = Seq(Apple, Apple, Apple, Apple)

    // When
    val checkoutSummary = checkout.calculate(inputItems)

    // Then
    checkoutSummary.subTotal should be(1.2)
  }

  it should "not apply bogof offers to apples in the shopping cart when there there is an insufficient quantity" in {
    // Given
    val checkout = new ItemCheckout(Seq(AppleBogofOffer))
    val inputItems = Seq(Apple)

    // When
    val checkoutSummary = checkout.calculate(inputItems)

    // Then
    checkoutSummary.subTotal should be(0.6)
  }

  it should "apply 3 for 2 offers on oranges when there is at least 3 oranges in the basket" in {
    // Given
    val checkout = new ItemCheckout(Seq(Oranges3For2Offer))
    val inputItems = Seq(Orange, Orange, Orange)

    // When
    val checkoutSummary = checkout.calculate(inputItems)

    // Then
    checkoutSummary.subTotal should be(0.5)
  }

  it should "not apply 3 for 2 on oranges when there is less than 3 oranges in the basket" in {
    // Given
    val checkout = new ItemCheckout(Seq(Oranges3For2Offer))
    val inputItems = Seq(Orange, Orange)

    // When
    val checkoutSummary = checkout.calculate(inputItems)

    // Then
    checkoutSummary.subTotal should be(0.5)
  }

  it should "apply multiple offers to a mixed item list with with eligible quantities" in {
    // Given
    val checkout = new ItemCheckout(Seq(AppleBogofOffer, Oranges3For2Offer))
    val inputItems = Seq(Orange, Apple, Orange, Apple, Orange)

    // When
    val checkoutSummary = checkout.calculate(inputItems)

    // Then
    checkoutSummary.subTotal should be(1.1)
  }

  it should "not apply offers to a mixed item list when min quantities not satisfied" in {
    val checkout = new ItemCheckout(Seq(AppleBogofOffer, Oranges3For2Offer))
    val inputItems = Seq(Orange, Apple, Orange)

    // When
    val checkoutSummary = checkout.calculate(inputItems)

    // Then
    checkoutSummary.subTotal should be(1.1)
  }

}