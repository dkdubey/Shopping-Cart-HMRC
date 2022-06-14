package com.daya.hmrc

trait DiscountOffer {
  def calcAdjustment(items: Seq[ShopItem]): Double
}

object AppleBogofOffer extends DiscountOffer {
  val offerQuantity: Int = 2

  override def calcAdjustment(items: Seq[ShopItem]): Double = {
    val apples = items.filter(_ match { case Apple => true case _ => false })
    val eligibleQuantity = apples.size - (apples.size % offerQuantity)
    0.0 - ((eligibleQuantity / offerQuantity) * Apple.price)
  }
}

object Oranges3For2Offer extends DiscountOffer {
  val offerQuantity: Int = 3

  override def calcAdjustment(items: Seq[ShopItem]): Double = {
    val oranges = items.filter(_ match { case Orange => true case _ => false})
    val eligibleQuantity = oranges.size - (oranges.size % offerQuantity)
    0.0 - ((eligibleQuantity / offerQuantity) * Orange.price)
  }
}
