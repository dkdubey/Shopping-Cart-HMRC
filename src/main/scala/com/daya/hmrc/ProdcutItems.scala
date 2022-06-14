package com.daya.hmrc

sealed trait ShopItem {
  def price: Double
}

case object Apple extends ShopItem {
  val price = 0.6
}

case object Orange extends ShopItem {
  val price = 0.25
}