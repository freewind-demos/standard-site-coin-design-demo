package example
import example.Hello.huobi.HuobiCoins

object Hello extends App {

  trait Site
  trait Standard extends Site

  trait Coin[T <: Site] {
    val value: String
    def toStandard: Coin[Standard]
    def canEqual(other: Any): Boolean = other.isInstanceOf[Coin[T]]
    override def hashCode(): Int = value.hashCode()
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Coin[T] => (that canEqual this) && this.value == that.value
      case _ => false
    }
    override def toString = s"Coin($value)"
  }

  trait AssetCoin[T <: Site] extends Coin[T] {
    def toStandard: AssetCoin[Standard]
    override def toString = s"AssetCoin($value)"
  }
  trait QuoteCoin[T <: Site] extends Coin[T] {
    override def toStandard: QuoteCoin[Standard]
    override def toString = s"QuoteCoin($value)"
  }

  trait SiteCoins[T <: Site] {
    def coin(standard: Coin[Standard]): Coin[T] = coin(standard.value)
    def coin(_value: String): Coin[T] = new Coin[T] {
      override val value: String = _toSite(_value)
      override def toStandard: Coin[Standard] = StandardCoins.quote(_toStandard(value))
    }
    def asset(standard: AssetCoin[Standard]): AssetCoin[T] = asset(standard.value)
    def asset(_value: String): AssetCoin[T] = new AssetCoin[T] {
      override val value: String = _toSite(_value)
      override def toStandard: AssetCoin[Standard] = StandardCoins.asset(_toStandard(value))
    }
    def quote(standard: QuoteCoin[Standard]): QuoteCoin[T] = quote(standard.value)
    def quote(_value: String): QuoteCoin[T] = new QuoteCoin[T] {
      override val value: String = _toSite(_value)
      override def toStandard: QuoteCoin[Standard] = StandardCoins.quote(_toStandard(value))
    }
    def _toStandard(value: String): String
    def _toSite(value: String): String
  }

  object StandardCoins extends SiteCoins[Standard] {
    override def _toStandard(value: String): String = value.toUpperCase
    override def _toSite(value: String): String = value
  }

  case class Symbol[T <: Site](base: AssetCoin[T], quote: QuoteCoin[T])

  object huobi {
    trait Huobi extends Site

    object HuobiCoins extends SiteCoins[Huobi] {
      def _toSite(v: String) = v.toLowerCase
      def _toStandard(v: String) = v.toUpperCase
    }

    def fetchPrice(symbol: Symbol[Huobi]): Double = {
      println(symbol.base)
      println(symbol.quote)
      111
    }
  }

  object binance {
    trait Binance extends Site
    object BinanceCoins extends SiteCoins[Binance] {

      private val siteMapping = Map("BCH" -> "BCC")
      private val standardMapping = siteMapping.map(_.swap)

      override def _toStandard(v: String): String = {
        val s = v.toUpperCase
        standardMapping.getOrElse(s, s)
      }
      override def _toSite(v: String): String = {
        val s = v.toUpperCase
        siteMapping.getOrElse(s, s)
      }
    }

    def fetchPrice(symbol: Symbol[Binance]): Double = 222
  }

  val result = huobi.fetchPrice(Symbol(HuobiCoins.asset("BTM"), HuobiCoins.quote("BTC")))
  println(result)

}
