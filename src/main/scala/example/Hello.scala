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
      override val value: String = _toSiteCoin(_value)
      override def toStandard: Coin[Standard] = StandardCoins.quote(_toStandardCoin(value))
    }
    def asset(standard: AssetCoin[Standard]): AssetCoin[T] = asset(standard.value)
    def asset(_value: String): AssetCoin[T] = new AssetCoin[T] {
      override val value: String = _toSiteCoin(_value)
      override def toStandard: AssetCoin[Standard] = StandardCoins.asset(_toStandardCoin(value))
    }
    def quote(standard: QuoteCoin[Standard]): QuoteCoin[T] = quote(standard.value)
    def quote(_value: String): QuoteCoin[T] = new QuoteCoin[T] {
      override val value: String = _toSiteCoin(_value)
      override def toStandard: QuoteCoin[Standard] = StandardCoins.quote(_toStandardCoin(value))
    }
    def _toStandardCoin(value: String): String
    def _toSiteCoin(value: String): String
  }

  trait SiteSymbols[T <: Site] {
    def symbol(_asset: AssetCoin[T], _quote: QuoteCoin[T]): Symbol[T] = new Symbol[T] {
      override val base: AssetCoin[T] = _asset
      override val quote: QuoteCoin[T] = _quote
      override def combined: String = _toSymbolCombined(_asset, _quote)
    }
    def _parseCombinedSymbol(combined: String): Symbol[T]
    def _toSymbolCombined(asset: AssetCoin[T], quote: QuoteCoin[T]): String
  }

  object StandardCoins extends SiteCoins[Standard] with SiteSymbols[Standard] {
    override def _toStandardCoin(value: String): String = value.toUpperCase
    override def _toSiteCoin(value: String): String = value
    override def _parseCombinedSymbol(combined: String): Symbol[Standard] = combined.split("-") match {
      case Array(b, q) => symbol(asset(b), quote(q))
      case _ => throw new Exception("Invalid symbol: " + combined)
    }
    override def _toSymbolCombined(asset: AssetCoin[Standard], quote: QuoteCoin[Standard]): String = s"${asset.value}-${quote.value}"
  }

  trait Symbol[T <: Site] {
    val base: AssetCoin[T]
    val quote: QuoteCoin[T]
    def combined: String
    def isBaseOn(coin: AssetCoin[T]): Boolean = this.base == coin
    def isQuoteOn(coin: QuoteCoin[T]): Boolean = this.quote == coin

    def toStandard: Symbol[Standard] = StandardCoins.symbol(base.toStandard, quote.toStandard)

    def canEqual(other: Any): Boolean = other.isInstanceOf[Symbol[T]]
    override def hashCode(): Int = (base.value + quote.value).hashCode()
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Symbol[T] => (that canEqual this) && this.base == that.base && this.quote == that.quote
      case _ => false
    }
  }

  object huobi {
    trait Huobi extends Site

    object HuobiCoins extends SiteCoins[Huobi] with SiteSymbols[Huobi] {
      override def _toSiteCoin(v: String) = v.toLowerCase
      override def _toStandardCoin(v: String) = v.toUpperCase
      override def _parseCombinedSymbol(combined: String): Symbol[Huobi] = {
        val quotes = List("usdt", "btc", "eth")
        quotes.map(_ -> combined)
          .find({ case (q, c) => c.endsWith(q) })
          .map({ case (q, c) => symbol(asset(c.stripSuffix(q)), quote(q)) })
          .getOrElse(throw new Exception("Invalid symbol"))
      }
      override def _toSymbolCombined(asset: AssetCoin[Huobi], quote: QuoteCoin[Huobi]): String = asset.value + quote.value
    }

    def fetchPrice(symbol: Symbol[Huobi]): Double = {
      println(symbol.base)
      println(symbol.quote)
      111
    }
  }

  object binance {
    trait Binance extends Site
    object BinanceCoins extends SiteCoins[Binance] with SiteSymbols[Binance] {

      private val siteMapping = Map("BCH" -> "BCC")
      private val standardMapping = siteMapping.map(_.swap)

      override def _toStandardCoin(v: String): String = {
        val s = v.toUpperCase
        standardMapping.getOrElse(s, s)
      }
      override def _toSiteCoin(v: String): String = {
        val s = v.toUpperCase
        siteMapping.getOrElse(s, s)
      }
      override def _parseCombinedSymbol(combined: String): Symbol[Binance] = {
        val quotes = List("USDT", "BTC", "ETH")
        quotes.map(_ -> combined)
          .find({ case (q, c) => c.endsWith(q) })
          .map({ case (q, c) => symbol(asset(c.stripSuffix(q)), quote(q)) })
          .getOrElse(throw new Exception(s"Invalid symbol: $combined"))
      }

      override def _toSymbolCombined(asset: AssetCoin[Binance], quote: QuoteCoin[Binance]): String = asset.value + quote.value
    }

    def fetchPrice(symbol: Symbol[Binance]): Double = 222
  }

  val result = huobi.fetchPrice(HuobiCoins.symbol(HuobiCoins.asset("BTM"), HuobiCoins.quote("BTC")))
  println(result)

}
