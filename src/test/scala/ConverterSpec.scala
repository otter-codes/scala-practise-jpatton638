import org.scalatest._

class ConverterSpec extends WordSpec with MustMatchers {

  "Converter" must {

    "Give Arabic equivalent when a Roman Numeral is given" in {
      Converter.convert("V") mustEqual 5
    }

    "Give Arabic equivalent when a sequence of Roman Numerals are given" in {
      Converter.convert("VIII") mustEqual 8
    }

    "Handle two character numerals as correct number" in {
      Converter.convert("CIV") mustEqual 104
    }

    "Handle illegal Roman Numerals" in {
      val caught = intercept[IllegalArgumentException] {
        Converter.convert("VIIQ")
      }
      assert(caught.getMessage.equals("Invalid character in input. Valid characters are I,V,X,L,C,D,M"))
    }

  }

}
