package dataencryptionstandard

import org.scalatest._

class CryptoSpec extends FlatSpec with Matchers {

  "DES.encrypt" should "encrypt a string" in {
    DES.encrypt("87878787", "0E3292232") shouldEqual "00000000"
  }


}
