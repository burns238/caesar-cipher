package dataencryptionstandard

import org.scalatest._
import dataencryptionstandard.DES._
import scala.concurrent.Future

import scala.util.Success

class CryptoSpec extends AsyncFlatSpec with Matchers {

  "decrypt" should "decrypt a numerical string we've encrypted" in {
    testEncryption("87878787","aB88eiP3")
  }

  it should "decrypt a string with spaces we've encrypted" in {
    testEncryption("one time","aB88eiP3")
  }

  it should "decrypt a string with a mix of casing" in {
    testEncryption("OneTimes","aB88eiP3")
  }

  it should "decrypt a string with a numerical key" in {
    testEncryption("87878787","12345678")
  }

  it should "decrypt a string longer than 8 characters" in {
  	val initialString = "LOREM IPSUM DOLOR SIT AMET CONSECTETUR ADIPISCING ELIT ITA CUM EA VOLUNT RETINERE QUAE SUPERIORI SENTENTIAE CONVENIUNT IN ARISTONEM INCIDUNT"
    testEncryption(initialString, "aa236eGh")
  }

  it should "decrypt a string shorter than 8 characters" in {
    testEncryption("Ab3","aa236eGh")
  }

  it should "decrypt a string that's ridiculously long" in {
    testEncryption(LoremIpsumHelper.LoremIpsum, "aa236eGh")
  }

  def testEncryption(initialString: String, key: String): Future[Assertion] = {
      for {
        enc <- encrypt(initialString, key)
        dec <- decrypt(enc.get, key)
      } yield dec.map(_.trim) shouldEqual Success(initialString)
  }

}
