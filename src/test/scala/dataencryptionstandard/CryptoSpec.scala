package dataencryptionstandard

import org.scalatest._

class CryptoSpec extends FlatSpec with Matchers {

  "DES.decrypt" should "decrypt a numerical string we've encrypted" in {
  	val initialString = "87878787"
  	val key = "aB88eiP3"
    val encrypted = DES.encrypt(initialString, key)
    encrypted shouldEqual "Á$tn¡þ°O"
    DES.decrypt(encrypted, key) shouldEqual initialString
  }

  it should "decrypt a string with spaces we've encrypted" in {
  	val initialString = "one time"
  	val key = "aB88eiP3"
    val encrypted = DES.encrypt(initialString, key)
    DES.decrypt(encrypted, key) shouldEqual initialString
  }

  it should "decrypt a string with a mix of casing" in {
  	val initialString = "OneTimes"
  	val key = "aB88eiP3"
    val encrypted = DES.encrypt(initialString, key)
    DES.decrypt(encrypted, key) shouldEqual initialString
  }

  it should "decrypt a string with a numerical key" in {
  	val initialString = "87878787"
  	val key = "12345678"
    val encrypted = DES.encrypt(initialString, key)
    DES.decrypt(encrypted, key) shouldEqual initialString
  }

}
