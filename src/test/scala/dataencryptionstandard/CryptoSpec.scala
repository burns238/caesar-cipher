package dataencryptionstandard

import org.scalatest._
import dataencryptionstandard.DataEncryptionStandard._

class CryptoSpec extends FlatSpec with Matchers {

  "decrypt" should "decrypt a numerical string we've encrypted" in {
  	val initialString = "87878787"
  	val key = "aB88eiP3"
    val encrypted = encrypt(initialString, key)
    encrypted shouldEqual "Á$tn¡þ°O"
    decrypt(encrypted, key) shouldEqual initialString
  }

  it should "decrypt a string with spaces we've encrypted" in {
  	val initialString = "one time"
  	val key = "aB88eiP3"
    val encrypted = DataEncryptionStandard.encrypt(initialString, key)
    decrypt(encrypted, key) shouldEqual initialString
  }

  it should "decrypt a string with a mix of casing" in {
  	val initialString = "OneTimes"
  	val key = "aB88eiP3"
    val encrypted = encrypt(initialString, key)
    decrypt(encrypted, key) shouldEqual initialString
  }

  it should "decrypt a string with a numerical key" in {
  	val initialString = "87878787"
  	val key = "12345678"
    val encrypted = encrypt(initialString, key)
    decrypt(encrypted, key) shouldEqual initialString
  }

  it should "decrypt a string longer than 8 characters" in {
  	val initialString = "LOREM IPSUM DOLOR SIT AMET CONSECTETUR ADIPISCING ELIT ITA CUM EA VOLUNT RETINERE QUAE SUPERIORI SENTENTIAE CONVENIUNT IN ARISTONEM INCIDUNT"
  	val key = "aa236eGh"
    val encrypted = encrypt(initialString, key)
    decrypt(encrypted, key) shouldEqual initialString
  }

  it should "decrypt a string shorter than 8 characters" in {
  	val initialString = "Ab3"
  	val key = "aa236eGh"
    val encrypted = encrypt(initialString, key)
    decrypt(encrypted, key) shouldEqual initialString
  }

  it should "decrypt a string that's ridiculously long" in {
  	val initialString = LoremIpsumHelper.LoremIpsum
  	val key = "aa236eGh"
    val encrypted = encrypt(initialString, key)
    decrypt(encrypted, key) shouldEqual initialString
  }

}
