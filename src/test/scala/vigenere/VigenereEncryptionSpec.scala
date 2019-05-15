package vigenere

import org.scalatest._

class VigenereEncryptionSpec extends FlatSpec with Matchers {

  val PlainText = "LOREM IPSUM DOLOR SIT AMET CONSECTETUR ADIPISCING ELIT ITA CUM EA VOLUNT RETINERE QUAE SUPERIORI SENTENTIAE CONVENIUNT IN ARISTONEM INCIDUNT"

  "Encrypting then decrypting using Vigenere Cipher" should "return the original value for a key larger than the text" in {
  	val key = s"${PlainText} AND THIS"
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for a single character key" in {
  	val key = "J"
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for a five character key" in {
    val key = "FEVER"
    testEncryptThenDecrypt(key)
  }

  it should "return the original value for two words separated by a space" in {
    val key = "RADIO HEART"
    testEncryptThenDecrypt(key)
  }

  it should "return the original value for an arctic monkeys song" in {
    val key = "FOUR STARS OUT OF FIVE"
    testEncryptThenDecrypt(key)
  }

  it should "return the original value for a lower case key" in {
    val key = "something wicked this way comes"
    testEncryptThenDecrypt(key)
  }

  def testEncryptThenDecrypt(key: String) {
  	val encrypted = VigenereEncryption.encryptText(PlainText, key)
  	VigenereEncryption.decryptText(encrypted, key) shouldEqual PlainText.toUpperCase
  }

}
