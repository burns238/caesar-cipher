package caesar

import org.scalatest._

class CaesarEncryptionSpec extends FlatSpec with Matchers {

  val PlainText = "LOREM IPSUM DOLOR SIT AMET CONSECTETUR ADIPISCING ELIT ITA CUM EA VOLUNT RETINERE QUAE SUPERIORI SENTENTIAE CONVENIUNT IN ARISTONEM INCIDUNT"

  "Encrypting then decrypting using Caesar Cipher" should "return the original value for key 1" in {
  	val key = 1
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for key 2" in {
  	val key = 2
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for key 3" in {
  	val key = 3
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for key 6" in {
  	val key = 6
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for key 8" in {
  	val key = 8
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for key 10" in {
  	val key = 10
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for key 12" in {
  	val key = 12
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for key 15" in {
  	val key = 15
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for key 26" in {
  	val key = 26
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for key 9000" in {
  	val key = 9000
  	testEncryptThenDecrypt(key)
  }

  it should "return the original value for a large key" in {
  	val key = 1827236238
  	testEncryptThenDecrypt(key)
  }

  def testEncryptThenDecrypt(key: Int) {
  	val encrypted = CaesarEncryption.encryptText(PlainText, key)
  	CaesarEncryption.decryptText(encrypted, key) shouldEqual PlainText
  }

}
