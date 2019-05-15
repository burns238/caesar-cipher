package caesar

import org.scalatest._

class EncryptionSpec extends FlatSpec with Matchers {
  "Encrypting then decrypting" should "return the original value for key 1" in {
  	val plainText = "HELLO THERE"
  	val key = 1
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for key 2" in {
  	val plainText = "HELLO THERE"
  	val key = 2
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for key 3" in {
  	val plainText = "HELLO THERE"
  	val key = 3
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for key 6" in {
  	val plainText = "HELLO THERE"
  	val key = 6
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for key 8" in {
  	val plainText = "HELLO THERE"
  	val key = 8
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for key 10" in {
  	val plainText = "HELLO THERE"
  	val key = 10
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for key 12" in {
  	val plainText = "HELLO THERE"
  	val key = 12
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for key 15" in {
  	val plainText = "HELLO THERE"
  	val key = 15
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for key 26" in {
  	val plainText = "HELLO THERE"
  	val key = 26
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for key 9000" in {
  	val plainText = "HELLO THERE"
  	val key = 9000
  	testEncryptThenDecrypt(plainText, key)
  }

  it should "return the original value for a large key" in {
  	val plainText = "HELLO THERE"
  	val key = 1827236238
  	testEncryptThenDecrypt(plainText, key)
  }

  def testEncryptThenDecrypt(plainText: String, key: Int) {
  	val encrypted = Encryption.encryptText(plainText, key)
  	Encryption.decryptText(encrypted, key) shouldEqual plainText
  }

}
