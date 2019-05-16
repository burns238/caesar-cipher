package encryption

import org.scalatest._

class OneTimePadEncryptionSpec extends FlatSpec with Matchers {

  val PlainText = "LOREM IPSUM DOLOR SIT AMET CONSECTETUR ADIPISCING ELIT ITA CUM EA VOLUNT RETINERE QUAE SUPERIORI SENTENTIAE CONVENIUNT IN ARISTONEM INCIDUNT"

  "Encrypting then decrypting using OTP" should "return the original value" in {
  	testEncryptThenDecrypt(PlainText)
  }

  def testEncryptThenDecrypt(plainText: String) {
  	val (encrypted, key) = OneTimePadEncryption.encryptTextAndReturnKey(plainText)
  	OneTimePadEncryption.decryptText(encrypted, key) shouldEqual plainText
  }

  "Encrypting using OTP" should "return a key of the same length as the plain text" in {
    val (encrypted, key) = OneTimePadEncryption.encryptTextAndReturnKey(PlainText)
    PlainText.length shouldEqual key.length
  }

  it should "return a cipher text different to the plain text" in {
    val (encrypted, key) = OneTimePadEncryption.encryptTextAndReturnKey(PlainText)
    (encrypted == PlainText) shouldBe false
  }

}
