package encryption

import scala.util.Random

object OneTimePadEncryption {

	private def generateRandomKey(length: Int): String = {
		val keyChars = for (i <- 1 to length) yield {
			val millis = System.currentTimeMillis();
			val r = new Random(millis)
			EncryptionUtils.Alphabet.apply(r.nextInt(EncryptionUtils.Alphabet.length))
		}

		keyChars.mkString
	}

	def encryptTextAndReturnKey(plainText: String): (String, String) = {
		val stringKey = generateRandomKey(plainText.length)
		val intsKey = EncryptionUtils.encryptionKeyIntegersFromString(stringKey)
		(EncryptionUtils.shiftText(plainText, intsKey), stringKey)
	}

	def decryptText(cipherText: String, key: String): String = {
		EncryptionUtils.shiftText(cipherText, EncryptionUtils.decryptionKeyIntegersFromString(key))
	}


}