package encryption

import scala.math.abs

object CaesarEncryption {

	def encryptText(plainText: String, key: Int): String = {
		EncryptionUtils.shiftText(plainText, List(key))
	}

	def decryptText(cipherText: String, key: Int): String = {
		EncryptionUtils.shiftText(cipherText, List(-key))
	}

}