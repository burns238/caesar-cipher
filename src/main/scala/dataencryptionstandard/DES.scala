package dataencryptionstandard

import scala.concurrent.Future
import dataencryptionstandard.SubkeyFunctions._

import scala.util.Try

object DES {

	def encrypt(plainText: String, key: String): Future[Try[String]] =
		CryptoFunctions.crypto(plainText, generateSubKeys(key))

	//To decrypt, we use the same algorithm but reverse the sub keys we generate from the key passed in
	def decrypt(plainText: String, key: String): Future[Try[String]] =
		CryptoFunctions.crypto(plainText, generateSubKeys(key).map(_.reverse))

}