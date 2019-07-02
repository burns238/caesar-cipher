package dataencryptionstandard

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import dataencryptionstandard.BitFunctions._
import dataencryptionstandard.SubkeyFunctions._
import cats.implicits._

import scala.util.Try

object DES {
	
	private def crypto(plainText: String, subKeys: Try[Seq[Vector[Char]]]): Future[Try[String]] = {

		val textChunks = plainText.grouped(8).toVector

		val futures =	for ((c, i) <- textChunks.zipWithIndex)
				yield Future(cryptChunk(c.padTo(8,' '), subKeys, i))

		//When we have the futures results back we need to order by the index and map it back to our initial string
		Future.sequence(futures).map(t => t.sequence.map(list =>
				list.sortBy(_._2).map(_._1).mkString))
	}

	def encrypt(plainText: String, key: String): Future[Try[String]] =
		crypto(plainText, generateSubKeys(key))

	//To decrypt, we use the same algorithm but reverse the sub keys we generate from the key passed in
	def decrypt(plainText: String, key: String): Future[Try[String]] =
		crypto(plainText, generateSubKeys(key).map(_.reverse))

}