package dataencryptionstandard

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}

import dataencryptionstandard.BitFunctions._
import dataencryptionstandard.Permutation._
import dataencryptionstandard.SubkeyFunctions._
import dataencryptionstandard.Round._

object DES {
	
	private def crypto(plainText: String, subKeys: Seq[Vector[Char]]): String = {
		val textChunks: Vector[String] = plainText.grouped(8).toVector
		val futures: Vector[Future[(String,Int)]] = {
			val zipped = textChunks.zipWithIndex
			for ((c, i) <- zipped) yield cryptChunk(c.padTo(8,' '), subKeys, i)
		}
		val futSeq = Future.sequence(futures)
		//When we have the futures results back we need to order by the index and map it back to our initial string
		Await.result(futSeq, 2 second).sortBy(_._2).map(_._1).mkString 
	}

	private def cryptChunk(plainText: String, subKeys: Seq[Vector[Char]], index: Int): Future[(String, Int)] = {
		Future {
			val InitialRound = 1
			val (l, r) = (initialPermutation andThen split _)(stringToBits(plainText))
			val postRounds = round(l, r, subKeys, InitialRound)
			val finalBits = (swap _ andThen inversePermutation)(postRounds)
			(bitsToString(finalBits), index)
		}
	}

	def encrypt(plainText: String, key: String): String = {
		val subKeys = generateSubKeys(key)
		crypto(plainText, subKeys)
	}

	//To decrypt, we use the same algorithm but reverse the subkeys we generate from the key passed in
	def decrypt(plainText: String, key: String): String = {
		val subKeys = generateSubKeys(key).reverse
		crypto(plainText, subKeys).trim
	}

}