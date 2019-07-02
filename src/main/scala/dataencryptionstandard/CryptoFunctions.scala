package dataencryptionstandard

import dataencryptionstandard.BitFunctions._
import dataencryptionstandard.BitMappings._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Try}
import cats.implicits._

object CryptoFunctions {

  def crypto(plainText: String, subKeys: Try[Seq[Vector[Char]]]): Future[Try[String]] = {

    val textChunks = plainText.grouped(8).toVector

    val futures =	for ((c, i) <- textChunks.zipWithIndex)
      yield Future(cryptChunk(c.padTo(8,' '), subKeys, i))

    //When we have the futures results back we need to order by the index and map it back to our initial string
    Future.sequence(futures).map(t => t.sequence.map(list =>
      list.sortBy(_._2).map(_._1).mkString))
  }

  def cryptChunk(plainText: String, subKeys: Try[Seq[Vector[Char]]], index: Int): Try[(String, Int)] =
    for {
      bits    <- stringToBits(plainText)
      init    <- permutate(bits, InitialPermutation)
      (l, r) 	<- split(init)
      newBits <- round(l, r, subKeys, InitialRound)
      swapped <- swap(newBits)
      inverse <- permutate(swapped, FinalPermutation)
    } yield (bitsToString(inverse), index)

  //Recursive function to run through all 16 rounds. The initial roundCount needs to be 1.
  def round(leftBits: Vector[Char], rightBits: Vector[Char], subKeys: Try[Seq[Vector[Char]]], count: Int): Try[Vector[Char]] =
    count match {
      case 17 => Success(leftBits ++ rightBits)
      case c => for {
        key  						<- subKeys
        expanded 				<- permutate(rightBits, Expansion)
        xoredWithSubKey <- xor(expanded, key(c-1))
        sboxed 					<- applySboxes(xoredWithSubKey)
        permuted 				<- permutate(sboxed, RoundPermutation)
        next       			<- xor(permuted, leftBits)
        result 					<- CryptoFunctions.round(rightBits, next, subKeys, c+1)
      } yield result
    }

}
