package dataencryptionstandard

import dataencryptionstandard.BitFunctions._
import dataencryptionstandard.BitMappings._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Try}
import cats.implicits._

import scala.annotation.tailrec

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
      (l, r)  <- split(init)
      newBits <- round(Success(l), Success(r), subKeys, InitialRound)
      swapped <- swap(newBits)
      inverse <- permutate(swapped, FinalPermutation)
    } yield (bitsToString(inverse), index)

  //Recursive function to run through all 16 rounds. The initial roundCount needs to be 1.
  @tailrec
  def round(leftBits: Try[Vector[Char]], rightBits: Try[Vector[Char]], subKeys: Try[Seq[Vector[Char]]], count: Int): Try[Vector[Char]] =
    count match {
      case 17 => for {
        l <- leftBits
        r <- rightBits
      } yield l ++ r
      case c =>
        val subKeyIndex = c-1
        val nextRoundCount = c+1
        val nextRight = for {
          key       <- subKeys
          r         <- rightBits
          l         <- leftBits
          expanded  <- permutate(r, Expansion)
          xored     <- xor(expanded, key(subKeyIndex))
          sboxed    <- applySboxes(xored)
          permuted  <- permutate(sboxed, RoundPermutation)
          next      <- xor(permuted, l)
        } yield next
        CryptoFunctions.round(rightBits, nextRight, subKeys, nextRoundCount)
    }

}
