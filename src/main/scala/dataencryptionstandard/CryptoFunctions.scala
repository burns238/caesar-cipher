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
      (l, r)  <- split(init)
      newBits <- rounds(l, r, subKeys)
      swapped <- swap(newBits)
      inverse <- permutate(swapped, FinalPermutation)
    } yield (bitsToString(inverse), index)

  def rounds(leftBits: Vector[Char], rightBits: Vector[Char], subKeys: Try[Seq[Vector[Char]]]): Try[Vector[Char]] = {
    val range = 1 to 16
    val (newLeft, newRight) = range.foldLeft((Try(leftBits), Try(rightBits))){(tup, count) =>
      val currentLeftBits = tup._1
      val currentRightBits = tup._2
      val subKeyIndex = count-1
      (currentRightBits, applyRound(currentLeftBits, currentRightBits, subKeys.map(_(subKeyIndex))))
    }

    for {
      l <- newLeft
      r <- newRight
    } yield l ++ r
  }

  def applyRound(leftBits: Try[Vector[Char]], rightBits: Try[Vector[Char]], subKey: Try[Vector[Char]]): Try[Vector[Char]] =
    for {
      key       <- subKey
      r         <- rightBits
      l         <- leftBits
      expanded  <- permutate(r, Expansion)
      xored     <- xor(expanded, key)
      sboxed    <- applySboxes(xored)
      permuted  <- permutate(sboxed, RoundPermutation)
      next      <- xor(permuted, l)
    } yield next

}
