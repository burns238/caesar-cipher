package dataencryptionstandard

import dataencryptionstandard.BitFunctions._
import dataencryptionstandard.Sbox._

import scala.util.Try

object Round {

	//Recursive function to run through all 16 rounds. The initial roundCount needs to be 1.
	def round(leftBits: Vector[Char], rightBits: Vector[Char], subKeys: Try[Seq[Vector[Char]]], count: Int): Try[Vector[Char]] =
		count match {
			case 17 => Try(leftBits ++ rightBits)
			case c => for {
				key  		<- subKeys
				next 		<- encryptBits(leftBits, rightBits, key(c - 1))
				result 	<- round(rightBits, next, subKeys, c+1)
			} yield result
		}

	private def encryptBits(leftBits: Vector[Char], rightBits: Vector[Char], subKey: Vector[Char]): Try[Vector[Char]] =
		for {
			expanded 				<- expand(rightBits)
			xoredWithSubKey <- xor(expanded, subKey)
			sboxed 					<- applySboxes(xoredWithSubKey)
			permuted 				<- roundPermutation(sboxed)
			encrypted       <- xor(permuted, leftBits)
		} yield encrypted
	
}