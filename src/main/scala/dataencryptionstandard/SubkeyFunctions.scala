package dataencryptionstandard

import scala.annotation.tailrec

import dataencryptionstandard.Permutation._
import dataencryptionstandard.BitMappings._
import dataencryptionstandard.BitFunctions._

object SubkeyFunctions {
	
	def leftShiftKey(subKey: Vector[Char], roundNumber: Int): Vector[Char] = {
		val (l,r) = split56(subKey)
		leftShift(l, Shifts(roundNumber-1)) ++ leftShift(r, Shifts(roundNumber-1))
	} 

	def generateSubKeys(key: String): Seq[Vector[Char]] = {
		val permuted1 = permutedChoice1(stringToBits(key))
		generateKeys(permuted1, Seq(), 1)

	}

	@tailrec
	private def generateKeys(initialKey: Vector[Char], subKeys: Seq[Vector[Char]], count: Int): Seq[Vector[Char]] = {
		count match {
			case 17 => subKeys
			case c => {
				val shiftedKey = leftShiftKey(initialKey, c)
				val newSubKey = permutedChoice2(shiftedKey)
				generateKeys(shiftedKey, subKeys :+ newSubKey, c+1)
			}
		}
	}

}