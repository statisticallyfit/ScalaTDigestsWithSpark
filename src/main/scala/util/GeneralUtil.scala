package util

/**
 *
 */

import scala.reflect.runtime.universe._


object GeneralUtil {

	// GOAL:
	// given: [0,0,0, 2,2,2,2,2,3,3,3,4,4, 5,5,5,5,...]
	// return [[0,0,0], [2,2,2,2,2], [3,3,3], [4,4], [5,5,5...], ...]
	def splitGroups(lst: Seq[Int]): Seq[Seq[Int]] = {
		def splitter(acc: Seq[Seq[Int]], processList: Seq[Int]): Seq[Seq[Int]] = {
			if(processList.isEmpty){
				return acc
			} else {
				if(acc.isEmpty){
					splitter(List(List(processList.head)), processList.tail)
				}
				else if(processList.head == acc.last.head){ // , tag onto end off last one in accumulator
					splitter(acc.init :+ (acc.last :+ processList.head), processList.tail)
				} else { // else the split happens here [n2, n2, ...] vs. [[n1,n1], ...]
					splitter(acc :+ List(processList.head), processList.tail)
				}
			}
		}
		splitter(List(), lst)
	}



	// Just for sake of the argument: going to use this to make a sequence of T values at runtime
	/*def convertToT[T: Numeric](x: T): T = {
		val result = new java.lang.Double(implicitly[Numeric[T]].toDouble(x))
		(x match {
			case x: Double => result
			case x: Int => result.toInt
			case x: Float => result.toFloat
			case x: Long => result.toLong
		}).asInstanceOf[T]
	}*/


	// NOTE: source here https://stackoverflow.com/a/27213057 for generateTSeq
	// Could have done @specialized

	def generateTSeq[T: TypeTag](xmin: Double, xmax: Double)(implicit evNum: Numeric[T]): Seq[T] = {
		//NOTE: assume that T == Real (BigDecimal) because the xmin, xmax are doubles
		// Goal: intend for this function to just connect smoothly from the KSD in Algebird, where xmin, xmax are
		// doubels from the keymax cluster number in the tdigest objects

		assert(typeOf[T].toString.split('.').last == "Real")

		val step: Double = (xmax - xmin) / 10000.0

		(BigDecimal(xmin) to xmax by step).map(_.asInstanceOf[T]).asInstanceOf[Seq[T]]

	}

	def generateTSeq[T: TypeTag](xmin: T, xmax: T)(implicit evNum: Numeric[T]): Seq[T] = {
		val givenXmin: Double = new java.lang.Double(evNum.toDouble(xmin))
		val givenXmax: Double = new java.lang.Double(evNum.toDouble(xmax))

		//println(xmin.getClass.getSimpleName)

		(typeOf[T].toString.split('.').last match {
			case "IntZ" => (BigInt(givenXmin.toInt) to givenXmax.toInt by 1).map(_.asInstanceOf[T])
			case "Real" => {

				val step: Double = (givenXmax - givenXmin) / 10000.0

				(BigDecimal(givenXmin) to givenXmax by step).map(_.asInstanceOf[T])
			}
		}).asInstanceOf[Seq[T]]
	}
}
