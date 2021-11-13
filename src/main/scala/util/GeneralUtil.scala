package util

/**
 *
 */
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
}
