package util

import flip.pdf.Sketch
import flip.implicits._

import scala.language.implicitConversions

/**
 *
 */

object EnhanceFlipSketchUpdate {
	implicit class MyMultipleUpdate[A](sketch: Sketch[A]) {

		/**
		 * Taking the Flip library definition but altering to pass in multiple as' at the same time
		 * GOAL: the Flip library's `updateTrace` just maps
		 * one A -> Sketch[A} but this function will map
		 * List[A] -> Sketch[A]
		 * @param aas
		 * @return list of Sketch[A]s corresponding to each of the List[A]'s
		 */
		def updateWithMany(aas: List[List[A]]): List[Sketch[A]] = {

			var temp: Sketch[A] = sketch
			aas.map { (as: List[A]) =>
				temp = temp.update(as:_*); temp
			}
			// Passing to this definition in the Flip code:
			/*def update(as: A*): Sketch[A] =
				Sketch.update(sketch, as.toList.map(a => (a, 1d)))*/
		}
	}
}
