package Project_IsarnSketchesAlgebirdAPI

/**
 * SOURCE CODE OF FILE: https://github.com/erikerlandson/isarn-sketches-algebird-api/blob/blog/t_digest_sum/src/main/scala/org/isarnproject/sketchesAlgebirdAPI/implicits.scala
 */
import scala.language.implicitConversions

import com.twitter.algebird.Monoid

import org.isarnproject.sketches.TDigest



/**
 * Implicit definitions for Algebird objects based on TDigest
 */
object implicits {
	private val tDigestMonoid: Monoid[TDigest] = AlgebirdFactory.tDigestMonoid

	/**
	 * Implicit definition of TDigest as an Algebird Monoid
	 * @note the implicit Monoid[TDigest] uses default sketch resolution parameter delta.
	 * See AlgebirdFactory for non-default delta values
	 */
	implicit def tDigestMonoidImplicit: Monoid[TDigest] = tDigestMonoid
}
