package util

import util.distributionExtensions.distributions._

import smile.stat.distribution._

import scala.reflect.runtime.universe._

/**
 *
 */


object ConvertMyDistToSmileDist {


	// NOTE: using 'getdeclaredfields' works for my dist[T, D] construction, have to use 'getfields' for the smile dist
	def getDistParams[T: Numeric, D](dist: Distr[T, D]): Map[String, (AnyRef, String)] = {

		dist.getDist.getClass.getDeclaredFields.toList
			.map(f => {
				f.setAccessible(true) //;
				(f.getName, (f.get(dist.getDist), f.get(dist.getDist).getClass.getSimpleName))
			}).toMap
	}

	// D = DiscreteDistribution or ContinuousDistribution (in smile)

	type SmileDist = smile.stat.distribution.Distribution

	// NOTE: using 'getdeclaredfields' works for my dist[T, D] construction, have to use 'getfields' for the smile dist
	def getSmileDistParams(smileDist: SmileDist): Map[String, (AnyRef, String)] = {
		smileDist.getClass.getFields.toList
			.map(f => {
				f.setAccessible(true)
				(f.getName, (f.get(smileDist), f.get(smileDist).getClass.getSimpleName))
			}).toMap
	}



	//TODO now just instead map to the smile distribution one


	// USAGE
	// val res = myDistToSmileDist[IntZ, BinomialDist, BinomialDistribution](myb)
	//res: smile.stat.distribution.BinomialDistribution = Binomial Distribution(100, 0.4500)
	//myDistToSmileDist[Real, GammaDist, GammaDistribution](GammaDist(shape = 25.4, scale = 3.1))
	//res58: smile.stat.distribution.GammaDistribution = Gamma Distribution(3.1000, 25.4000)
	// NOTE: T must be IntZ / Real not Int / Double TODO fix
	import scala.language.higherKinds
	import scala.language.implicitConversions

	implicit class ToSmileDist[T: Numeric, D](distObj: Distr[T, D]) {

		def toSmileDist[S: TypeTag]: S = {
			import scala.reflect.runtime._
			val cm = universe.runtimeMirror(getClass.getClassLoader)
			import scala.tools.reflect.ToolBox
			val tb = cm.mkToolBox()

			val paramnameToValAndType: Map[String, (AnyRef, String)] = getDistParams(distObj)
			//mb: Map[String,(AnyRef, String)] = Map(numTrials -> (100,Integer), p -> (0.45,Double))

			val argStr: String = paramnameToValAndType.foldLeft("") { case (accStr, (paramName, (paramVal, paramType))) => {
				s"$accStr, $paramVal.asInstanceOf[$paramType]"
			}}.drop(2) // to drop the first " ,"

			// canonical name smile.stat.distribution.BinomialDistribution
			val classPckgNameStr: String = typeOf[S].toString

			val classStr: String = s"new $classPckgNameStr($argStr)"
			// NOTE: must have 'new' since smile's class is not a case class
			//tb.eval(tb.parse("new smile.stat.distribution.BinomialDistribution(10.asInstanceOf[Int], 0.4.asInstanceOf[Double])"))
			//res44: Any = Binomial Distribution(10, 0.4000)
			val parsed: Tree = tb.parse(classStr)
			val originalDist: S = tb.eval(parsed).asInstanceOf[S] //.asInstanceOf[Dist[T, D]]

			// TODO match up arg names maybe through dict of mydist-names TO smiledist-names (snagit)
			originalDist
		}
	}


	//def toSmiledist[T: Numeric, D, S: TypeTag](distObj: Dist[T, D]): S =
	// Some Source code = https://stackoverflow.com/a/59894917





	// USAGE:
	// scala> reflectDist[Real, BetaDist](BetaDist(4.5, 6.7))
	// or reflectDist(BetaDist(4.5, 6.7)
	//res30: util.distributionExtensions.distributions.BetaDist = BetaDist(4.5,6.7)
	// NOTE: must use my types Real / IntZ not Double / Int WARNING
	def reflectDist[T: Numeric, D](distObj: Distr[T, D]): D = {
		import scala.reflect.runtime._
		val cm = universe.runtimeMirror(getClass.getClassLoader)
		import scala.tools.reflect.ToolBox
		val tb = cm.mkToolBox()

		//val mb = getDistParams(BinomialDist(100, 0.45))

		val paramnameToValAndType: Map[String, (AnyRef, String)] = getDistParams(distObj)
		//mb: Map[String,(AnyRef, String)] = Map(numTrials -> (100,Integer), p -> (0.45,Double))

		/*val (nv, nt) = (paramnameToValAndType.get("numTrials").get._1, paramnameToValAndType.get("numTrials").get._2)
		val (pv, pt) = (paramnameToValAndType.get("p").get._1, paramnameToValAndType.get("p").get._2)*/

		val argStr: String = paramnameToValAndType.foldLeft("") { case (accStr, (paramName, (paramVal, paramType))) => {
			s"$accStr, $paramVal.asInstanceOf[$paramType]"
		}
		}.drop(2) // to drop the first " ,"

		//val classNameStr: String = distObj.getClass.getSimpleName
		// canonical name getlso package + file name = util.distributionExtensions.distributions.BinomialDist
		val classPckgNameStr: String = distObj.getClass.getCanonicalName
		//val classStr: String = s"$classPckgNameStr($nv.asInstanceOf[$nt], $pv.asInstanceOf[$pt])"
		val classStr: String = s"$classPckgNameStr($argStr)"

		val parsed: Tree = tb.parse(classStr)
		val originalDist: D = tb.eval(parsed).asInstanceOf[D] //.asInstanceOf[Dist[T, D]]

		originalDist
	}
	// TODO similar = http://www.smartjava.org/content/scala-typeclass-explained-implement-stringread-function/
}
