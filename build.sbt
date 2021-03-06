name := "ScalaTDigestsWithSpark"

version := "0.1"

scalaVersion := "2.12.13"




// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:postfixOps", "-language:higherKinds", "-Ypartial-unification")

// disable updating dynamic revisions (including -SNAPSHOT versions)
offline := true


//For the Kind projector plugin
resolvers += Resolver.sonatypeRepo("releases")
resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
//resolvers += Resolver.bintrayRepo("cibotech", "public")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10")


libraryDependencies ++= Seq(
	//Scala Libraries
	"org.scala-lang" % "scala-library" % "2.12.13",
	"org.scala-lang" % "scala-compiler" % "2.12.13",
	"org.scala-lang" % "scala-reflect" % "2.12.13",

	//ScalaCheck
	"org.scalacheck" %% "scalacheck" % "1.15.4" % Test, // was 1.15.2
	// ScalaTest
	"org.scalatest" %% "scalatest" % "3.2.10" % Test,
	//Specs2
	"org.specs2" %% "specs2-core" % "4.12.9" % Test, // was 4.10.6
	"org.specs2" %% "specs2-scalacheck" % "4.12.9" % Test, // was 4.10.6
	//Discipline
	"org.typelevel" %% "discipline" % "0.11.1", //was 0.8


	// -- Apache commons lang3:
	"org.apache.commons" % "commons-lang3" % "3.12.0", // was 3.6


	// Math + Statistics libraries
	// -- Apache commons math3
	"org.apache.commons" % "commons-math3" % "3.6.1",
	// -- Breeze:
	"org.scalanlp" %% "breeze" % "2.0",
	// -- Probability Monad
	"org.jliszka" %% "probability-monad" % "1.0.4",
	// -- Smile (for distribution fitting to sample data)
	"com.github.haifengl" % "smile-math" % "2.6.0",


// Plotting libraries:
	// -- Breeze-viz
	"org.scalanlp" %% "breeze-viz" % "2.0",
	// -- Plotly
	"org.plotly-scala" %% "plotly-core" % "0.8.2",
	"org.plotly-scala" %% "plotly-render" % "0.8.2",
	// Evilplot
	"io.github.cibotech" %% "evilplot" % "0.8.1",
	"io.github.cibotech" %% "evilplot-repl" % "0.8.1",

	// NOTE: not using cats in this project, and took it out because including xxxnell / flip library said it needs
	//  cats 1.0.1 while mine is using 2.6.1
	//Scalaz
	/*"org.scalaz"      %% "scalaz-core"    % "7.3.5", // 7.3.0-M19
	//Cats
	//"org.typelevel"   %% "cats"           % "1.0.1",
	"org.typelevel" %% "cats-core" %        "2.6.1", //was 2.0.0
	"org.typelevel"   %% "cats-macros"           % "2.1.1", // was 2.0.0
	"org.typelevel"   %% "cats-kernel"           % "2.6.1", //was 2.0.0
	"org.typelevel"   %% "cats-laws"           % "2.6.1", // was 2.0.0
	"org.typelevel"   %% "cats-free"           % "2.6.1", // was 2.0.0
	"org.typelevel"   %% "cats-testkit"           % "2.6.1", // was 2.0.0
	"org.typelevel" %% "cats-effect" % "3.2.8",

	//Shapeless
	"com.chuusai"     %% "shapeless"      % "2.3.7", // was 2.3.3*/
	//Kind projector plugin
	//"org.spire-math" %% "kind-projector" % "0.9.10", // was 0.9.9

	//FunctionMeta library to print function name, arguments... for a function while inside that function
	//"com.github.katlasik" %% "functionmeta" % "0.4.1" % "provided",
	// sourcecode library to print function name passed as argument
	"com.lihaoyi" %% "sourcecode" % "0.2.7",


	// Matryoshka
	//"com.slamdata" %% "matryoshka-core" % "0.21.3",

	//Droste recursion schemes
	//	"io.higherkindness" %% "droste-core" % "0.8.0",
	//	"io.higherkindness" %% "droste-laws" % "0.8.0",
	//	"io.higherkindness" %% "droste-macros" % "0.8.0",
	//	/*"io.higherkindness" %% "droste-meta" % "0.8.0",
	//	"io.higherkindness" %% "droste-reftree" % "0.8.0",*/
	//	"io.higherkindness" %% "droste-scalacheck" % "0.8.0",




	// ISARN (T-Digests with Spark work)
	"org.isarnproject" %% "isarn-sketches" % "0.3.0",
	"org.isarnproject" % "isarn-sketches-java" % "0.3.0",
	"org.isarnproject" %% "isarn-sketches-spark" % "0.5.2-sp3.0",

	"org.isarnproject" %% "isarn-collections" % "0.0.4",
	"org.isarnproject" %% "isarn-algebra-api" % "0.0.3",
	"org.isarnproject" %% "isarn-algebird-algebra-api" % "0.0.4",
	"com.twitter" %% "algebird-core" % "0.13.4",

	// Another t-sketch library
	"com.xxxnell" %% "flip" % "0.0.4",

	// Spark
	"org.apache.spark" %% "spark-core" % "3.1.2",
	"org.apache.spark" %% "spark-sql" % "3.1.2", // % "provided",
	"org.apache.spark" %% "spark-mllib" % "3.1.2",


	// CDF Spline tutorial dependencies here: https://github.com/erikerlandson/cdf-splining-prototype/blob/master/cdf-splining-poc.ipynb
	"com.manyangled" % "gibbous" % "0.3.0", // Convex optimization
	"com.manyangled" % "snowball" % "0.3.0", // Monotonic splining

	//"com.cibo" %% "evilplot" % "0.8.0", // evilplot
	//"com.cibo" %% "evilplot-repl" % "0.8.0"// evilplot repl - makes plotting easier from repl (dynamic window)
	// TODO how to update this build.sbt if using Ammonite = https://hyp.is/QPtwKjmOEeytewuDfT7KyA/cibotech
	//  .github.io/evilplot/getting-started.html


	// T Digest divergence Experiment dependencies:
	// -- DivergenceExperiment:
	// https://github.com/erikerlandson/isarn-sketches-algebird-api/blob/blog/t_digest_sum/src/main/scala/org/isarnproject/sketchesAlgebirdAPI/AlgebirdFactory.scala#L111-L114
	// -- Dependencies:
	// https://github.com/erikerlandson/isarn-sketches-algebird-api/blob/blog/t_digest_sum/src/main/scala/org/isarnproject/sketchesAlgebirdAPI/AlgebirdFactory.scala#L111-L114
	"org.json4s" %% "json4s-jackson" % "4.0.3", //"3.2.10"
	"com.twitter" %% "algebird-core" % "0.13.8" //"0.12.1"
)
