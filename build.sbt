name := "ScalaTDigestsWithSpark"

version := "0.1"

scalaVersion := "2.12.13"




// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:postfixOps", "-language:higherKinds", "-Ypartial-unification")

// disable updating dynamic revisions (including -SNAPSHOT versions)
offline := true


//For the Kind projector plugin
resolvers += Resolver.sonatypeRepo("releases")

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


	// Plotting libraries:
	// -- Breeze-viz
	"org.scalanlp" %% "breeze-viz" % "2.0",
	// -- Plotly
	"org.plotly-scala" %% "plotly-core" % "0.8.2",
	"org.plotly-scala" %% "plotly-render" % "0.8.2",


	//Scalaz
	"org.scalaz"      %% "scalaz-core"    % "7.3.5", // 7.3.0-M19
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
	"com.chuusai"     %% "shapeless"      % "2.3.7", // was 2.3.3
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

	"org.isarnproject" %% "isarn-collections" % "0.0.4",
	"org.isarnproject" %% "isarn-algebra-api" % "0.0.3",
	"org.isarnproject" %% "isarn-algebird-algebra-api" % "0.0.4",
	"com.twitter" %% "algebird-core" % "0.13.4",

	"org.isarnproject" %% "isarn-sketches-spark" % "0.5.2-sp3.0",


	// Spark
	"org.apache.spark" %% "spark-core" % "3.1.2",
	"org.apache.spark" %% "spark-sql" % "3.1.2", // % "provided",
	"org.apache.spark" %% "spark-mllib" % "3.1.2"

)
