import sbt.Keys.unmanagedJars
import sbt.file

ThisBuild / version      := "1.0"
ThisBuild / scalaVersion := "2.12.11"
ThisBuild / organization := "org.chainsaw"

// SpinalHDL
val spinalVersion    = "1.9.4"
val spinalCore       = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib        = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

// Chisel
val chiselVersion = "3.5.6"
val chisel        = "edu.berkeley.cs" %% "chisel3" % chiselVersion
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)

// JGraphT JDK=1.8
val jGraphTVersion = "1.4.0" // last version compatible with Java 1.8
val jGraphCore     = "org.jgrapht" % "jgrapht-core" % jGraphTVersion
val jGraphExt      = "org.jgrapht" % "jgrapht-ext" % jGraphTVersion

// optimus
val optimusVersion = "3.2.4"
val optimus        = "com.github.vagmcs" %% "optimus" % optimusVersion
val optimusOj      = "com.github.vagmcs" %% "optimus-solver-oj" % optimusVersion
val optimusLp      = "com.github.vagmcs" %% "optimus-solver-lp" % optimusVersion

val djlBackend = "ai.djl.pytorch" % "pytorch-engine" % "0.20.0"

// for config file
// djl for AI & array manipulation
val djlCore   = "ai.djl" % "api" % "0.20.0"
val snakeYaml = "org.yaml" % "snakeyaml" % "1.33"

// for design pre-placement
val rapidwright = "com.xilinx.rapidwright" % "rapidwright" % "2022.2.1"

// for algebra
val spireVersion = "0.17.0" // last version compatible with Scala 2.12
val spire = "org.typelevel" %% "spire" % spireVersion
val algebird = "com.twitter" %% "algebird-core" % "0.13.10"


lazy val Chainsaw = (project in file("."))
  .settings(
    name := "Chainsaw",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin),
    libraryDependencies ++= Seq(jGraphCore, jGraphExt),
    libraryDependencies += chisel,
    libraryDependencies += "org.scalanlp" %% "breeze" % "1.0",          // for numeric & matrix operations
    libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.7", // for finite field operations
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9",    // for scala test
    libraryDependencies ++= Seq(optimus, optimusOj, optimusLp),
    libraryDependencies += "com.google.code.gson" % "gson" % "2.10",
    libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.14.1",
    libraryDependencies += djlCore,
    libraryDependencies += djlBackend,
    libraryDependencies += snakeYaml,
    libraryDependencies += rapidwright,
    libraryDependencies += "org.scalanlp" %% "breeze-viz" % "2.1.0",
    //    libraryDependencies += "cplex.maven" % "cplex" % "12.8", // for cplex solver
    libraryDependencies += "com.github.dwickern" %% "scala-nameof" % "4.0.0" % "provided",
//    libraryDependencies += algebird
  )

fork := true
