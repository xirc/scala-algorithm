val ScalaTestVersion = "3.2.16"
val CatsVersion = "2.9.0"

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-Xsource:3",
  "-Werror"
)
ThisBuild / Compile / doc / autoAPIMappings := true
ThisBuild / git.remoteRepo := "git@github.com:xirc/scala-algorithm.git"

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.scalatest" %% "scalatest-wordspec" % ScalaTestVersion % Test
    )
  )

lazy val docs = (project in file("docs"))
  .enablePlugins(
    ParadoxSitePlugin,
    ParadoxMaterialThemePlugin,
    SiteScaladocPlugin,
    ScalaUnidocPlugin,
    GhpagesPlugin
  )
  .aggregate(core)
  .dependsOn(core)
  .settings(
    name := "Scala Algorithm",
    Compile / paradoxMaterialTheme ~= {
      _.withRepository(uri("https://github.com/xirc/scala-algorithm"))
    },
    Compile / paradoxProperties ++= Map(
      "scaladoc.algo.base_url" -> s"api"
    ),
    ScalaUnidoc / siteSubdirName := "api",
    addMappingsToSiteDir(
      ScalaUnidoc / packageDoc / mappings,
      ScalaUnidoc / siteSubdirName
    )
  )

addCommandAlias(
  name = "ciFormat",
  Seq(
    "scalafmtSbt",
    "scalafmtAll"
  ).mkString(";")
)

addCommandAlias(
  "ciCheck",
  Seq(
    "clean",
    "scalafmtSbtCheck",
    "scalafmtCheckAll",
    "coverage",
    "Test / compile",
    "test",
    "coverageAggregate",
    "makeSite"
  ).mkString(";")
)
