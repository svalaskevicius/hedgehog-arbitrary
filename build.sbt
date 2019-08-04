val hedgehogVersion = "7ab9f74d7ca93864170cda181f4b4909156c7413"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.url("bintray-scala-hedgehog", url("https://dl.bintray.com/hedgehogqa/scala-hedgehog"))(Resolver.ivyStylePatterns)

lazy val settings = Seq(
  scalaVersion := "2.13.0",
  organization := "com.svalaskevicius",
  organizationName := "svalaskevicius",
  startYear := Some(2019),
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-language:_",
    "-target:jvm-1.8",
    "-encoding",
    "UTF-8",
    "-Yrangepos",
    /* "-Xlog-implicits", */
    "-explaintypes"
  ),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
)

lazy val `hedgehog-arbitrary` = project
  .in(file("."))
  .settings(settings)
  .settings(
    name := "hedgehog-arbitrary",
    version := "0.0.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"     % "2.0.0-M4",
      "com.chuusai"   %% "shapeless"     % "2.3.3",
      "org.typelevel" %% "kittens"       % "1.0.0-SNAPSHOT",
      "hedgehog"      %% "hedgehog-core" % hedgehogVersion
    )
  )
  .settings(bintraySettings)

lazy val bintraySettings = Seq(
  bintrayOrganization := Some("svalaskevicius"),
  bintrayRepository := "scala-hedgehog-arbitrary",
  publishMavenStyle := false,
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0"))
)
