import com.indoorvivants.detective.Platform
import scala.scalanative.build.*

name := "llvm-ir-lsp"

scalaVersion := "3.9.0-RC3"

enablePlugins(ScalaNativePlugin)
enablePlugins(ForgeNativeBinaryPlugin)

libraryDependencies ++= Seq(
  "com.github.j-mie6"           %% "parsley-debug"     % "5.0.0-M16",
  "com.github.j-mie6"           %% "parsley"           % "5.0.0-M16",
  "com.indoorvivants.detective" %% "platform"          % "0.1.0",
  "com.indoorvivants"           %% "opaque-newtypes"   % "0.1.0",
  "com.lihaoyi"                 %% "pprint"            % "0.9.6",
  "tech.neander"                %% "langoustine-app"   % "0.1.0",
  "com.outr"                    %% "scribe-file"       % "3.19.0",
  "com.indoorvivants"           %% "sn-demangler-core" % "0.2.0",

  // Test dependencies
  "org.typelevel" %% "weaver-cats" % "0.13.0" % Test,
  "org.scalameta" %% "munit"       % "1.3.4"  % Test
)
buildBinaryConfig ~= { _.withName("sniper") }

nativeLink / nativeConfig ~= {
  import scala.scalanative.build.*

  _.withIncrementalCompilation(true)
    .withSourceLevelDebuggingConfig(SourceLevelDebuggingConfig.enabled)
}

// this doesn't actually work but it makes me feel good about thinking
// that this could ever possibly work
nativeLinkReleaseFast / nativeConfig ~= {
  _.withLTO(if (Platform.os == Platform.OS.MacOS) LTO.full else LTO.thin)
}

scalacOptions ++= Seq(
  "-language:strictEquality",
  "-language:experimental.strictEqualityPatternMatching"
)

inThisBuild(
  List(
    organization := "com.indoorvivants",
    homepage     := Some(url("https://github.com/indoorvivants/sniper")),
    licenses     := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "indoorvivants",
        "Anton Sviridov",
        "contact@indoorvivants.com",
        url("https://blog.indoorvivants.com")
      )
    ),
    version := (if (!sys.env.contains("CI")) "dev" else version.value),
    crossScalaVersions := Nil
  )
)
