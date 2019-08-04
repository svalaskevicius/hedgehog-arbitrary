# Arbitrary generators for scala-hedgehog

## Purpose

Generates arbitrary values for a given type.

Currently, there is a limited subset of default type generators, as well as a Shapeless based derivation mechanism for ADTs.

Default generators are overrideable by providing an implicit `Arbitrary` instance in scope.

## Usage

1. prepare your project by importing in build.sbt:

```
resolvers += Resolver.url("bintray-scala-hedgehog-arbitrary", url("https://dl.bintray.com/svalaskevicius/scala-hedgehog-arbitrary"))(Resolver.ivyStylePatterns)
libraryDependencies += "com.svalaskevicius" %% "hedgehog-arbitrary" % "0.0.1" % Test
```

2. add arbitrary support in your test:

```
import hedgehog.arbitrary._
```

3. when using in your test, invoke it as:

```
Arbitrary[MyType].gen
```

## License

[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)
