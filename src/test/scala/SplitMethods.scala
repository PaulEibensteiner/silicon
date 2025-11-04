import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

import viper.silver.ast.Program
import viper.silver.ast.pretty.FastPrettyPrinter
import viper.silver.ast.pretty.FastPrettyPrinter._
import tests.instantiateFrontend
import viper.silver.ast._

// A significant amount of code in this file was created by generative artificial intelligence.

/**
  * Behaviour:
  *   Recursively scans the input dir for .vpr files, translates each into a Program, then pretty prints
  *   each method together with its dependencies.
  *   For an input file   <inRoot>/sub/dir/Foo.vpr and method m, outputs to
  *     <outRoot>/sub/dir/Foo_m.vpr
  *   If multiple methods have the same (sanitized) name, adds a numeric suffix _<n>.
  *
  * Skips (cancels) the spec if the required system properties are not provided or the input dir doesn't exist.
  */
class SplitMethods extends AnyWordSpec with Matchers with Inside {
  def prettyMethodWithDependencies(program: Program, method: Method, width: Int = 75): String = {

    // Directly called methods (only from body; pre/post should not contain method calls)
    val calledMethodNames: Set[String] =
      method.body.toSeq.flatMap(_.deepCollect { case MethodCall(n, _, _) => n }).toSet - method.name

    // Collect initial function names from target method (pres, posts, body)
    def functionNamesInExprs(es: Seq[Exp]): Set[String] =
      es.flatMap(_.deepCollect { case FuncApp(n, _) => n }).toSet

    val initialFunctionNames: Set[String] =
      functionNamesInExprs(method.pres ++ method.posts) ++
        method.body.toSeq.flatMap(_.deepCollect { case FuncApp(n, _) => n })

    // Functions reachable via function bodies/contracts (transitive closure).
    def functionDeps(func: Function): Set[String] = {
      val inContracts = functionNamesInExprs(func.pres ++ func.posts)
      val inBody = func.body.toSeq.flatMap(_.deepCollect { case FuncApp(n, _) => n }).toSet
      inContracts ++ inBody - func.name
    }

    // Fixed-point closure over functions.
    def closure(todo: List[String], seen: Set[String]): Set[String] = todo match {
      case Nil => seen
      case f :: rest =>
        if (!program.functionsByName.contains(f) || seen(f)) closure(rest, seen)
        else {
          val fn = program.findFunction(f)
            // Add newly discovered function names.
          val next = functionDeps(fn)
          closure((next -- seen).toList ++ rest, seen + f)
        }
    }
    val allFunctionNames = closure(initialFunctionNames.toList, Set.empty)

    val neededFunctions: Seq[Function] =
      allFunctionNames.toSeq.sorted.map(program.findFunction)

    val neededMethodSignatures: Seq[Method] =
      calledMethodNames.toSeq.sorted.map { n =>
        val m = program.findMethod(n)
        m.copy(body = None)(m.pos, m.info, m.errT)
      }

    val docSeq =
      (neededFunctions.map(FastPrettyPrinter.showMember) ++
        neededMethodSignatures.map(FastPrettyPrinter.showMember) :+
        FastPrettyPrinter.showMember(method))

    val merged: FastPrettyPrinter.Cont =
      docSeq.reduceLeftOption((a, b) => a <@> FastPrettyPrinter.line <@> b).getOrElse(FastPrettyPrinter.nil)

    FastPrettyPrinter.pretty(width, merged)
  }

  private val InputProp  = "pretty.input.dir"
  private val OutputProp = "pretty.output.dir"

  private def prop(name: String): Option[String] = Option(System.getProperty(name)).filter(_.nonEmpty)

  private def sanitizeMethodName(name: String): String = {
    val sanitized = name.replaceAll("[^A-Za-z0-9_]+", "_").replaceAll("__+", "_")
    sanitized.stripPrefix("_").stripSuffix("_") match {
      case "" => "method"
      case other => other
    }
  }

  private def relativize(root: Path, file: Path): Path = root.relativize(file)

  private def writeFile(path: Path, content: String): Unit = {
    val parent = path.getParent
    if (parent != null && !Files.exists(parent)) Files.createDirectories(parent)
    Files.write(path, content.getBytes("UTF-8"), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }

  private def collectVprFiles(root: Path): Seq[Path] = {
    if (!Files.isDirectory(root)) Seq.empty
    else {
      val stream = Files.walk(root)
      try stream.iterator().asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".vpr")).toSeq
      finally stream.close()
    }
  }

  private def translate(frontend: viper.silver.frontend.SilFrontend, file: Path): Option[Program] = {
    try {
        frontend.reset(file)
        frontend.runTo(frontend.Translation)
        Some(frontend.translationResult)
    }
    catch {
        case NonFatal(t) => 
            alert(s"Failed to translate $file: ${t.getMessage}")
            None
    }
  }

  InputProp should {
    "pretty print each method of each .vpr file with dependencies" in {
      val maybeIn  = Some(prop(InputProp).getOrElse("../CAV24-data/benchmarks/examples_all"))
      val maybeOut = Some(prop(OutputProp).getOrElse("../CAV24-data/benchmarks/examples-split"))

      if (maybeIn.isEmpty || maybeOut.isEmpty) {
        cancel(s"System properties -D$InputProp and -D$OutputProp must be set to run this spec.")
      }

      val inRoot  = Paths.get(maybeIn.get)
      val outRoot = Paths.get(maybeOut.get)

      if (!Files.exists(inRoot) || !Files.isDirectory(inRoot)) {
        cancel(s"Input directory '$inRoot' does not exist or is not a directory")
      }

      val frontend = instantiateFrontend()

      val vprFiles = collectVprFiles(inRoot)
      info(s"Discovered ${vprFiles.size} .vpr files under $inRoot")

      var totalMethods = 0
      var writtenFiles = 0
      var failedTranslations = 0

      vprFiles.foreach { file =>
        val programOpt = try { translate(frontend, file) } catch { case NonFatal(e) => info(s"Failed to translate $file: ${e.getMessage}"); None }
        programOpt.foreach { program =>
          val rel = relativize(inRoot, file)
          val baseName = file.getFileName.toString.stripSuffix(".vpr")

            // Track used output filenames for this source file to avoid collisions after sanitization
          var usedNames = Set.empty[String]

          program.methods.foreach { m =>
            info(s"Pretty printing method ${m.name} in $file")
            totalMethods += 1
            val sanitized = sanitizeMethodName(m.name)
            val unique = Iterator.from(0).map { i => if (i == 0) sanitized else s"${sanitized}_$i" }.dropWhile(usedNames).next()
            usedNames += unique

            val pretty = prettyMethodWithDependencies(program, m)
            val outRel = rel.getParent match {
              case null => Paths.get(s"${baseName}_$unique.vpr")
              case parent => parent.resolve(s"${baseName}_$unique.vpr")
            }
            val outPath = outRoot.resolve(outRel)
            writeFile(outPath, pretty + System.lineSeparator())
            writtenFiles += 1
          }
        }
        if (programOpt.isEmpty) failedTranslations += 1
      }

      info(s"Pretty printed $totalMethods methods into $writtenFiles files under $outRoot (failed translations: $failedTranslations)")
    }
  }
}
