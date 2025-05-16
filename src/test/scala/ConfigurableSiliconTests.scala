package viper.silicon

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths => JPaths}
import scala.collection.immutable
import org.scalatest.ConfigMap
import scala.collection.Map
import viper.silicon.{Silicon, SiliconFrontend}
import viper.silver.ast.HasLineColumn
import viper.silver.reporter.NoopReporter
import viper.silver.testing.{ProjectInfo, SystemUnderTest, AnnotatedTestInput, AbstractOutput, SilOutput}
import viper.silver.utility.{Paths, TimingUtils}
import viper.silver.verifier.{AbstractError, AbstractVerificationError, Failure, Success, TimeoutOccurred, Verifier}
import viper.silver.testing.SilSuite
import scala.util.parsing.json.JSON


class ConfigurableSiliconTests extends SilSuite {

    /** Following a hyphenation-based naming scheme is important for handling project-specific annotations. */
    def name = "Silicon-Statistics"

    // The only system property - path to the config file
    private val configFilePathProperty = "SILICON_CONFIG_FILE"
    
    // Default configuration values - all in one place for easy overview
    private object Defaults {
        val repetitions: Int = 1
        val warmupLocation: Option[String] = None
        val targetLocation: Option[String] = None  // Required, no default
        val csvFile: Option[String] = None
        val inclusionFile: Option[String] = None
        val randomizeZ3: Boolean = false
        val timeout: Int = 600
        val siliconArguments: Seq[String] = Seq(
            "--disableCatchingExceptions",
            "--numberOfParallelVerifiers=1",
            "--assumeInjectivityOnInhale",
            "--proverSaturationTimeoutWeights=[1.0,0.5,0.0,0.0,0.02]",
            "--numberOfErrorsToReport=0",
            "--useOldAxiomatization"
        )
    }
    
    // Config structure parsed from JSON
    private lazy val configFilePath: Path = {
        val pathStr = Option(System.getProperty(configFilePathProperty))
            .getOrElse(throw new IllegalArgumentException(s"System property '$configFilePathProperty' must be set"))
        JPaths.get(pathStr).toAbsolutePath
    }
    private lazy val configDir: Path = configFilePath.getParent
    
    private lazy val config: Map[String, Any] = {
        val jsonString = new String(Files.readAllBytes(configFilePath))
        JSON.parseFull(jsonString) match {
            case Some(parsed: Map[_, _]) => 
                parsed.map { case (k, v) => (k.toString, v) }
            case _ => 
                throw new IllegalArgumentException(s"Invalid JSON configuration file: $configFilePath")
        }
    }

    // Helper to resolve paths relative to the config file directory
    private def resolvePathRelativeToConfig(pathStr: String): Path = {
        val path = JPaths.get(pathStr)
        if (path.isAbsolute) {
            path
        } else {
            configDir.resolve(path).normalize()
        }
    }

    // Centralized configuration access methods
    protected def repetitions: Int = {
        val reps = config.getOrElse("repetitions", Defaults.repetitions).toString.toDouble.toInt
        failIf(s"Repetitions must be >= 1, but got $reps", reps < 1)
        reps
    }

    protected def warmupDirName: Option[String] = {
        val dir = config.getOrElse("warmupLocation", "").toString.trim
        if (dir.isEmpty) None else Some(resolvePathRelativeToConfig(dir).toString)
    }

    protected def targetDirName: String = 
        config.get("targetLocation") match {
            case Some(name) => resolvePathRelativeToConfig(name.toString).toString
            case None => fail("'targetLocation' not specified in config file")
        }

    protected def csvFileName: Option[String] = getConfigStringOption("csvFile").map(resolvePathRelativeToConfig(_).toString)
    protected def inclusionFileName: Option[String] = getConfigStringOption("inclusionFile").map(resolvePathRelativeToConfig(_).toString)
    protected def randomizeZ3: Boolean = config.getOrElse("randomizeZ3", Defaults.randomizeZ3).toString.toBoolean
    protected def timeout: Int = config.getOrElse("timeout", Defaults.timeout).toString.toDouble.toInt

    private var csvFile: BufferedWriter = _
    private var testsToInclude: Option[Set[String]] = None

    // Silicon arguments constructed from config
    lazy val siliconArguments: Seq[String] = {
        // Get custom arguments from config, or use defaults
        val baseArgs = config.get("siliconArguments") match {
            case Some(args: List[_]) => args.map(_.toString)
            case _ => Defaults.siliconArguments
        }
        
        // Add timeout argument
        val timeoutArg = s"--timeout=$timeout"
        
        // Add randomization if configured
        val allArgs = baseArgs :+ timeoutArg
        if (randomizeZ3) allArgs :+ "--proverRandomizeSeeds" else allArgs
    }

    val randomization: Option[(Seq[String], String, Int => Int)] = {
        Some(siliconArguments, "--proverSpecificRandomSeed", i => i)
    }

    // Verifier configuration
    lazy val verifier: Silicon = {
        val args = siliconArguments ++
            Silicon.optionsFromScalaTestConfigMap(prefixSpecificConfigMap.getOrElse("silicon", Map()))
        val reporter = NoopReporter
        val debugInfo = ("startedBy" -> "viper.silicon.SiliconTests") :: Nil
        val silicon = Silicon.fromPartialCommandLineArguments(args, reporter, debugInfo)
        silicon
    }

    override def verifiers = Vector(verifier)

    // Test directory methods
    override def testDirectories: Seq[String] = warmupDirName match {
        case Some(w) => Vector(w, targetDirName)
        case None => Vector(targetDirName)
    }

    override def getTestDirPath(testDir: String): Path = {
        failIf(s"Test directory path cannot be null", testDir == null)
        
        // Use JPaths to properly handle "../" in paths
        val path = JPaths.get(testDir).toAbsolutePath().normalize()
        val targetFile = path.toFile
        
        // Log working directory and resolved path for debugging
        info(s"Working directory: ${new File(".").getAbsolutePath}")
        info(s"Resolved test directory: ${targetFile.getAbsolutePath}")
        
        // Check that the directory exists
        failIf(s"Test directory does not exist: ${targetFile.getAbsolutePath}", !targetFile.exists())
        failIf(s"Path is not a directory: ${targetFile.getAbsolutePath}", !targetFile.isDirectory)
        
        path
    }

    // Lifecycle methods
    override def beforeAll(configMap: ConfigMap): Unit = {
        super.beforeAll(configMap)
        csvFileName foreach { filename =>
            val csvPath = new File(filename).getAbsolutePath // Path is already resolved
            info(s"Writing results to CSV: $csvPath") // Print the CSV path
            // Ensure parent directory exists
            Option(new File(filename).getParentFile).foreach(_.mkdirs())
            csvFile = new BufferedWriter(new FileWriter(filename))
            csvFile.write("File,Outputs,Mean [ms],StdDev [ms],RelStdDev [%],Best [ms],Median [ms],Worst [ms], ResultsConsistent, Results")
            csvFile.newLine()
            csvFile.flush()
        }
        inclusionFileName foreach { filename =>
            val source = scala.io.Source.fromFile(filename) // Path is already resolved
            try {
                testsToInclude = Some(source.getLines().toSet)
            } finally {
                source.close()
            }
        }
    }

    override def afterAll(configMap: ConfigMap): Unit = {
        super.afterAll(configMap)
        if (csvFileName.isDefined) csvFile.close()
    }

    // System under test
    override def systemsUnderTest: Seq[SystemUnderTest] = Vector(testingInstance)

    private val testingInstance: SystemUnderTest with TimingUtils = new SystemUnderTest with TimingUtils {
        lazy val projectInfo: ProjectInfo = ConfigurableSiliconTests.this.projectInfo.update(name)

        override def run(input: AnnotatedTestInput): Seq[AbstractOutput] = {
            testsToInclude foreach { tests =>
                if (!tests.contains(input.name)) {
                    alert(s"Skipping ${input.name} (not included)")
                    return Seq.empty
                }
            }

            val phaseNames: Seq[String] = frontend(verifier, input.files).phases.map(_.name) :+ "Overall"
            val isWarmup = warmupDirName.isDefined && Paths.isInSubDirectory(Paths.canonize(warmupDirName.get), input.file.toFile)
            val reps = if (isWarmup) 1 else repetitions
            
            var foundTimeout = false
            var lastActualErrors: Seq[AbstractError] = null

            // collect data
            val data = for (r <- 1 to reps if !foundTimeout) yield {
                val fe = frontend(verifier, input.files)
                val perPhaseTimings = fe.phases.map(p => time(p.f)._2)
                val actualErrors: Seq[AbstractError] =
                    fe.result match {
                        case Success => Nil
                        case Failure(es) => es collect {
                            case te: TimeoutOccurred =>
                                foundTimeout = true; te
                            case e: AbstractVerificationError => e.transformedError()
                            case rest: AbstractError => rest
                        }
                    }
                if (lastActualErrors != null) {
                    if (!resultsConsistent(Seq(lastActualErrors, actualErrors))) {
                        foundTimeout = true
                    }
                }
                lastActualErrors = actualErrors
                (actualErrors, perPhaseTimings)
            }
            val (verResults: immutable.Seq[Seq[AbstractError]], timeResults: immutable.Seq[Seq[Long]]) = data.unzip
            val actualReps = verResults.length

            val timingsWithTotal = timeResults.toVector.map(row => row :+ row.sum)
            val sortedTimings = timingsWithTotal.sortBy(_.last)
            val (trimmedTimings, isTrimmed) = if (actualReps >= 4) {
                (sortedTimings.slice(1,actualReps-1), true)
            } else {
                (sortedTimings, false)
            }
            val meaningfulReps = trimmedTimings.length
            Predef.assert(trimmedTimings.nonEmpty)

            val meanTimings = trimmedTimings.transpose.map(col => (col.sum.toFloat / col.length).toLong)
            val bestRun = trimmedTimings.head
            val medianRun = trimmedTimings(meaningfulReps / 2)
            val worstRun = trimmedTimings.last

            val stddevTimings = trimmedTimings.transpose.zip(meanTimings).map { case (col, mean) =>
                math.sqrt(col.map(v => math.pow((v - mean).toDouble, 2)).sum / col.length).toLong
            }

            val relStddevTimings = stddevTimings.zip(meanTimings).map { case (stddev, mean) =>
                (100.0 * stddev / math.abs(mean)).toLong
            }

            if (isWarmup) {
                info(s"[JVM warmup] Time required: ${printableTimings(meanTimings, phaseNames)}.")
            } else if (meaningfulReps == 1) {
                info(s"[Benchmark] Time required: ${printableTimings(meanTimings, phaseNames)}.")
                if (csvFileName.isDefined) {
                    val csvRowData = Seq(
                        JPaths.get(targetDirName).toAbsolutePath.relativize(input.file.toAbsolutePath),
                        verResults.head.length,
                        meanTimings.last, stddevTimings.last, relStddevTimings.last,
                        bestRun.last, medianRun.last, worstRun.last, resultsConsistent(verResults), summarizeResults(verResults.head))
                    csvFile.write(csvRowData.mkString(","))
                    csvFile.newLine()
                    csvFile.flush()
                }
            } else {
                if (isTrimmed) {
                    info(s"[Benchmark] Trimmed ${sortedTimings.length - trimmedTimings.length} marginal runs.")
                }
                val n = "%2d".format(meaningfulReps)
                info(s"[Benchmark] Mean / $n runs: ${printableTimings(meanTimings, phaseNames)}.")
                info(s"[Benchmark] Stddevs:        ${printableTimings(stddevTimings, phaseNames)}.")
                info(s"[Benchmark] Rel. stddev:    ${printablePercentage(relStddevTimings, phaseNames)}.")
                info(s"[Benchmark] Best run:       ${printableTimings(bestRun, phaseNames)}.")
                info(s"[Benchmark] Median run:     ${printableTimings(medianRun, phaseNames)}.")
                info(s"[Benchmark] Worst run:      ${printableTimings(worstRun, phaseNames)}.")

                if (csvFileName.isDefined) {
                    val csvRowData = Seq(
                        JPaths.get(targetDirName).toAbsolutePath.relativize(input.file.toAbsolutePath),
                        verResults.head.length,
                        meanTimings.last, stddevTimings.last, relStddevTimings.last,
                        bestRun.last, medianRun.last, worstRun.last, resultsConsistent(verResults), summarizeResults(verResults.head))
                    csvFile.write(csvRowData.mkString(","))
                    csvFile.newLine()
                    csvFile.flush()
                }
            }

            verResults.flatten.map(SilOutput)
        }

        private def printableTimings(timings: Seq[Long], phaseNames: Seq[String]): String =
            timings.map(formatTimeForTable).zip(phaseNames).map(tup => tup._1 + " (" + tup._2 + ")").mkString(", ")

        private def printablePercentage(percentage: Seq[Long], phaseNames: Seq[String]): String = {
            percentage
                .map(p => "%6s %%   ".format(p))
                .zip(phaseNames)
                .map(tup => tup._1 + " (" + tup._2 + ")")
                .mkString(", ")
        }
    }

    private def resultsConsistent(results: Seq[Seq[AbstractError]]): Boolean = {
        val first = results.head
        results.tail.forall(_ == first)
    }

    private def summarizeResults(results: Seq[AbstractError]): String = {
        if (results.isEmpty) {
        "success"
        } else {
        results.map(e => {
            val posString = e.pos match {
            case lc: HasLineColumn => s"${lc.line}.${lc.column}-" 
            case _ => ""
            }
            s"${posString}${e.fullId}"
        }).sorted.mkString(";")
        }
    }

    private def summarizeStats(stats: Map[String, Long]): String = {
        stats.toSeq.sortBy(_._1).map(tup => s"${tup._1}=${tup._2}").mkString(";")
    }

    // Frontend implementation
    override def frontend(verifier: Verifier, files: Seq[Path]): SiliconFrontend = {
        require(files.length == 1, "tests should consist of exactly one file")
        val fe = new SiliconFrontend(NoopReporter)
        fe.init(verifier)
        fe.reset(files.head)
        fe
    }

    // Helper methods
    private def failIf(message: => String, condition: Boolean): Unit =
        if (condition) fail(message)

    private def getConfigStringOption(key: String): Option[String] = {
        val value = config.getOrElse(key, "").toString.trim
        if (value.isEmpty) None else Some(value)
    }
}