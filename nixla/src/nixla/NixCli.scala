package nixla

import scala.sys.process.*

/** Stage 4 of the meta compiler (DESIGN.md): the real Nix CLI as the final
  * oracle. Never invoked at Scala compile time — compilation stays hermetic;
  * this is a runtime / test-time API. The dev-shell flake guarantees the
  * `nix` binary is present there.
  */
object NixCli:

  private def run(cmd: Seq[String]): Either[String, String] =
    val out    = new StringBuilder
    val err    = new StringBuilder
    val logger = ProcessLogger(o => { out.append(o); out.append('\n') }, e => { err.append(e); err.append('\n') })
    try
      val code = Process(cmd).!(logger)
      if code == 0 then Right(out.toString)
      else Left(err.toString.trim)
    catch case e: java.io.IOException => Left(s"nix CLI not available: ${e.getMessage}")

  lazy val available: Boolean =
    run(Seq("nix-instantiate", "--version")).isRight

  /** syntax gate: does the real Nix parser accept this source? */
  def parseCheck(src: String): Either[String, Unit] =
    run(Seq("nix-instantiate", "--parse", "-E", src)).map(_ => ())

  /** semantic gate for pure expressions: strict evaluation */
  def evalCheck(src: String): Either[String, Unit] =
    run(Seq("nix-instantiate", "--eval", "--strict", "-E", src)).map(_ => ())

  /** strict evaluation returning nix's printed result */
  def eval(src: String): Either[String, String] =
    run(Seq("nix-instantiate", "--eval", "--strict", "-E", src)).map(_.trim)

extension [T <: NAny](n: Nix[T])
  /** verify the canonical render against the real Nix parser */
  def check(): Either[String, Unit] = NixCli.parseCheck(n.render)
