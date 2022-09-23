//> using lib "com.indoorvivants.detective::platform::0.0.2"

import com.indoorvivants.detective.Platform.*

@main def run(mode: String) = mode match
  case "release-mode" =>
    print(
      if sys.env.get("GITHUB_REF").exists(_.startsWith("refs/tags/v")) then
        "release-fast"
      else "debug"
    )
  case "build-suffix" =>
    print("-" + str(target))

def str(bits: Bits, arch: Arch): String = {
  (bits, arch) match {
    case (Bits.x64, Arch.Intel) => "x86_64"
    case (Bits.x64, Arch.Arm)   => "aarch_64"
    case (Bits.x32, Arch.Intel) => "x86_32"
    case (Bits.x32, Arch.Arm)   => "aarch_32"
  }
}

def str(os: OS): String = {
  import OS.*
  os match {
    case Windows => "pc-win32"
    case MacOS   => "apple-darwin"
    case Linux   => "pc-linux"
    case Unknown => "unknown"
  }
}

def str(target: Target): String = {
  str(target.bits, target.arch) + "-" + str(target.os)
}
