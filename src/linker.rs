use std::{path::Path, process::Command};

use target_lexicon::{OperatingSystem, Triple};

fn is_apple(t: &Triple) -> bool {
    use OperatingSystem::*;
    matches!(t.operating_system, Darwin(_))
}

pub fn link(output: &Path, objs: &[&Path], triple: &Triple) {
    println!("OS: {}", triple.operating_system);
    println!("Arch: {}", triple.architecture);

    use OperatingSystem::*;
    let mut linker = match triple.operating_system {
        Darwin(_) => Command::new("ld"),
        os => todo!("Unhandled operating system {os}"),
    };
    linker.env_clear();

    linker.arg("-o").arg(output).args(objs);

    if is_apple(triple) {
        linker
            .arg("-platform_version")
            .arg("macos")
            .arg("10.7")
            .arg("10.7");

        linker
            .arg("-syslibroot")
            .arg("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk")
            .arg("-lSystem")
            .arg("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/17/lib/darwin/libclang_rt.osx.a");
    }

    println!(
        "{} {}",
        linker.get_program().display(),
        linker
            .get_args()
            .map(|s| format!("{} ", s.to_str().unwrap()))
            .collect::<String>()
    );
    assert!(linker.status().unwrap().success());
}
