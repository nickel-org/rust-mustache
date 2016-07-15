#[cfg(feature = "test-serde")]
extern crate serde_codegen;

#[cfg(feature = "test-serde")]
pub fn main() {
    use std::env;
    use std::path::Path;

    let out_dir = env::var_os("OUT_DIR").unwrap();

    let src = Path::new("src/test_codegen.rs.in");
    let dst = Path::new(&out_dir).join("test_codegen.rs");

    serde_codegen::expand(&src, &dst).unwrap();
}

#[cfg(not(feature = "test-serde"))]
fn main() {}
