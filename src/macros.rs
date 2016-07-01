/// This should be the only place to panic inside non-test code.
// TODO: ensure no panics elsewhere via clippy
macro_rules! bug {
    ($msg:expr) => ({
        bug!("{}", $msg)
    });
    ($fmt:expr, $($arg:tt)+) => ({
        panic!(
            concat!("bug: ",
                    $fmt,
                    ". Please report this issue on GitHub if you find \
                    an input that triggers this case."),
            $($arg)*
        )
    });
}
