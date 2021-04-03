macro_rules! bug {
    ($msg:expr) => ({
        bug!("{}", $msg)
    });
    ($fmt:expr, $($arg:tt)+) => ({
        error!(
            concat!("bug: ",
                    $fmt,
                    ". Please report this issue on GitHub if you find \
                    an input that triggers this case."),
            $($arg)*
        )
    });
}
