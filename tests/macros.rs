#[cfg(test)]
macro_rules! assert_let {
    (@as_block $block:block) => { $block };
    ($pattern:pat = $thing:expr) => {
        assert_let!($pattern = $thing => {});
    };
    ($pattern:pat = $thing:expr => $($block:tt)*) => {
        match $thing {
            $pattern => assert_let!(@as_block { $($block)* }),
            ref failure => {
                panic!("assertion failed: expected `{:?}`, found `{:?}`", stringify!($pattern), failure)
            }
        }
    };
}
