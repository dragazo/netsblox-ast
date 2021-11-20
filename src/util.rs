use std::fmt::{self, Debug, Display};

pub struct Punctuated<'a, T: Iterator + Clone>(pub T, pub &'a str);
macro_rules! impl_puntuated {
    ($($req:ident => $fmt:literal),*$(,)?) => {$(
        impl<'a, T: Iterator + Clone> $req for Punctuated<'a, T> where <T as Iterator>::Item: $req {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let mut vals = self.0.clone();
                if let Some(first) = vals.next() {
                    write!(f, $fmt, first)?;
                    for rest in vals {
                        write!(f, concat!("{}", $fmt), self.1, rest)?;
                    }
                }
                Ok(())
            }
        }
    )*}
}
impl_puntuated! { Debug => "{:?}", Display => "{}" }

pub fn indent(code: &str) -> String {
    Punctuated(code.lines().map(|s| format!("    {}", s)), "\n").to_string()
}
