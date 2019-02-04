extern crate symbolics_core;
use symbolics_core::Expr;
use symbolics_core::{s, apply};

trait Derivative {
    fn deriv1<S: Into<String>>(self, S) -> Self;
}

impl Derivative for Expr {
    fn deriv1<S: Into<String>>(self, wrt_raw: S) -> Expr {
        use Expr::*;
        let wrt = wrt_raw.into();
        match self {
            Num(_) => Num(0.),
            Symbol(s) => Num(if s == wrt {1.} else {0.}),
            Add(a, b) => a.clone().deriv1(wrt.as_ref()) + b.clone().deriv1(wrt.as_ref()),
            Mul(a, b) => a.clone().deriv1(wrt.as_ref())**b.clone() + *a.clone()*b.clone().deriv1(wrt.as_ref()),
            Pow(a, b) => (*a.clone()^*b.clone()) * (a.clone().ln()*b.clone().deriv1(wrt.as_ref()) + (*b.clone()*a.clone().deriv1(wrt.as_ref())/(*a.clone()))),
            Log(a, b) => a.clone().deriv1(wrt.as_ref()) / (*a.clone() * b.ln()),
            Sin(x) => x.clone().cos() * x.clone().deriv1(wrt.as_ref()),
            Cos(x) => -x.clone().sin() * x.clone().deriv1(wrt.as_ref()),
            Arcsin(x) => x.clone().deriv1(wrt) / (1 - (*x.clone() ^ 2)).sqrt(),
            Arccos(x) => -x.clone().deriv1(wrt) / (1 - (*x.clone() ^ 2)).sqrt(),
            Arctan(x) => x.clone().deriv1(wrt) / (1 + (*x.clone() ^ 2)),
        }.apply1("_", 0) // This is to auto-simplify
    }
}

#[macro_use]
mod macros {
/// Differentiate an expression with respect to any amount of variables.
///
/// # Examples
///
/// To take the derivative of `y` with respect to `x`:
/// diff!(y, x)
///
/// To take the second derivative of `x` with respect to `t`:
/// diff!(x, t, t)
///
/// To take the derivative of `(s!(x) ^ 2) * (s!(y) ^ 3)` with respect to `x`, then `y`:
/// diff!((s!(x) ^ 2) * (s!(y) ^ 3), x, y)
    #[macro_export]
    macro_rules! diff {
        ($expr:expr, $($sym:ident),+) => {
            $expr $(.deriv1(stringify!($sym)))+
        }
    }
}

#[test]
fn linear1() {
    let y = 3 * s!(x);
    assert_eq!(apply!(diff!(y, x), x=0).val().unwrap(), 3.);
}

#[test]
fn polynomial1() {
    let y = (s!(x)^3) + (s!(x)^2) + s!(x) + 2;
    assert_eq!(apply!(diff!(y, x), x=1).val().unwrap(), 6.);
}

#[test]
fn trig1() {
    use symbolics_core::consts::pi;
    let y = s!(x).sin() * s!(x).cos();
    assert_eq!(apply!(diff!(y, x), x=pi()/2).val().unwrap(), -1.);
}

#[test]
fn partial1() {
    let z = 2*s!(x) + s!(y);
    assert_eq!(apply!(diff!(z, x), y=5, x=0).val().unwrap(), 2.);
}
