pub trait Bind<R, F, A, B>
where
    F: Fn(A) -> R,
{
    fn bind(self, f: F) -> R;
}

impl<F, A, B, E> Bind<Result<B, E>, F, A, B> for Result<A, E>
where
    F: Fn(A) -> Result<B, E>,
{
    fn bind(self, f: F) -> Result<B, E> {
        match self {
            Ok(x) => f(x),
            Err(e) => Err(e),
        }
    }
}

impl<F, A, B> Bind<Option<B>, F, A, B> for Option<A>
where
    F: Fn(A) -> Option<B>,
{
    fn bind(self, f: F) -> Option<B> {
        match self {
            Some(x) => f(x),
            None => None,
        }
    }
}

pub trait BindMut<R, F, A, B>
where
    F: FnMut(A) -> R,
{
    fn bind_mut(self, f: F) -> R;
}

impl<F, A, B, E> BindMut<Result<B, E>, F, A, B> for Result<A, E>
where
    F: FnMut(A) -> Result<B, E>,
{
    fn bind_mut(self, mut f: F) -> Result<B, E> {
        match self {
            Ok(x) => f(x),
            Err(e) => Err(e),
        }
    }
}

impl<F, A, B> BindMut<Option<B>, F, A, B> for Option<A>
where
    F: FnMut(A) -> Option<B>,
{
    fn bind_mut(self, mut f: F) -> Option<B> {
        match self {
            Some(x) => f(x),
            None => None,
        }
    }
}
