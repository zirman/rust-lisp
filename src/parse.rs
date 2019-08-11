use std::marker::PhantomData;

pub type Res<'a, A> = Result<(A, &'a str), &'a str>;

pub trait Parser<A>: Sized
where
    A: Clone,
{
    fn fmap<F, B>(self, f: F) -> ParserMap<Self, F, A, B>
    where
        F: Fn(A) -> B,
        B: Clone,
    {
        ParserMap {
            fa: self,
            f,
            phantom_a: PhantomData,
            phantom_b: PhantomData,
        }
    }

    fn bind<FB, F, B>(self, f: F) -> ParserBind<Self, FB, F, A, B>
    where
        FB: Parser<B>,
        F: Fn(A) -> FB,
        B: Clone,
    {
        ParserBind {
            fa: self,
            phantom_fb: PhantomData,
            f,
            phantom_a: PhantomData,
            phantom_b: PhantomData,
        }
    }

    fn prev<FB, B>(self, fb: FB) -> ParserPrev<Self, FB, A, B>
    where
        FB: Parser<B>,
        B: Clone,
    {
        ParserPrev {
            fa: self,
            fb,
            phantom_a: PhantomData,
            phantom_b: PhantomData,
        }
    }

    fn next<FB, B>(self, fb: FB) -> ParserNext<Self, FB, A, B>
    where
        FB: Parser<B>,
        B: Clone,
    {
        ParserNext {
            fa: self,
            fb,
            phantom_a: PhantomData,
            phantom_b: PhantomData,
        }
    }

    fn or<FA>(self, fa2: FA) -> ParserOr<Self, FA, A>
    where
        FA: Parser<A>,
    {
        ParserOr {
            fa1: self,
            fa2,
            phantom_a: PhantomData,
        }
    }

    fn parse<'a, 'b>(&'a self, source: &'b str) -> Res<'b, A>;
}

pub struct ParserMap<FA, F, A, B>
where
    FA: Parser<A>,
    F: Fn(A) -> B,
    A: Clone,
    B: Clone,
{
    fa: FA,
    f: F,
    phantom_a: PhantomData<A>,
    phantom_b: PhantomData<B>,
}

impl<FA, F, A, B> Parser<B> for ParserMap<FA, F, A, B>
where
    FA: Parser<A>,
    F: Fn(A) -> B,
    A: Clone,
    B: Clone,
{
    fn parse<'a, 'b>(&'a self, source: &'b str) -> Res<'b, B> {
        match self.fa.parse(source) {
            Ok((x, source1)) => Ok(((self.f)(x), source1)),
            Err(e) => Err(e),
        }
    }
}

pub struct ParserBind<FA, FB, F, A, B>
where
    FA: Parser<A>,
    FB: Parser<B>,
    F: Fn(A) -> FB,
    A: Clone,
    B: Clone,
{
    fa: FA,
    phantom_fb: PhantomData<FB>,
    f: F,
    phantom_a: PhantomData<A>,
    phantom_b: PhantomData<B>,
}

impl<FA, FB, F, A, B> Parser<B> for ParserBind<FA, FB, F, A, B>
where
    FA: Parser<A>,
    FB: Parser<B>,
    F: Fn(A) -> FB,
    A: Clone,
    B: Clone,
{
    fn parse<'a, 'b>(&'a self, source: &'b str) -> Res<'b, B> {
        match self.fa.parse(source) {
            Ok((x, source1)) => (self.f)(x).parse(source1),
            Err(e) => Err(e),
        }
    }
}

pub struct ParserPrev<FA, FB, A, B>
where
    FA: Parser<A>,
    FB: Parser<B>,
    A: Clone,
    B: Clone,
{
    fa: FA,
    fb: FB,
    phantom_a: PhantomData<A>,
    phantom_b: PhantomData<B>,
}

impl<FA, FB, A, B> Parser<A> for ParserPrev<FA, FB, A, B>
where
    FA: Parser<A>,
    FB: Parser<B>,
    A: Clone,
    B: Clone,
{
    fn parse<'a, 'b>(&'a self, source: &'b str) -> Res<'b, A> {
        match self.fa.parse(source) {
            Ok((x, source)) => match self.fb.parse(source) {
                Ok((_, source)) => Ok((x, source)),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        }
    }
}

pub struct ParserNext<FA, FB, A, B>
where
    FA: Parser<A>,
    FB: Parser<B>,
    A: Clone,
    B: Clone,
{
    fa: FA,
    fb: FB,
    phantom_a: PhantomData<A>,
    phantom_b: PhantomData<B>,
}

impl<FA, FB, A, B> Parser<B> for ParserNext<FA, FB, A, B>
where
    FA: Parser<A>,
    FB: Parser<B>,
    A: Clone,
    B: Clone,
{
    fn parse<'a, 'b>(&'a self, source: &'b str) -> Res<'b, B> {
        match self.fa.parse(source) {
            Ok((_, source)) => match self.fb.parse(source) {
                Ok((x, source)) => Ok((x, source)),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        }
    }
}

pub struct ParserOr<FA1, FA2, A>
where
    FA1: Parser<A>,
    FA2: Parser<A>,
    A: Clone,
{
    fa1: FA1,
    fa2: FA2,
    phantom_a: PhantomData<A>,
}

impl<FA1, FA2, A> Parser<A> for ParserOr<FA1, FA2, A>
where
    FA1: Parser<A>,
    FA2: Parser<A>,
    A: Clone,
{
    fn parse<'a, 'b>(&'a self, source: &'b str) -> Res<'b, A> {
        let res1 = self.fa1.parse(source);
        match res1 {
            Ok(_) => res1,
            Err(_) => self.fa2.parse(source),
        }
    }
}

pub struct ParserFn<F, A> {
    f: F,
    phantom_a: PhantomData<A>,
}

impl<F, A> ParserFn<F, A>
where
    F: Fn(&str) -> Res<A>,
    A: Clone,
{
    fn from(f: F) -> ParserFn<F, A> {
        ParserFn {
            f,
            phantom_a: PhantomData,
        }
    }
}

impl<F, A> Parser<A> for ParserFn<F, A>
where
    F: Fn(&str) -> Res<A>,
    A: Clone,
{
    fn parse<'a, 'b>(&'a self, source: &'b str) -> Res<'b, A> {
        (self.f)(source)
    }
}

pub fn pure<A: Clone>(x: A) -> impl Parser<A> {
    ParserFn::from(move |source| Ok((x.clone(), source)))
}

pub fn pchar(ch: char) -> impl Parser<char> {
    ParserFn::from(move |source| {
        let mut chars = source.chars();
        match chars.next() {
            Some(c) => {
                if c == ch {
                    Ok((c, chars.as_str()))
                } else {
                    Err(source)
                }
            }
            None => Err(source),
        }
    })
}

pub fn one_of(cs: &str) -> impl Parser<char> {
    let cs = cs.to_owned();
    ParserFn::from(move |source| {
        let mut chars = source.chars();
        match chars.next() {
            Some(c) => {
                if cs.contains(c) {
                    Ok((c, chars.as_str()))
                } else {
                    Err(source)
                }
            }
            None => Err(source),
        }
    })
}

pub fn predicate<F>(f: F) -> impl Parser<char>
where
    F: Fn(char) -> bool,
{
    ParserFn::from(move |source| {
        let mut chars = source.chars();
        match chars.next() {
            Some(c) => {
                if f(c) {
                    Ok((c, chars.as_str()))
                } else {
                    Err(source)
                }
            }
            None => Err(source),
        }
    })
}

pub fn none_of(cs: &str) -> impl Parser<char> {
    let cs = cs.to_owned();
    ParserFn::from(move |source| {
        let mut chars = source.chars();
        match chars.next() {
            Some(c) => {
                if !cs.contains(c) {
                    Ok((c, chars.as_str()))
                } else {
                    Err(source)
                }
            }
            None => Err(source),
        }
    })
}

pub fn many<PA, A>(pa: PA) -> impl Parser<Vec<A>>
where
    PA: Parser<A>,
    A: Clone,
{
    ParserFn::from(move |mut source| {
        let mut xs: Vec<A> = vec![];
        loop {
            match pa.parse(source) {
                Ok((x, source1)) => {
                    xs.push(x);
                    source = source1
                }
                Err(_) => return Ok((xs, source)),
            }
        }
    })
}

pub fn many1<PA, A>(pa: PA) -> impl Parser<Vec<A>>
where
    PA: Parser<A>,
    A: Clone,
{
    ParserFn::from(move |mut source| {
        let mut xs: Vec<A> = vec![];
        match pa.parse(source) {
            Ok((x, source1)) => {
                xs.push(x);
                source = source1
            }
            Err(e) => return Err(e),
        }
        loop {
            match pa.parse(source) {
                Ok((x, source1)) => {
                    xs.push(x);
                    source = source1
                }
                Err(_) => return Ok((xs, source)),
            }
        }
    })
}

pub fn skip_many1<PA, A>(pa: PA) -> impl Parser<()>
where
    PA: Parser<A>,
    A: Clone,
{
    ParserFn::from(move |mut source| {
        match pa.parse(source) {
            Ok((_, source1)) => source = source1,
            Err(e) => return Err(e),
        }
        loop {
            match pa.parse(source) {
                Ok((_, source1)) => source = source1,
                Err(_) => return Ok(((), source)),
            }
        }
    })
}

pub fn sep_by<PA, PS, A, S>(pa: PA, ps: PS) -> impl Parser<Vec<A>>
where
    PA: Parser<A>,
    PS: Parser<S>,
    A: Clone,
    S: Clone,
{
    ParserFn::from(move |mut source| {
        let mut xs: Vec<A> = vec![];
        match pa.parse(source) {
            Ok((x, source1)) => {
                xs.push(x);
                source = source1
            }
            Err(e) => return Err(e),
        }
        loop {
            match ps.parse(source) {
                Ok((_, source1)) => source = source1,
                Err(_) => return Ok((xs, source)),
            }
            match pa.parse(source) {
                Ok((x, source1)) => {
                    xs.push(x);
                    source = source1
                }
                Err(_) => return Ok((xs, source)),
            }
        }
    })
}

pub fn end_by<PA, PS, A, S>(pa: PA, ps: PS) -> impl Parser<Vec<A>>
where
    PA: Parser<A>,
    PS: Parser<S>,
    A: Clone,
    S: Clone,
{
    ParserFn::from(move |mut source| {
        let mut xs: Vec<A> = vec![];
        match pa.parse(source) {
            Ok((x, source1)) => {
                xs.push(x);
                source = source1
            }
            Err(e) => return Err(e),
        }
        match ps.parse(source) {
            Ok((_, source1)) => source = source1,
            Err(_) => return Ok((xs, source)),
        }
        loop {
            match pa.parse(source) {
                Ok((x, source1)) => {
                    xs.push(x);
                    source = source1
                }
                Err(_) => return Ok((xs, source)),
            }
            match ps.parse(source) {
                Ok((_, source1)) => source = source1,
                Err(_) => return Ok((xs, source)),
            }
        }
    })
}

//pub fn spaces() -> impl Parser<()> {
//    ParserFn::from(|source| {
//        let mut chars = source.chars();
//        match chars.next() {
//            Some(c) => {
//                if c.is_whitespace() {
//                    let mut peek_chars = chars.clone();
//                    while peek_chars.next().filter(|x| x.is_whitespace()).is_some() {
//                        chars = peek_chars.clone();
//                    }
//                    Ok(((), chars.as_str()))
//                } else {
//                    Err(source)
//                }
//            }
//            None => Err(source),
//        }
//    })
//}
