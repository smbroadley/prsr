// core.rs - core parsers and parser combinators
//
pub type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn box_map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn map<F, NewOutput>(self, map_fn: F) -> impl Parser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        map(self, map_fn)
    }

    fn pred<F>(self, pred_fn: F) -> impl Parser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        pred(self, pred_fn)
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> impl Parser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        and_then(self, f)
    }

    fn box_and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }

    fn or<NextParser>(self, parser: NextParser) -> impl Parser<'a, Output>
    where
        Self: Sized + 'a,
        NextParser: Parser<'a, Output> + 'a,
    {
        or(self, parser)
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

pub fn literal<'a>(expected: &'static str) -> impl Parser<'a, &'static str> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], expected)),
        _ => Err(input),
    }
}

impl<'a> Parser<'a, &'static str> for &'static str {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'static str> {
        literal(*self).parse(input)
    }
}

pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

pub fn inner<'a, P1, P2, P3, R1, R2, R3>(
    parser_left: P1,
    parser: P2,
    parser_right: P3,
) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
    P3: Parser<'a, R3>,
{
    right(parser_left, left(parser, parser_right))
}

pub fn outer<'a, P1, P2, P3, R1, R2, R3>(
    parser_left: P1,
    parser: P2,
    parser_right: P3,
) -> impl Parser<'a, (R1, R3)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
    P3: Parser<'a, R3>,
{
    pair(parser_left, right(parser, parser_right))
}

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

pub fn sep<'a, P1, P2, R1, R2>(parser: P1, separator: P2) -> impl Parser<'a, Vec<R1>>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |mut input| {
        let mut values = Vec::new();

        match parser.parse(input) {
            Ok((next_input, value)) => {
                input = next_input;
                values.push(value);
            }
            Err(err) => return Err(err),
        }

        loop {
            match separator.parse(input) {
                Ok((next_input, _)) => input = next_input,
                Err(_) => break,
            }

            match parser.parse(input) {
                Ok((next_input, value)) => {
                    input = next_input;
                    values.push(value);
                }
                Err(_) => break,
            }
        }

        Ok((input, values))
    }
}

#[test]
fn test_sep() {
    let expected = Ok(("", vec!['a', 'b', 'c']));
    assert_eq!(expected, sep(any_char, "|").parse("a|b|c"));
}

pub fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

pub fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

pub fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

pub fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

pub fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

pub fn or<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

pub fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

pub fn trim<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    inner(space0(), parser, space0())
}

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

// HELPERS

pub fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        literal("\""),
        left(zero_or_more(any_char.pred(|c| *c != '"')), literal("\"")),
    )
    .map(|chars| chars.into_iter().collect())
}

/// A parser combinator which chooses the first of the input parsers which matches
/// successfully.
///
/// All input parsers must have the same output type. This is a variadic form for [or].
///
/// # Examples
///
/// ```
/// use helix_parsec::{choice, or, Parser};
/// let parser = choice!("foo", "bar", "baz");
/// assert_eq!(Ok(("", "foo")), parser.parse("foo"));
/// assert_eq!(Ok(("", "bar")), parser.parse("bar"));
/// assert_eq!(Err("quiz"), parser.parse("quiz"));
/// ```
#[macro_export]
macro_rules! choice {
    ($parser: expr $(,)?) => {
        $parser
    };
    ($parser: expr, $($rest: expr),+ $(,)?) => {
        or($parser, choice!($($rest),+))
    }
}
