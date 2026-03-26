use std::ops::Rem;

/// A Matcher is a single rule of fizzbuzz: given a function on T, should
/// a word be substituted in? If yes, which word?
pub struct Matcher<T> {
    condition: Box<dyn Fn(T) -> bool>,
    subs: String,
}

impl<T> Matcher<T> {
    pub fn new<F, S>(condition: F, subs: S) -> Matcher<T>
    where
        F: Fn(T) -> bool + 'static,
        String: From<S>,
    {
        Self {
            condition: Box::new(condition),
            subs: String::from(subs),
        }
    }
}

/// A Fizzy is a set of matchers, which may be applied to an iterator.
pub struct Fizzy<T> {
    matchers: Vec<Matcher<T>>,
}

impl<T: Copy + ToString> Fizzy<T> {
    pub fn new() -> Self {
        Self { matchers: vec![] }
    }

    #[must_use]
    pub fn add_matcher(mut self, matcher: Matcher<T>) -> Self {
        self.matchers.push(matcher);
        self
    }

    /// map this fizzy onto every element of an iterator,
    /// returning a new iterator
    pub fn apply<I: Iterator<Item = T>>(
        self,
        iter: I,
    ) -> impl Iterator<Item = String> {
        iter.map(move |x| {
            let mut r = String::new();
            for matcher in &self.matchers {
                if (matcher.condition)(x) {
                    r += &matcher.subs
                }
            }
            if r.is_empty() {
                r = x.to_string()
            }
            r
        })
    }
}

/// convenience function: return a Fizzy which applies the standard fizz-buzz
/// rules
pub fn fizz_buzz<T>() -> Fizzy<T>
where
    T: Copy + From<u8> + PartialEq + Rem<Output = T> + ToString,
{
    Fizzy::new()
        .add_matcher(Matcher::new(|n: T| n.rem(3.into()) == 0.into(), "fizz"))
        .add_matcher(Matcher::new(|n: T| n.rem(5.into()) == 0.into(), "buzz"))
}
