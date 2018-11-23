use std::fmt;

use std::sync::Mutex;
use string_interner::{StringInterner, Sym};

// We intern all strings using a static table.
// This table lives forever, but saves us from having to manage lifetimes.
lazy_static! {
    static ref CACHE: Mutex<StringInterner<Sym>> = Mutex::new(StringInterner::default());
}

// Thin wrapper around an interned string.
#[derive(Copy, Clone, Debug, PartialOrd, Ord, Eq, PartialEq, Hash)]
pub struct Interned(Sym);

impl ToString for Interned {
    /// panics if not a valid symbol
    #[inline]
    fn to_string(&self) -> String {
        // lookup in the table
        CACHE.lock().unwrap().resolve(self.0).unwrap().to_string()
    }
}

impl Interned {
    #[inline]
    pub fn new(s: &str) -> Interned {
        Interned(CACHE.lock().unwrap().get_or_intern(s))
    }
}

#[derive(Copy, Clone, PartialOrd, Ord, Eq, PartialEq, Hash)]
pub struct Name(Interned);

impl Name {
    pub fn new(s: &str) -> Name {
        Name(Interned::new(s))
    }

    pub fn fresh(prefix: &str) -> Name {
        let s = format!("{}'{}", prefix, CACHE.lock().unwrap().len());
        Name::new(&s)
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Name(\"{}\")", self.0.to_string())
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}
