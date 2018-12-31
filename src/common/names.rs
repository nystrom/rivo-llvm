use std::fmt;
use serde::Serialize;
use serde::Deserialize;
use serde::Serializer;
use serde::Deserializer;
use serde::de;

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
    // panics if not a valid symbol
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

impl Serialize for Interned {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Interned {
    fn deserialize<D>(deserializer: D) -> Result<Interned, D::Error>
        where D: Deserializer<'de>
    {
        struct InternedVisitor;

        impl<'de> de::Visitor<'de> for InternedVisitor {
            type Value = Interned;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string")
            }

            fn visit_str<E>(self, value: &str) -> Result<Interned, E>
                where E: de::Error,
            {
                let x = Interned::new(value);
                Ok(x)
            }
        }

        deserializer.deserialize_str(InternedVisitor)
    }
}

#[derive(Copy, Clone, PartialOrd, Ord, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Name(Interned);

impl Name {
    pub fn new(s: &str) -> Name {
        Name(Interned::new(s))
    }

    pub fn fresh(prefix: &str) -> Name {
        let s = format!("{}.{}", prefix, CACHE.lock().unwrap().len());
        Name::new(&s)
    }

    pub fn hashit(&self) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut s = DefaultHasher::new();
        // hash to_string because hashing the interned string can produce different values in
        // different runs of the program depending on the order of interning
        self.to_string().hash(&mut s);   
        s.finish()
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

pub struct FreshNameGenerator {
    prefix: String,
    next: usize,
}

impl FreshNameGenerator {
    pub fn new(prefix: &str) -> FreshNameGenerator {
        FreshNameGenerator {
            prefix: String::from(prefix),
            next: 0
        }
    }

    pub fn fresh(&mut self, prefix: &str) -> Name {
        let x = Name::new(&format!("{}.{}.{}", prefix, self.prefix, self.next));
        self.next += 1;
        x
    }
}
