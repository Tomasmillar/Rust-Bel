use std::fmt;

/// Unique identifier for an interned symbol.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

/// Unique identifier for a character (index into chars table, 0..127).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CharId(pub u8);

/// Unique identifier for an open stream.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StreamId(pub u32);

/// Index into the cons-cell heap. This is the GC handle.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PairId(pub u32);

/// The fundamental Bel value. 8 bytes: discriminant + payload.
/// Copy semantics — the actual pair data lives in the heap.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BelValue {
    Nil,
    Symbol(SymbolId),
    Char(CharId),
    Stream(StreamId),
    Pair(PairId),
}

impl BelValue {
    pub fn is_nil(self) -> bool {
        matches!(self, BelValue::Nil)
    }

    pub fn is_pair(self) -> bool {
        matches!(self, BelValue::Pair(_))
    }

    pub fn is_symbol(self) -> bool {
        matches!(self, BelValue::Symbol(_))
    }

    pub fn is_char(self) -> bool {
        matches!(self, BelValue::Char(_))
    }

    pub fn is_stream(self) -> bool {
        matches!(self, BelValue::Stream(_))
    }

    pub fn as_pair(self) -> Option<PairId> {
        match self {
            BelValue::Pair(id) => Some(id),
            _ => None,
        }
    }

    pub fn as_symbol(self) -> Option<SymbolId> {
        match self {
            BelValue::Symbol(id) => Some(id),
            _ => None,
        }
    }

    pub fn as_char(self) -> Option<CharId> {
        match self {
            BelValue::Char(id) => Some(id),
            _ => None,
        }
    }

    pub fn as_stream(self) -> Option<StreamId> {
        match self {
            BelValue::Stream(id) => Some(id),
            _ => None,
        }
    }

    /// Returns true if this value is an atom (not a pair).
    pub fn is_atom(self) -> bool {
        !self.is_pair()
    }
}

impl fmt::Debug for BelValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BelValue::Nil => write!(f, "Nil"),
            BelValue::Symbol(id) => write!(f, "Sym({})", id.0),
            BelValue::Char(id) => write!(f, "Char({})", id.0),
            BelValue::Stream(id) => write!(f, "Stream({})", id.0),
            BelValue::Pair(id) => write!(f, "Pair({})", id.0),
        }
    }
}

impl fmt::Debug for SymbolId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SymbolId({})", self.0)
    }
}

impl fmt::Debug for CharId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CharId({})", self.0)
    }
}

impl fmt::Debug for StreamId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StreamId({})", self.0)
    }
}

impl fmt::Debug for PairId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PairId({})", self.0)
    }
}
