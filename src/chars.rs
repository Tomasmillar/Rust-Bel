use crate::error::BelResult;
use crate::heap::Heap;
use crate::value::{BelValue, CharId};

/// Named character constants.
pub const CHAR_BEL: CharId = CharId(7);
pub const CHAR_TAB: CharId = CharId(9);
pub const CHAR_LF: CharId = CharId(10);
pub const CHAR_CR: CharId = CharId(13);
pub const CHAR_SP: CharId = CharId(32);

/// Character table: ASCII 128 characters with 7-bit binary representations.
pub struct CharTable {
    /// The Bel `chars` global: a proper list of (char . binary-string) pairs.
    /// binary-string is a proper list of \1 and \0 characters.
    pub chars_list: BelValue,
}

impl CharTable {
    /// Build the character table. Creates the `chars` global list on the heap.
    /// Each entry is (char . (b6 b5 b4 b3 b2 b1 b0)) where bi is \1 or \0.
    pub fn new(heap: &mut Heap) -> BelResult<Self> {
        let char_1 = BelValue::Char(CharId(b'1'));
        let char_0 = BelValue::Char(CharId(b'0'));

        // Build chars list from char 127 down to 0 (so list is in order 0..127)
        let mut chars_list = BelValue::Nil;

        for i in (0u8..128).rev() {
            // Build the 7-bit binary representation as a list of \1 and \0
            let mut bits = BelValue::Nil;
            for bit_pos in 0..7 {
                let bit = if (i >> bit_pos) & 1 == 1 { char_1 } else { char_0 };
                let pair = heap.alloc(bit, bits)?;
                bits = BelValue::Pair(pair);
            }

            // Create the (char . bits) pair
            let char_val = BelValue::Char(CharId(i));
            let entry = heap.alloc(char_val, bits)?;

            // Cons onto the chars list
            let node = heap.alloc(BelValue::Pair(entry), chars_list)?;
            chars_list = BelValue::Pair(node);
        }

        Ok(CharTable { chars_list })
    }

    /// Convert an ASCII byte to a CharId.
    pub fn from_ascii(byte: u8) -> Option<CharId> {
        if byte < 128 {
            Some(CharId(byte))
        } else {
            None
        }
    }

    /// Convert a CharId to an ASCII byte.
    pub fn to_ascii(id: CharId) -> u8 {
        id.0
    }

    /// Convert a CharId to a Rust char.
    pub fn to_char(id: CharId) -> char {
        id.0 as char
    }

    /// Get the named character for a name like "bel", "tab", "lf", "cr", "sp".
    pub fn named_char(name: &str) -> Option<CharId> {
        match name {
            "bel" => Some(CHAR_BEL),
            "tab" => Some(CHAR_TAB),
            "lf" => Some(CHAR_LF),
            "cr" => Some(CHAR_CR),
            "sp" => Some(CHAR_SP),
            _ => None,
        }
    }

    /// Get the name for a named character, if it has one.
    pub fn char_name(id: CharId) -> Option<&'static str> {
        match id.0 {
            7 => Some("bel"),
            9 => Some("tab"),
            10 => Some("lf"),
            13 => Some("cr"),
            32 => Some("sp"),
            _ => None,
        }
    }

    /// Returns true if the character is printable (has a single-character representation).
    pub fn is_printable(id: CharId) -> bool {
        id.0 > 32 && id.0 < 127
    }
}
