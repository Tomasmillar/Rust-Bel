use std::fmt;

/// Errors that can occur in the Bel interpreter at the Rust level.
#[derive(Debug, Clone)]
pub enum BelError {
    /// A Bel-level error signaled by sigerr (e.g. 'unbound, 'overargs).
    /// The String is a human-readable description; the attached value description
    /// is what sigerr was called with.
    BelErr(String),

    /// Attempted to call car/cdr on a non-nil atom.
    TypeError(String),

    /// Too many arguments to a primitive.
    OverArgs,

    /// Too few arguments (underargs in parameter matching).
    UnderArgs,

    /// Unbound variable.
    Unbound(String),

    /// Reader error.
    ReadError(String),

    /// Fuel (step counter) exhausted — likely an infinite loop.
    FuelExhausted,

    /// User pressed Ctrl+C.
    Interrupted,

    /// Heap capacity exceeded.
    HeapOverflow,

    /// I/O error from stream operations.
    IoError(String),

    /// Internal interpreter error (should not happen in correct code).
    Internal(String),
}

impl fmt::Display for BelError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BelError::BelErr(msg) => write!(f, "Error: {}", msg),
            BelError::TypeError(msg) => write!(f, "Type error: {}", msg),
            BelError::OverArgs => write!(f, "Error: too many arguments"),
            BelError::UnderArgs => write!(f, "Error: too few arguments"),
            BelError::Unbound(name) => write!(f, "Error: unbound variable '{}'", name),
            BelError::ReadError(msg) => write!(f, "Read error: {}", msg),
            BelError::FuelExhausted => write!(f, "Error: step limit exceeded (possible infinite loop)"),
            BelError::Interrupted => write!(f, "Interrupted"),
            BelError::HeapOverflow => write!(f, "Error: heap capacity exceeded"),
            BelError::IoError(msg) => write!(f, "I/O error: {}", msg),
            BelError::Internal(msg) => write!(f, "Internal error: {}", msg),
        }
    }
}

impl std::error::Error for BelError {}

pub type BelResult<T> = Result<T, BelError>;
