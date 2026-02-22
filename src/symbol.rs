use std::collections::HashMap;
use crate::value::SymbolId;

/// Interned symbol table. Each unique symbol name maps to a unique SymbolId.
/// `(id 'foo 'foo)` is true because both resolve to the same SymbolId.
pub struct SymbolTable {
    name_to_id: HashMap<String, SymbolId>,
    id_to_name: Vec<String>,
}

/// Well-known symbol IDs, pre-interned at startup.
/// These must match the order of interning in SymbolTable::new().
#[allow(non_upper_case_globals)]
pub mod sym {
    use crate::value::SymbolId;

    pub const NIL: SymbolId = SymbolId(0);
    pub const T: SymbolId = SymbolId(1);
    pub const O: SymbolId = SymbolId(2);
    pub const APPLY: SymbolId = SymbolId(3);
    pub const LIT: SymbolId = SymbolId(4);
    pub const PRIM: SymbolId = SymbolId(5);
    pub const CLO: SymbolId = SymbolId(6);
    pub const MAC: SymbolId = SymbolId(7);    // both type tag and operator
    pub const CONT: SymbolId = SymbolId(8);
    pub const NUM: SymbolId = SymbolId(9);
    pub const QUOTE: SymbolId = SymbolId(10);
    pub const IF: SymbolId = SymbolId(11);
    pub const WHERE: SymbolId = SymbolId(12);
    pub const DYN: SymbolId = SymbolId(13);
    pub const AFTER: SymbolId = SymbolId(14);
    pub const CCC: SymbolId = SymbolId(15);
    pub const THREAD: SymbolId = SymbolId(16);
    pub const SYMBOL: SymbolId = SymbolId(17);
    pub const PAIR: SymbolId = SymbolId(18);
    pub const CHAR: SymbolId = SymbolId(19);
    pub const STREAM: SymbolId = SymbolId(20);
    pub const SCOPE: SymbolId = SymbolId(21);
    pub const GLOBE: SymbolId = SymbolId(22);
    pub const INS: SymbolId = SymbolId(23);
    pub const OUTS: SymbolId = SymbolId(24);
    pub const ERR: SymbolId = SymbolId(25);
    pub const LOCK: SymbolId = SymbolId(26);
    pub const A: SymbolId = SymbolId(27);
    pub const D: SymbolId = SymbolId(28);
    pub const IN: SymbolId = SymbolId(29);
    pub const OUT: SymbolId = SymbolId(30);
    pub const CLOSED: SymbolId = SymbolId(31);
    pub const EOF: SymbolId = SymbolId(32);
    pub const COMPOSE: SymbolId = SymbolId(33);
    pub const NO: SymbolId = SymbolId(34);
    pub const FN: SymbolId = SymbolId(35);
    pub const SET: SymbolId = SymbolId(36);
    pub const DEF: SymbolId = SymbolId(37);
    pub const BQUOTE: SymbolId = SymbolId(38);
    pub const COMMA: SymbolId = SymbolId(39);
    pub const COMMA_AT: SymbolId = SymbolId(40);
    pub const SPLICE: SymbolId = SymbolId(41);
    pub const FUT: SymbolId = SymbolId(42);
    pub const BIND: SymbolId = SymbolId(43);
    pub const LOC: SymbolId = SymbolId(44);
    pub const PROT: SymbolId = SymbolId(45);
    pub const DO: SymbolId = SymbolId(46);
    pub const PLUS: SymbolId = SymbolId(47);
    pub const MINUS: SymbolId = SymbolId(48);
    pub const UNBOUND: SymbolId = SymbolId(49);
    pub const OVERARGS: SymbolId = SymbolId(50);
    pub const UNDERARGS: SymbolId = SymbolId(51);
    pub const MALFORMED: SymbolId = SymbolId(52);
    pub const CANNOT_APPLY: SymbolId = SymbolId(53);
    pub const BAD_LIT: SymbolId = SymbolId(54);
    pub const BAD_CLO: SymbolId = SymbolId(55);
    pub const BAD_CONT: SymbolId = SymbolId(56);
    pub const BAD_FORM: SymbolId = SymbolId(57);
    pub const MISTYPE: SymbolId = SymbolId(58);
    pub const LITERAL_PARM: SymbolId = SymbolId(59);
    pub const ATOM_ARG: SymbolId = SymbolId(60);
    pub const CANNOT_BIND: SymbolId = SymbolId(61);
    pub const UNFINDABLE: SymbolId = SymbolId(62);
    pub const UNKNOWN_MARK: SymbolId = SymbolId(63);
    pub const UNKNOWN_PRIM: SymbolId = SymbolId(64);
    pub const UNAPPLYABLE: SymbolId = SymbolId(65);
    pub const NO_ERR: SymbolId = SymbolId(66);
    pub const WRONG_NO_ARGS: SymbolId = SymbolId(67);
    pub const UPON: SymbolId = SymbolId(68);
    pub const LIST: SymbolId = SymbolId(69);
    pub const CONS: SymbolId = SymbolId(70);
    pub const APPEND: SymbolId = SymbolId(71);
    pub const ARR: SymbolId = SymbolId(72);
    pub const TAB: SymbolId = SymbolId(73);
    pub const MACRO: SymbolId = SymbolId(74);
    pub const UNDERSCORE: SymbolId = SymbolId(75);
    // Primitive names
    pub const ID: SymbolId = SymbolId(76);
    pub const JOIN: SymbolId = SymbolId(77);
    pub const CAR: SymbolId = SymbolId(78);
    pub const CDR: SymbolId = SymbolId(79);
    pub const TYPE: SymbolId = SymbolId(80);
    pub const XAR: SymbolId = SymbolId(81);
    pub const XDR: SymbolId = SymbolId(82);
    pub const SYM: SymbolId = SymbolId(83);
    pub const NOM: SymbolId = SymbolId(84);
    pub const WRB: SymbolId = SymbolId(85);
    pub const RDB: SymbolId = SymbolId(86);
    pub const OPS: SymbolId = SymbolId(87);
    pub const CLS: SymbolId = SymbolId(88);
    pub const STAT: SymbolId = SymbolId(89);
    pub const COIN: SymbolId = SymbolId(90);
    pub const SYS: SymbolId = SymbolId(91);
    pub const LET: SymbolId = SymbolId(92);
    pub const UNBOUNDB: SymbolId = SymbolId(93);
    pub const CHARS: SymbolId = SymbolId(94);
    pub const TIMES: SymbolId = SymbolId(95);
    pub const SLASH: SymbolId = SymbolId(96);
    pub const LESS: SymbolId = SymbolId(97);
    pub const GREATER: SymbolId = SymbolId(98);
}

impl SymbolTable {
    /// Create a new symbol table with all well-known symbols pre-interned.
    /// The order MUST match the constants in the `sym` module above.
    pub fn new() -> Self {
        let names = vec![
            "nil", "t", "o", "apply", "lit", "prim", "clo", "mac", "cont", "num",
            "quote", "if", "where", "dyn", "after", "ccc", "thread",
            "symbol", "pair", "char", "stream",
            "scope", "globe", "ins", "outs", "err", "lock",
            "a", "d", "in", "out", "closed", "eof",
            "compose", "no", "fn", "set", "def", "bquote", "comma", "comma-at", "splice",
            "fut", "bind", "loc", "prot",
            "do", "+", "-",
            "unbound", "overargs", "underargs", "malformed", "cannot-apply",
            "bad-lit", "bad-clo", "bad-cont", "bad-form", "mistype",
            "literal-parm", "atom-arg", "cannot-bind", "unfindable", "unknown-mark",
            "unknown-prim", "unapplyable", "no-err", "wrong-no-args",
            "upon", "list", "cons", "append", "arr", "tab", "macro", "_",
            "id", "join", "car", "cdr", "type", "xar", "xdr", "sym", "nom",
            "wrb", "rdb", "ops", "cls", "stat", "coin", "sys",
            "let", "unboundb", "chars",
            "*", "/", "<", ">",
        ];

        let mut name_to_id = HashMap::new();
        let mut id_to_name = Vec::new();

        for (i, name) in names.iter().enumerate() {
            let id = SymbolId(i as u32);
            name_to_id.insert(name.to_string(), id);
            id_to_name.push(name.to_string());
        }

        SymbolTable {
            name_to_id,
            id_to_name,
        }
    }

    /// Intern a symbol name. Returns the existing ID if already interned,
    /// or creates a new one.
    pub fn intern(&mut self, name: &str) -> SymbolId {
        if let Some(&id) = self.name_to_id.get(name) {
            return id;
        }
        let id = SymbolId(self.id_to_name.len() as u32);
        self.name_to_id.insert(name.to_string(), id);
        self.id_to_name.push(name.to_string());
        id
    }

    /// Look up a symbol name by its ID.
    pub fn name(&self, id: SymbolId) -> &str {
        &self.id_to_name[id.0 as usize]
    }

    /// Look up a symbol ID by name, without interning.
    pub fn lookup(&self, name: &str) -> Option<SymbolId> {
        self.name_to_id.get(name).copied()
    }

    /// Total number of interned symbols.
    pub fn count(&self) -> usize {
        self.id_to_name.len()
    }
}
