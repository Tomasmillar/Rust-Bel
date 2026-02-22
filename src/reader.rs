use crate::chars::CharTable;
use crate::error::{BelError, BelResult};
use crate::heap::Heap;
use crate::symbol::{sym, SymbolTable};
use crate::value::{BelValue, CharId};

/// Bootstrap reader: parses Bel source text into BelValue structures.
pub struct Reader<'a> {
    input: &'a [u8],
    pos: usize,
    heap: &'a mut Heap,
    symbols: &'a mut SymbolTable,
}

impl<'a> Reader<'a> {
    pub fn new(input: &'a str, heap: &'a mut Heap, symbols: &'a mut SymbolTable) -> Self {
        Reader {
            input: input.as_bytes(),
            pos: 0,
            heap,
            symbols,
        }
    }

    /// Read one expression. Returns None at EOF.
    pub fn read(&mut self) -> BelResult<Option<BelValue>> {
        self.skip_whitespace_and_comments();
        if self.pos >= self.input.len() {
            return Ok(None);
        }
        let val = self.read_expr()?;
        Ok(Some(val))
    }

    /// Return current position in input.
    pub fn position(&self) -> usize {
        self.pos
    }

    /// Read all expressions from input.
    pub fn read_all(&mut self) -> BelResult<Vec<BelValue>> {
        let mut results = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.input.len() {
                break;
            }
            results.push(self.read_expr()?);
        }
        Ok(results)
    }

    fn peek(&self) -> Option<u8> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<u8> {
        if self.pos < self.input.len() {
            let ch = self.input[self.pos];
            self.pos += 1;
            Some(ch)
        } else {
            None
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // Skip whitespace
            while self.pos < self.input.len() {
                let ch = self.input[self.pos];
                if ch == b' ' || ch == b'\t' || ch == b'\n' || ch == b'\r' {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            // Skip line comments
            if self.pos < self.input.len() && self.input[self.pos] == b';' {
                while self.pos < self.input.len() && self.input[self.pos] != b'\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn read_expr(&mut self) -> BelResult<BelValue> {
        self.skip_whitespace_and_comments();

        let ch = self.peek().ok_or_else(|| BelError::ReadError("unexpected EOF".into()))?;

        match ch {
            b'(' => self.read_list(),
            b')' => Err(BelError::ReadError("unexpected ')'".into())),
            b'[' => self.read_bracket(),
            b']' => Err(BelError::ReadError("unexpected ']'".into())),
            b'\'' => self.read_quote(),
            b'`' => self.read_backquote(),
            b',' => self.read_comma(),
            b'"' => self.read_string(),
            b'\\' => self.read_char(),
            b'|' => self.read_escaped_symbol(),
            b'#' => self.read_shared(),
            _ => self.read_word(),
        }
    }

    /// Read a list: (a b c) or (a . b) or (a b . c)
    fn read_list(&mut self) -> BelResult<BelValue> {
        self.advance(); // consume '('
        self.skip_whitespace_and_comments();

        if self.peek() == Some(b')') {
            self.advance();
            return Ok(BelValue::Nil);
        }

        let mut elements = Vec::new();
        let mut dot_tail = None;

        loop {
            self.skip_whitespace_and_comments();

            if self.peek() == Some(b')') {
                self.advance();
                break;
            }

            if self.pos >= self.input.len() {
                return Err(BelError::ReadError("unterminated list".into()));
            }

            // Check for dot notation
            if self.peek() == Some(b'.') && self.is_dot_separator() {
                self.advance(); // consume '.'
                self.skip_whitespace_and_comments();
                dot_tail = Some(self.read_expr()?);
                self.skip_whitespace_and_comments();
                if self.peek() != Some(b')') {
                    return Err(BelError::ReadError("expected ')' after dot tail".into()));
                }
                self.advance();
                break;
            }

            elements.push(self.read_expr()?);
        }

        // Build the list
        let mut result = dot_tail.unwrap_or(BelValue::Nil);
        for val in elements.into_iter().rev() {
            let pair = self.heap.alloc(val, result)?;
            result = BelValue::Pair(pair);
        }
        Ok(result)
    }

    /// Check if a '.' is a dot separator (not part of a word).
    fn is_dot_separator(&self) -> bool {
        // Dot is a separator if followed by whitespace, ')', or EOF
        // Or if preceded by whitespace or '('
        let next_pos = self.pos + 1;
        if next_pos >= self.input.len() {
            return true;
        }
        let next = self.input[next_pos];
        next == b' ' || next == b'\t' || next == b'\n' || next == b'\r'
            || next == b')' || next == b']'
    }

    /// Read bracket notation: [f _] -> (fn (_) (f _))
    fn read_bracket(&mut self) -> BelResult<BelValue> {
        self.advance(); // consume '['
        self.skip_whitespace_and_comments();

        let mut elements = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.peek() == Some(b']') {
                self.advance();
                break;
            }
            if self.pos >= self.input.len() {
                return Err(BelError::ReadError("unterminated bracket".into()));
            }
            elements.push(self.read_expr()?);
        }

        // Build the body as a list
        let body = self.heap.list(&elements)?;

        // Wrap as (fn (_) body)
        let underscore = BelValue::Symbol(sym::UNDERSCORE);
        let fn_sym = BelValue::Symbol(sym::FN);
        // (fn (_) body) = (fn . ((_) . (body . nil)))
        let body_list = self.heap.alloc(body, BelValue::Nil)?;
        let parms = self.heap.alloc(underscore, BelValue::Nil)?;
        let parms_and_body = self.heap.alloc(BelValue::Pair(parms), BelValue::Pair(body_list))?;
        let fn_form = self.heap.alloc(fn_sym, BelValue::Pair(parms_and_body))?;
        Ok(BelValue::Pair(fn_form))
    }

    /// Read quote: 'expr -> (quote expr)
    fn read_quote(&mut self) -> BelResult<BelValue> {
        self.advance(); // consume '\''
        let expr = self.read_expr()?;
        let quote_sym = BelValue::Symbol(sym::QUOTE);
        let inner = self.heap.alloc(expr, BelValue::Nil)?;
        let outer = self.heap.alloc(quote_sym, BelValue::Pair(inner))?;
        Ok(BelValue::Pair(outer))
    }

    /// Read backquote: `expr -> expand at read time
    fn read_backquote(&mut self) -> BelResult<BelValue> {
        self.advance(); // consume '`'
        let expr = self.read_expr()?;
        self.expand_backquote(expr)
    }

    /// Expand backquote at read time.
    fn expand_backquote(&mut self, expr: BelValue) -> BelResult<BelValue> {
        match expr {
            BelValue::Pair(id) => {
                let car = self.heap.car(id);

                // Check if expr itself is (comma x) — i.e., car is the SYMBOL comma
                if car == BelValue::Symbol(sym::COMMA) {
                    // ,x -> x (unquote)
                    let cdr = self.heap.cdr(id);
                    return self.heap.car_val(cdr);
                }

                // Check if expr itself is (comma-at x) or (splice x)
                if car == BelValue::Symbol(sym::COMMA_AT) || car == BelValue::Symbol(sym::SPLICE) {
                    return Err(BelError::ReadError("splice outside list".into()));
                }

                // It's a list — expand each element
                self.expand_backquote_list(expr)
            }
            _ => {
                // Atom: quote it
                self.make_quoted(expr)
            }
        }
    }

    /// Expand a backquoted list, handling , and ,@ inside.
    fn expand_backquote_list(&mut self, list: BelValue) -> BelResult<BelValue> {
        // Build up using cons/append calls
        let mut segments: Vec<BelValue> = Vec::new();
        let mut current = list;

        while let BelValue::Pair(id) = current {
            let car = self.heap.car(id);
            let cdr = self.heap.cdr(id);

            if self.is_comma(car) {
                // ,x -> x (unquote)
                let comma_cdr = self.heap.cdr_val(car)?;
                let val = self.heap.car_val(comma_cdr)?;
                segments.push(self.wrap_in_list_call(val)?);
            } else if self.is_comma_at(car)? {
                // ,@x -> x (splice)
                let splice_cdr = self.heap.cdr_val(car)?;
                let val = self.heap.car_val(splice_cdr)?;
                segments.push(val);
            } else if car.is_pair() {
                // Nested list: recurse
                let expanded = self.expand_backquote(car)?;
                segments.push(self.wrap_in_list_call(expanded)?);
            } else {
                // Atom: quote it, wrap in list
                let quoted = self.make_quoted(car)?;
                segments.push(self.wrap_in_list_call(quoted)?);
            }

            current = cdr;
        }

        // Handle dotted tail
        if !current.is_nil() {
            // dotted pair: the last segment is the expanded tail
            let tail = self.expand_backquote(current)?;
            if segments.is_empty() {
                return Ok(tail);
            }
            // Append all segments, with tail as the last
            return self.build_append_chain(&segments, tail);
        }

        if segments.is_empty() {
            return Ok(BelValue::Nil);
        }

        // Build append chain ending in nil
        self.build_append_chain(&segments, BelValue::Nil)
    }

    /// Build (append seg1 (append seg2 ... tail))
    fn build_append_chain(&mut self, segments: &[BelValue], tail: BelValue) -> BelResult<BelValue> {
        let append_sym = BelValue::Symbol(sym::APPEND);

        let mut result = tail;
        for seg in segments.iter().rev() {
            // (append seg result)
            let inner = self.heap.alloc(result, BelValue::Nil)?;
            let args = self.heap.alloc(*seg, BelValue::Pair(inner))?;
            let call = self.heap.alloc(append_sym, BelValue::Pair(args))?;
            result = BelValue::Pair(call);
        }
        Ok(result)
    }

    /// Wrap a value in (list val): (list val) = (cons val nil)
    fn wrap_in_list_call(&mut self, val: BelValue) -> BelResult<BelValue> {
        let list_sym = BelValue::Symbol(sym::LIST);
        let inner = self.heap.alloc(val, BelValue::Nil)?;
        let call = self.heap.alloc(list_sym, BelValue::Pair(inner))?;
        Ok(BelValue::Pair(call))
    }

    /// Make (quote val)
    fn make_quoted(&mut self, val: BelValue) -> BelResult<BelValue> {
        let quote_sym = BelValue::Symbol(sym::QUOTE);
        let inner = self.heap.alloc(val, BelValue::Nil)?;
        let outer = self.heap.alloc(quote_sym, BelValue::Pair(inner))?;
        Ok(BelValue::Pair(outer))
    }

    /// Check if a value is (comma x)
    fn is_comma(&self, val: BelValue) -> bool {
        if let BelValue::Pair(id) = val {
            let car = self.heap.car(id);
            car == BelValue::Symbol(sym::COMMA)
        } else {
            false
        }
    }

    /// Check if a value is (comma-at x) or (splice x)
    fn is_comma_at(&self, val: BelValue) -> BelResult<bool> {
        if let BelValue::Pair(id) = val {
            let car = self.heap.car(id);
            Ok(car == BelValue::Symbol(sym::COMMA_AT) || car == BelValue::Symbol(sym::SPLICE))
        } else {
            Ok(false)
        }
    }

    /// Read comma: ,expr or ,@expr
    fn read_comma(&mut self) -> BelResult<BelValue> {
        self.advance(); // consume ','

        if self.peek() == Some(b'@') {
            self.advance(); // consume '@'
            let expr = self.read_expr()?;
            // Build (comma-at expr)
            let tag = BelValue::Symbol(sym::COMMA_AT);
            let inner = self.heap.alloc(expr, BelValue::Nil)?;
            let outer = self.heap.alloc(tag, BelValue::Pair(inner))?;
            return Ok(BelValue::Pair(outer));
        }

        let expr = self.read_expr()?;
        // Build (comma expr)
        let tag = BelValue::Symbol(sym::COMMA);
        let inner = self.heap.alloc(expr, BelValue::Nil)?;
        let outer = self.heap.alloc(tag, BelValue::Pair(inner))?;
        Ok(BelValue::Pair(outer))
    }

    /// Read a string: "hello" -> list of chars
    fn read_string(&mut self) -> BelResult<BelValue> {
        self.advance(); // consume '"'
        let mut chars = Vec::new();

        loop {
            let ch = self.advance().ok_or_else(|| BelError::ReadError("unterminated string".into()))?;
            match ch {
                b'"' => break,
                b'\\' => {
                    let esc = self.advance().ok_or_else(|| BelError::ReadError("unterminated escape".into()))?;
                    let c = match esc {
                        b'n' => b'\n',
                        b't' => b'\t',
                        b'r' => b'\r',
                        b'\\' => b'\\',
                        b'"' => b'"',
                        other => other,
                    };
                    chars.push(BelValue::Char(CharId(c)));
                }
                c => {
                    if c < 128 {
                        chars.push(BelValue::Char(CharId(c)));
                    }
                }
            }
        }

        self.heap.list(&chars)
    }

    /// Read a character literal: \a, \bel, \sp, etc.
    fn read_char(&mut self) -> BelResult<BelValue> {
        self.advance(); // consume '\'

        // Read the character name or single char
        let ch = self.peek().ok_or_else(|| BelError::ReadError("expected character after \\".into()))?;

        // Check for named characters (multi-character names)
        if ch.is_ascii_alphabetic() {
            let start = self.pos;
            while self.pos < self.input.len() && self.input[self.pos].is_ascii_alphanumeric() {
                self.pos += 1;
            }
            let name = std::str::from_utf8(&self.input[start..self.pos])
                .map_err(|_| BelError::ReadError("invalid UTF-8 in char name".into()))?;

            // Check if it's a single character
            if name.len() == 1 {
                return Ok(BelValue::Char(CharId(name.as_bytes()[0])));
            }

            // Check for named characters
            if let Some(cid) = CharTable::named_char(name) {
                return Ok(BelValue::Char(cid));
            }

            return Err(BelError::ReadError(format!("unknown character name: {}", name)));
        }

        // Single non-alpha character
        self.advance();
        if ch < 128 {
            Ok(BelValue::Char(CharId(ch)))
        } else {
            // Handle multi-byte UTF-8: broken bar ¦ (U+00A6) = 0xC2 0xA6
            if ch == 0xC2 && self.pos < self.input.len() && self.input[self.pos] == 0xA6 {
                self.pos += 1; // consume second byte
                return Ok(BelValue::Char(CharId(166))); // U+00A6
            }
            Err(BelError::ReadError("non-ASCII character".into()))
        }
    }

    /// Read an escaped symbol: |symbol name|
    fn read_escaped_symbol(&mut self) -> BelResult<BelValue> {
        self.advance(); // consume '|'
        let mut name = String::new();
        loop {
            let ch = self.advance().ok_or_else(|| BelError::ReadError("unterminated |symbol|".into()))?;
            if ch == b'|' {
                break;
            }
            name.push(ch as char);
        }
        let id = self.symbols.intern(&name);
        Ok(BelValue::Symbol(id))
    }

    /// Read shared structure: #n= or #n
    fn read_shared(&mut self) -> BelResult<BelValue> {
        self.advance(); // consume '#'
        // For now, skip shared structure support (not needed for bel.bel)
        // Just read the number and = or reference
        let mut num = 0u32;
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
            num = num * 10 + (self.input[self.pos] - b'0') as u32;
            self.pos += 1;
        }
        if self.peek() == Some(b'=') {
            self.advance(); // consume '='
            // Read the labeled expression
            self.read_expr()
        } else {
            // Reference — for now, return a placeholder
            Err(BelError::ReadError("shared structure references not yet supported".into()))
        }
    }

    /// Read a word: symbol, number, or intrasymbol expression.
    fn read_word(&mut self) -> BelResult<BelValue> {
        let start = self.pos;
        while self.pos < self.input.len() && !self.is_delimiter(self.input[self.pos]) {
            self.pos += 1;
        }

        let word = std::str::from_utf8(&self.input[start..self.pos])
            .map_err(|_| BelError::ReadError("invalid UTF-8".into()))?
            .to_string();

        if word.is_empty() {
            return Err(BelError::ReadError("empty word".into()));
        }

        // Check for special symbols
        if word == "nil" {
            return Ok(BelValue::Nil);
        }
        if word == "t" {
            return Ok(BelValue::Symbol(sym::T));
        }

        // Try to parse as a number
        if let Some(num_val) = self.try_parse_number(&word)? {
            return Ok(num_val);
        }

        // Check for intrasymbol characters: ~ : . !
        if let Some(result) = self.try_intrasymbol(&word)? {
            return Ok(result);
        }

        // Plain symbol
        let id = self.symbols.intern(&word);
        Ok(BelValue::Symbol(id))
    }

    fn is_delimiter(&self, ch: u8) -> bool {
        ch == b' ' || ch == b'\t' || ch == b'\n' || ch == b'\r'
            || ch == b'(' || ch == b')' || ch == b'[' || ch == b']'
            || ch == b'\'' || ch == b'`' || ch == b',' || ch == b'"'
            || ch == b';'
    }

    /// Try to parse a word as a number.
    /// Returns None if not a number.
    fn try_parse_number(&mut self, word: &str) -> BelResult<Option<BelValue>> {
        let s = word;

        // Check if it looks like a number: starts with digit, or starts with - followed by digit
        let first = s.as_bytes()[0];
        let looks_numeric = first.is_ascii_digit()
            || (first == b'-' && s.len() > 1 && s.as_bytes()[1].is_ascii_digit())
            || (first == b'+' && s.len() > 1 && s.as_bytes()[1].is_ascii_digit());

        if !looks_numeric {
            return Ok(None);
        }

        // Parse integer, rational, or decimal
        // Check for complex: has 'i' at end
        if s.ends_with('i') {
            // Complex number — skip for now, return as symbol
            return Ok(None);
        }

        // Check for rational: a/b
        if let Some(slash_pos) = s.find('/') {
            let numer_str = &s[..slash_pos];
            let denom_str = &s[slash_pos + 1..];
            if let (Ok(n), Ok(d)) = (numer_str.parse::<i64>(), denom_str.parse::<i64>()) {
                if d != 0 {
                    return Ok(Some(self.make_number(n, d)?));
                }
            }
            return Ok(None);
        }

        // Check for decimal: has '.'
        if let Some(dot_pos) = s.find('.') {
            let int_part = &s[..dot_pos];
            let frac_part = &s[dot_pos + 1..];
            if frac_part.chars().all(|c| c.is_ascii_digit()) && !frac_part.is_empty() {
                let int_val: i64 = if int_part.is_empty() || int_part == "-" {
                    0
                } else {
                    int_part.parse().unwrap_or(0)
                };
                let sign = if int_part.starts_with('-') { -1i64 } else { 1 };
                let denom = 10i64.pow(frac_part.len() as u32);
                let frac_val: i64 = frac_part.parse().unwrap_or(0);
                let numer = int_val.abs() * denom + frac_val;
                return Ok(Some(self.make_number(sign * numer, denom)?));
                }
            return Ok(None);
        }

        // Plain integer
        if let Ok(n) = s.parse::<i64>() {
            return Ok(Some(self.make_number(n, 1)?));
        }

        Ok(None)
    }

    /// Build (lit num (sign numer denom) (sign numer denom)) for a real number n/d.
    /// sign is + or - symbol. numer and denom are unary lists of t.
    /// Imaginary part is srzero = (+ nil (t)) = +0/1.
    fn make_number(&mut self, numer: i64, denom: i64) -> BelResult<BelValue> {
        // Simplify the fraction
        let g = gcd(numer.unsigned_abs(), denom.unsigned_abs());
        let n = numer.unsigned_abs() / g;
        let d = denom.unsigned_abs() / g;
        let sign_positive = (numer >= 0) == (denom > 0);

        let sign = if sign_positive {
            BelValue::Symbol(sym::PLUS)
        } else {
            BelValue::Symbol(sym::MINUS)
        };

        let numer_unary = self.make_unary(n)?;
        let denom_unary = self.make_unary(d)?;

        // Real part: (sign numer denom)
        let real = self.heap.list(&[sign, numer_unary, denom_unary])?;

        // Imaginary part: srzero = (+ nil (t)) = +0/1
        let imag_sign = BelValue::Symbol(sym::PLUS);
        let imag_numer = BelValue::Nil; // 0 in unary = nil (empty list)
        let imag_denom_list = self.heap.alloc(BelValue::Symbol(sym::T), BelValue::Nil)?;
        let imag = self.heap.list(&[imag_sign, imag_numer, BelValue::Pair(imag_denom_list)])?;

        // (lit num real imag)
        let inner = self.heap.list(&[
            BelValue::Symbol(sym::LIT),
            BelValue::Symbol(sym::NUM),
            real,
            imag,
        ])?;

        Ok(inner)
    }

    /// Make a unary integer: n -> (t t t ... t) with n elements
    fn make_unary(&mut self, n: u64) -> BelResult<BelValue> {
        let mut result = BelValue::Nil;
        for _ in 0..n {
            let pair = self.heap.alloc(BelValue::Symbol(sym::T), result)?;
            result = BelValue::Pair(pair);
        }
        Ok(result)
    }

    /// Try to process intrasymbol characters.
    /// Precedence: | > . ! > : > ~
    fn try_intrasymbol(&mut self, word: &str) -> BelResult<Option<BelValue>> {
        // Check for ~ (negate/compose with no)
        if word.starts_with('~') {
            let rest = &word[1..];
            if rest.is_empty() {
                return Ok(None); // Just ~, treat as symbol
            }
            let inner = self.intern_or_intrasymbol(rest)?;
            // (~f) -> (compose no f)
            let compose = BelValue::Symbol(sym::COMPOSE);
            let no = BelValue::Symbol(sym::NO);
            let result = self.heap.list(&[compose, no, inner])?;
            return Ok(Some(result));
        }

        // Check for : (compose)
        if word.contains(':') && !word.starts_with(':') && !word.ends_with(':') {
            let parts: Vec<&str> = word.splitn(2, ':').collect();
            if parts.len() == 2 {
                let left = self.intern_or_intrasymbol(parts[0])?;
                let right = self.intern_or_intrasymbol(parts[1])?;
                let compose = BelValue::Symbol(sym::COMPOSE);
                let result = self.heap.list(&[compose, left, right])?;
                return Ok(Some(result));
            }
        }

        // Check for . (apply / call) and ! (apply with quote)
        if word.contains('.') || word.contains('!') {
            // Find the first . or !
            let dot_pos = word.find('.');
            let bang_pos = word.find('!');
            let (sep_pos, is_bang) = match (dot_pos, bang_pos) {
                (Some(d), Some(b)) => if d < b { (d, false) } else { (b, true) },
                (Some(d), None) => (d, false),
                (None, Some(b)) => (b, true),
                (None, None) => unreachable!(),
            };

            let left = &word[..sep_pos];
            let right = &word[sep_pos + 1..];

            if left.is_empty() {
                // Leading . or ! — implicit upon
                let upon = BelValue::Symbol(self.symbols.intern("upon"));
                let arg = self.intern_or_intrasymbol(right)?;
                if is_bang {
                    let quoted = self.make_quoted(arg)?;
                    let result = self.heap.list(&[upon, quoted])?;
                    return Ok(Some(result));
                } else {
                    let result = self.heap.list(&[upon, arg])?;
                    return Ok(Some(result));
                }
            }

            if right.is_empty() {
                return Ok(None); // Trailing dot, treat as symbol
            }

            let f = self.intern_or_intrasymbol(left)?;
            let arg = self.intern_or_intrasymbol(right)?;

            if is_bang {
                // a!b -> (a 'b)
                let quoted = self.make_quoted(arg)?;
                let result = self.heap.list(&[f, quoted])?;
                return Ok(Some(result));
            } else {
                // a.b -> (a b)
                let result = self.heap.list(&[f, arg])?;
                return Ok(Some(result));
            }
        }

        Ok(None)
    }

    /// Intern a word, processing intrasymbol chars recursively if needed.
    fn intern_or_intrasymbol(&mut self, word: &str) -> BelResult<BelValue> {
        if word == "nil" {
            return Ok(BelValue::Nil);
        }
        if word == "t" {
            return Ok(BelValue::Symbol(sym::T));
        }
        if let Some(num) = self.try_parse_number(word)? {
            return Ok(num);
        }
        if let Some(result) = self.try_intrasymbol(word)? {
            return Ok(result);
        }
        let id = self.symbols.intern(word);
        Ok(BelValue::Symbol(id))
    }
}

/// GCD for fraction simplification.
fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 { a } else { gcd(b, a % b) }
}

/// Read a single expression from a string.
pub fn read_str(input: &str, heap: &mut Heap, symbols: &mut SymbolTable) -> BelResult<BelValue> {
    let mut reader = Reader::new(input, heap, symbols);
    reader.read()?.ok_or_else(|| BelError::ReadError("empty input".into()))
}

/// Read all expressions from a string.
pub fn read_all(input: &str, heap: &mut Heap, symbols: &mut SymbolTable) -> BelResult<Vec<BelValue>> {
    let mut reader = Reader::new(input, heap, symbols);
    reader.read_all()
}

/// Read one expression starting at byte offset `pos`.
/// Returns `Ok(Some((value, new_pos)))` or `Ok(None)` if only whitespace/comments remain.
pub fn read_one_at(input: &str, pos: usize, heap: &mut Heap, symbols: &mut SymbolTable) -> BelResult<Option<(BelValue, usize)>> {
    let mut reader = Reader::new(&input[pos..], heap, symbols);
    match reader.read()? {
        Some(val) => Ok(Some((val, pos + reader.position()))),
        None => Ok(None),
    }
}
