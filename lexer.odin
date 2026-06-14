package lexer

import "core:os"

TokType :: enum {
    ID,
    INT_LITERAL,         // 42, 0x2A, 0o177, 0b1101_0010
    FLOAT_LITERAL,       // 3.14, 2.5e-10, .5
    CHAR_LITERAL,        // 'a', '\n'
    STRING_LITERAL,      // "hello"

    PLUS,                // +
    MINUS,               // -
    ASTERISK,            // *
    SLASH,               // /
    PERCENT,             // %

    EQUAL_EQUAL,         // ==
    BANG_EQUAL,          // !=
    LESS,                // <
    LESS_EQUAL,          // <=
    GREATER,             // >
    GREATER_EQUAL,       // >=

    AMPERSAND_AMPERSAND, // &&
    PIPE_PIPE,           // ||
    BANG,                // !

    AMPERSAND,           // &
    PIPE,                // |
    CARET,               // ^
    TILDE,               // ~
    LESS_LESS,           // <<
    GREATER_GREATER,     // >>

    EQUAL,               // =
    PLUS_EQUAL,          // +=
    MINUS_EQUAL,         // -=
    ASTERISK_EQUAL,      // *=
    SLASH_EQUAL,         // /=
    PERCENT_EQUAL,       // %=
    AMPERSAND_EQUAL,     // &=
    PIPE_EQUAL,          // |=
    CARET_EQUAL,         // ^=
    LESS_LESS_EQUAL,     // <<=
    GREATER_GREATER_EQUAL, // >>=

    PLUS_PLUS,           // ++
    MINUS_MINUS,         // --

    ARROW,               // ->
    DOT,                 // .
    LPAREN,              // (
    RPAREN,              // )
    LBRACKET,            // [
    RBRACKET,            // ]
    LBODY,               // {
    RBODY,               // }
    SEMICOLON,           // ;
    COLON,               // :
    COMMA,               // ,
    QUESTION,            // ?
    ELLIPSIS,            // ...

    HASH,                // #
    HASH_HASH,           // ##

    NEWLINE,
    ILLEGAL,             // invalid lexem
    EOF,
}

Token :: struct {
    type: TokType,
    text: string,
    line: int,
    col:  int,
}

Lexer :: struct {
    src:    string,
    tokens: [dynamic]Token,
    cur:    int,
    line:   int,
    col:    int,
}

is_digit :: proc(ch: u8) -> bool { return ch >= '0' && ch <= '9' }
is_alpha :: proc(ch: u8) -> bool { return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') }
is_hex_digit :: proc(ch: u8) -> bool {
    return is_digit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
}
is_oct_digit :: proc(ch: u8) -> bool { return ch >= '0' && ch <= '7' }
is_bin_digit :: proc(ch: u8) -> bool { return ch == '0' || ch == '1' }

skip_whitespace_and_comments :: proc(self: ^Lexer) {
    for self.cur < len(self.src) {
        ch := self.src[self.cur]
        switch ch {
        case ' ', '\t', '\r':
            self.cur += 1
            self.col += 1
        case '\n':
            return
        case '/':
            if self.cur+1 >= len(self.src) do break
            nxt := self.src[self.cur+1]
            if nxt == '/' {
                self.cur += 2
                self.col += 2
                for self.cur < len(self.src) && self.src[self.cur] != '\n' {
                    self.cur += 1
                    self.col += 1
                }
            } else if nxt == '*' {
                self.cur += 2
                self.col += 2
                for self.cur < len(self.src) {
                    if self.src[self.cur] == '*' && self.cur+1 < len(self.src) && self.src[self.cur+1] == '/' {
                        self.cur += 2
                        self.col += 2
                        break
                    }
                    if self.src[self.cur] == '\n' {
                        self.line += 1
                        self.col = 0
                    } else {
                        self.col += 1
                    }
                    self.cur += 1
                }
            } else {
                break
            }
        case:
            break
        }
        if ch != ' ' && ch != '\t' && ch != '\r' && ch != '\n' && ch != '/' {
            break
        }
    }
}

lex_get_next_token :: proc(self: ^Lexer) -> (res: Token) {
    if self.cur >= len(self.src) {
        res.type = .EOF
        return
    }

    skip_whitespace_and_comments(self)

    if self.cur >= len(self.src) {
        res.type = .EOF
        return
    }

    if self.src[self.cur] == '\n' {
        res.type = .NEWLINE
        res.text = "\n"
        res.line = self.line
        res.col  = self.col
        self.cur += 1
        self.line += 1
        self.col = 0
        return
    }

    res.line = self.line
    res.col  = self.col

    ch := self.src[self.cur]

    if ch == '"' {
        self.cur += 1
        self.col += 1
        start := self.cur
        for self.cur < len(self.src) && self.src[self.cur] != '"' {
            if self.src[self.cur] == '\\' {
                self.cur += 1
                self.col += 1
                if self.cur < len(self.src) {
                    if self.src[self.cur] == '\n' {
                        self.line += 1
                        self.col = 0
                    } else {
                        self.col += 1
                    }
                    self.cur += 1
                }
            } else {
                if self.src[self.cur] == '\n' {
                    self.line += 1
                    self.col = 0
                } else {
                    self.col += 1
                }
                self.cur += 1
            }
        }
        if self.cur < len(self.src) && self.src[self.cur] == '"' {
            res.text = self.src[start:self.cur]
            self.cur += 1
            self.col += 1
        } else {
            res.text = self.src[start:self.cur]
        }
        res.type = .STRING_LITERAL
        return
    }

    if ch == '\'' {
        self.cur += 1
        self.col += 1
        start := self.cur
        if self.cur < len(self.src) {
            if self.src[self.cur] == '\\' {
                self.cur += 1
                self.col += 1
                if self.cur < len(self.src) {
                    if self.src[self.cur] == '\n' {
                        self.line += 1
                        self.col = 0
                    } else {
                        self.col += 1
                    }
                    self.cur += 1
                }
            } else if self.src[self.cur] != '\'' {
                if self.src[self.cur] == '\n' {
                    self.line += 1
                    self.col = 0
                } else {
                    self.col += 1
                }
                self.cur += 1
            }
        }
        if self.cur < len(self.src) && self.src[self.cur] == '\'' {
            res.text = self.src[start:self.cur]
            self.cur += 1
            self.col += 1
        } else {
            res.text = self.src[start:self.cur]
        }
        res.type = .CHAR_LITERAL
        return
    }

    if is_digit(ch) || (ch == '.' && self.cur+1 < len(self.src) && is_digit(self.src[self.cur+1])) {
        return lex_number(self)
    }

    if is_alpha(ch) || ch == '_' {
        return lex_identifier(self)
    }

    switch ch {
    case '+':
        if self.cur+1 < len(self.src) {
            n := self.src[self.cur+1]
            if n == '+' {
                res.type = .PLUS_PLUS; res.text = "++"
                self.cur += 2; self.col += 2
            } else if n == '=' {
                res.type = .PLUS_EQUAL; res.text = "+="
                self.cur += 2; self.col += 2
            } else {
                res.type = .PLUS; res.text = "+"; self.cur += 1; self.col += 1
            }
        } else {
            res.type = .PLUS; res.text = "+"; self.cur += 1; self.col += 1
        }
    case '-':
        if self.cur+1 < len(self.src) {
            n := self.src[self.cur+1]
            if n == '-' {
                res.type = .MINUS_MINUS; res.text = "--"
                self.cur += 2; self.col += 2
            } else if n == '=' {
                res.type = .MINUS_EQUAL; res.text = "-="
                self.cur += 2; self.col += 2
            } else if n == '>' {
                res.type = .ARROW; res.text = "->"
                self.cur += 2; self.col += 2
            } else {
                res.type = .MINUS; res.text = "-"; self.cur += 1; self.col += 1
            }
        } else {
            res.type = .MINUS; res.text = "-"; self.cur += 1; self.col += 1
        }
    case '*':
        if self.cur+1 < len(self.src) && self.src[self.cur+1] == '=' {
            res.type = .ASTERISK_EQUAL; res.text = "*="
            self.cur += 2; self.col += 2
        } else {
            res.type = .ASTERISK; res.text = "*"; self.cur += 1; self.col += 1
        }
    case '/':
        if self.cur+1 < len(self.src) && self.src[self.cur+1] == '=' {
            res.type = .SLASH_EQUAL; res.text = "/="
            self.cur += 2; self.col += 2
        } else {
            res.type = .SLASH; res.text = "/"; self.cur += 1; self.col += 1
        }
    case '%':
        if self.cur+1 < len(self.src) && self.src[self.cur+1] == '=' {
            res.type = .PERCENT_EQUAL; res.text = "%="
            self.cur += 2; self.col += 2
        } else {
            res.type = .PERCENT; res.text = "%"; self.cur += 1; self.col += 1
        }
    case '=':
        if self.cur+1 < len(self.src) && self.src[self.cur+1] == '=' {
            res.type = .EQUAL_EQUAL; res.text = "=="
            self.cur += 2; self.col += 2
        } else {
            res.type = .EQUAL; res.text = "="; self.cur += 1; self.col += 1
        }
    case '!':
        if self.cur+1 < len(self.src) && self.src[self.cur+1] == '=' {
            res.type = .BANG_EQUAL; res.text = "!="
            self.cur += 2; self.col += 2
        } else {
            res.type = .BANG; res.text = "!"; self.cur += 1; self.col += 1
        }
    case '<':
        if self.cur+1 < len(self.src) {
            n := self.src[self.cur+1]
            if n == '=' {
                res.type = .LESS_EQUAL; res.text = "<="
                self.cur += 2; self.col += 2
            } else if n == '<' {
                if self.cur+2 < len(self.src) && self.src[self.cur+2] == '=' {
                    res.type = .LESS_LESS_EQUAL; res.text = "<<="
                    self.cur += 3; self.col += 3
                } else {
                    res.type = .LESS_LESS; res.text = "<<"
                    self.cur += 2; self.col += 2
                }
            } else {
                res.type = .LESS; res.text = "<"; self.cur += 1; self.col += 1
            }
        } else {
            res.type = .LESS; res.text = "<"; self.cur += 1; self.col += 1
        }
    case '>':
        if self.cur+1 < len(self.src) {
            n := self.src[self.cur+1]
            if n == '=' {
                res.type = .GREATER_EQUAL; res.text = ">="
                self.cur += 2; self.col += 2
            } else if n == '>' {
                if self.cur+2 < len(self.src) && self.src[self.cur+2] == '=' {
                    res.type = .GREATER_GREATER_EQUAL; res.text = ">>="
                    self.cur += 3; self.col += 3
                } else {
                    res.type = .GREATER_GREATER; res.text = ">>"
                    self.cur += 2; self.col += 2
                }
            } else {
                res.type = .GREATER; res.text = ">"; self.cur += 1; self.col += 1
            }
        } else {
            res.type = .GREATER; res.text = ">"; self.cur += 1; self.col += 1
        }
    case '&':
        if self.cur+1 < len(self.src) {
            n := self.src[self.cur+1]
            if n == '&' {
                res.type = .AMPERSAND_AMPERSAND; res.text = "&&"
                self.cur += 2; self.col += 2
            } else if n == '=' {
                res.type = .AMPERSAND_EQUAL; res.text = "&="
                self.cur += 2; self.col += 2
            } else {
                res.type = .AMPERSAND; res.text = "&"; self.cur += 1; self.col += 1
            }
        } else {
            res.type = .AMPERSAND; res.text = "&"; self.cur += 1; self.col += 1
        }
    case '|':
        if self.cur+1 < len(self.src) {
            n := self.src[self.cur+1]
            if n == '|' {
                res.type = .PIPE_PIPE; res.text = "||"
                self.cur += 2; self.col += 2
            } else if n == '=' {
                res.type = .PIPE_EQUAL; res.text = "|="
                self.cur += 2; self.col += 2
            } else {
                res.type = .PIPE; res.text = "|"; self.cur += 1; self.col += 1
            }
        } else {
            res.type = .PIPE; res.text = "|"; self.cur += 1; self.col += 1
        }
    case '^':
        if self.cur+1 < len(self.src) && self.src[self.cur+1] == '=' {
            res.type = .CARET_EQUAL; res.text = "^="
            self.cur += 2; self.col += 2
        } else {
            res.type = .CARET; res.text = "^"; self.cur += 1; self.col += 1
        }
    case '~':
        res.type = .TILDE; res.text = "~"; self.cur += 1; self.col += 1
    case '.':
        if self.cur+2 < len(self.src) && self.src[self.cur+1] == '.' && self.src[self.cur+2] == '.' {
            res.type = .ELLIPSIS; res.text = "..."
            self.cur += 3; self.col += 3
        } else {
            res.type = .DOT; res.text = "."; self.cur += 1; self.col += 1
        }
    case '(': res.type = .LPAREN;    res.text = "("; self.cur += 1; self.col += 1
    case ')': res.type = .RPAREN;    res.text = ")"; self.cur += 1; self.col += 1
    case '[': res.type = .LBRACKET;  res.text = "["; self.cur += 1; self.col += 1
    case ']': res.type = .RBRACKET;  res.text = "]"; self.cur += 1; self.col += 1
    case '{': res.type = .LBODY;     res.text = "{"; self.cur += 1; self.col += 1
    case '}': res.type = .RBODY;     res.text = "}"; self.cur += 1; self.col += 1
    case ';': res.type = .SEMICOLON; res.text = ";"; self.cur += 1; self.col += 1
    case ':': res.type = .COLON;     res.text = ":"; self.cur += 1; self.col += 1
    case ',': res.type = .COMMA;     res.text = ","; self.cur += 1; self.col += 1
    case '?': res.type = .QUESTION;  res.text = "?"; self.cur += 1; self.col += 1
    case '#':
        if self.cur+1 < len(self.src) && self.src[self.cur+1] == '#' {
            res.type = .HASH_HASH; res.text = "##"
            self.cur += 2; self.col += 2
        } else {
            res.type = .HASH; res.text = "#"; self.cur += 1; self.col += 1
        }
    case:
        res.type = .ILLEGAL
        res.text = self.src[self.cur:self.cur+1]
        self.cur += 1
        self.col += 1
    }

    return
}

lex_number :: proc(self: ^Lexer) -> (res: Token) {
    res.line = self.line
    res.col  = self.col
    start := self.cur
    is_float := false

    if self.src[self.cur] == '0' && self.cur+1 < len(self.src) {
        next := self.src[self.cur+1]
        switch next {
        case 'x', 'X':
            self.cur += 2; self.col += 2
            if self.cur >= len(self.src) || !is_hex_digit(self.src[self.cur]) {
                res.type = .ILLEGAL
                res.text = self.src[start:self.cur]
                return
            }
            for self.cur < len(self.src) && (is_hex_digit(self.src[self.cur]) || self.src[self.cur] == '_') {
                self.cur += 1; self.col += 1
            }
            res.text = self.src[start:self.cur]
            res.type = .INT_LITERAL
            return
        case 'o', 'O':
            self.cur += 2; self.col += 2
            if self.cur >= len(self.src) || !is_oct_digit(self.src[self.cur]) {
                res.type = .ILLEGAL
                res.text = self.src[start:self.cur]
                return
            }
            for self.cur < len(self.src) && (is_oct_digit(self.src[self.cur]) || self.src[self.cur] == '_') {
                self.cur += 1; self.col += 1
            }
            res.text = self.src[start:self.cur]
            res.type = .INT_LITERAL
            return
        case 'b', 'B':
            self.cur += 2; self.col += 2
            if self.cur >= len(self.src) || !is_bin_digit(self.src[self.cur]) {
                res.type = .ILLEGAL
                res.text = self.src[start:self.cur]
                return
            }
            for self.cur < len(self.src) && (is_bin_digit(self.src[self.cur]) || self.src[self.cur] == '_') {
                self.cur += 1; self.col += 1
            }
            res.text = self.src[start:self.cur]
            res.type = .INT_LITERAL
            return
        }
    }

    if self.src[self.cur] == '.' {
        self.cur += 1; self.col += 1
        is_float = true
        for self.cur < len(self.src) && is_digit(self.src[self.cur]) {
            self.cur += 1; self.col += 1
        }
    } else {
        for self.cur < len(self.src) && is_digit(self.src[self.cur]) {
            self.cur += 1; self.col += 1
        }
        if self.cur < len(self.src) && self.src[self.cur] == '.' &&
           self.cur+1 < len(self.src) && is_digit(self.src[self.cur+1]) {
            self.cur += 1; self.col += 1
            is_float = true
            for self.cur < len(self.src) && is_digit(self.src[self.cur]) {
                self.cur += 1; self.col += 1
            }
        }
    }

    if self.cur < len(self.src) && (self.src[self.cur] == 'e' || self.src[self.cur] == 'E') {
        pe := self.cur
        self.cur += 1; self.col += 1
        if self.cur < len(self.src) && (self.src[self.cur] == '+' || self.src[self.cur] == '-') {
            self.cur += 1; self.col += 1
        }
        if self.cur < len(self.src) && is_digit(self.src[self.cur]) {
            is_float = true
            for self.cur < len(self.src) && is_digit(self.src[self.cur]) {
                self.cur += 1; self.col += 1
            }
        } else {
            self.cur = pe
            self.col -= (self.cur - pe)
        }
    }

    res.text = self.src[start:self.cur]
    res.type = is_float ? .FLOAT_LITERAL : .INT_LITERAL
    return
}

lex_identifier :: proc(self: ^Lexer) -> (res: Token) {
    res.line = self.line
    res.col  = self.col
    start := self.cur

    for self.cur < len(self.src) {
        ch := self.src[self.cur]
        if is_alpha(ch) || is_digit(ch) || ch == '_' {
            self.cur += 1
            self.col += 1
        } else {
            break
        }
    }

    res.text = self.src[start:self.cur]
    res.type = .ID
    return
}

lex_init :: proc(filename: string) -> (res: Lexer) {
    data, ok := os.read_entire_file(filename, context.allocator)
    if ok != nil {
        res.src = ""
        res.cur = 0
        res.line = 1
        res.col = 0
        res.tokens = nil
        return
    }
    
    res.src = string(data)
    res.cur = 0
    res.line = 1
    res.col = 0
    return
}

lex_create_tokens :: proc(self: ^Lexer) {
    clear(&self.tokens)
    
    tok := lex_get_next_token(self)
    append(&self.tokens, tok)
    for tok.type != .EOF {
        tok = lex_get_next_token(self)
        append(&self.tokens, tok)
    }
}

lex_destroy :: proc(self: ^Lexer) {
    delete(self.tokens)
    delete(self.src)
    self.tokens = nil
}
