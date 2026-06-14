package lexer

import "core:fmt"

main :: proc() {
    l: Lexer = lex_init("test.c"); defer lex_destroy(&l)
    
    lex_create_tokens(&l)
    
    for i in l.tokens {
        fmt.println(i)
    }
}
