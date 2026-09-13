import json
import re
import sys

'''
Lexer is the scanner/program that transfers a character
stream into a token-stream and removes comments and such
'''
def is_hex_digit(ch):
    return ch.isdigit() or ch in "abcedfABCDEF"

def valid_integer(text):
    # check if text is a valid integer 'xX'
    if text.startswith(("0x", "0X")):
        body = text[2:] #0x[2][3]...

        if not body:
            return False

        if body[0] == "_" or body[-1] == "_":
            return False
        if "__" in body:
            return False
        
        return all(is_hex_digit(ch) or ch == "_" for ch in body)

    if not text:
        return False
    
    if text[0] == "_" or text[-1] == "_":
        return False
    if "__" in text:
        return False

    return all(ch.isdigit() or ch == "_" for ch in text)

def lex(source):
    # convert input string into a list of tokens
    tokens = []
    i = 0
    
    while i < len(source):
        ch = source[i]
        # ignore whitespace
        if ch.isspace():
            i += 1
            continue
        # get rid of comments as noted by project description
        if source.startswith("//", i):
            i += 2
            while i < len(source) and source[i] != "\n":
                i += 1
            continue
        # two character operators --> move index by two
        if source.startswith("<<", i):
            tokens.append(("<<", "<<"))
            i += 2
            continue 
        if source.startswith(">>", i):
            tokens.append((">>", ">>"))
            i += 2
            continue
        # only move one if it is one character operand
        if ch in "~^&|()":
            tokens.append((ch, ch))
            i+= 1
            continue
        # integers 0 - 9
        if ch.isdigit():
            start = i
            # hex
            if (ch == "0" and i + 1 < len(source) and source[i+1] in "xX"):
                i +=2
                while i < len(source):
                    current = source[i]
                    if is_hex_digit(current) or current == "_":
                        i +=1
                    else:
                        break
            # else decimal integer
            else:
                i += 1
                while i < len(source):
                    current = source[i]
                    if current.isdigit() or current == "_":
                        i += 1
                    else:
                        break
            text = source[start:i]
            if not valid_integer(text):
                raise SyntaxError("invalid integer: " + text)
            tokens.append(("INTEGER", text))
            continue
        # anything else that is invalid
        raise SyntaxError("invalid character : " + repr(ch) + " \nExpected an integer, operator, (~, ^, &, |, <<, >>), or a parenthesis")
    return tokens

'''
    Parser is a program that given a token stream and a CFG,
    produces a prase tree/AST
'''
# parser works with tokens and position
tokens = [] # set it empty to initialize
pos = 0

def peek():
    # look at current token or none
    if pos < len(tokens):
        return tokens[pos]
    else:
        return None

def peek_type():
    # type of the current token
    token = peek()
    if token is None:
        return None
    else:
        return token[0]

# call consume when the symbol is a terminal symbol 't'
# if non terminal symbol then call parsing function
def consume(expected_type):
    # consume and return of expected token
    global pos
    token = peek()
    if token is None:
        raise SyntaxError("expected " + expected_type + ", found end of input ")
    if token[0] != expected_type:
        raise SyntaxError(
            "expected " + expected_type + ", found " + token[0]
        )
    pos += 1
    return token

# look to see if token type beings expression --> True
def starts_expression(token_type):
    return token_type in ("INTEGER", "(", "~")

def parse_program():
    # program : expr*
    result = []
    while pos < len(tokens):
        if not starts_expression(peek_type()):
            raise SyntaxError("expecting 'EOF' but got " + str(peek()[1]))
        result.append(parse_expr())
    return result

#expr : shiftExpr
def parse_expr():
    # expr : shiftExpr
    return parse_shift_expr()

# : bitwiseExpr ( ( '<<' | '>>' ) bitwiseExpr)?
def parse_shift_expr():
    left = parse_bitwise_expr()

    if peek_type() in ("<<", ">>"):
        operator = peek_type()
        consume(operator)

        right = parse_bitwise_expr()

        left = {
            "op": operator,
            "operand1": left,
            "operand2": right
        }
        if peek_type() in ("<<", ">>"):
            raise SyntaxError("shift operators are non-associative: " + peek()[1] + " cannot follow another shift expression without parentheses" )
    return left

# : xorExpr  ( ( '&' | '|' ) xorExpr)*
def parse_bitwise_expr():
    left = parse_xor_expr()
    '''
    using 'if' statement here led to an 
    error if it read one of the two symbols it would fail
    to read the one after the first
    '''
    while peek_type() in ("&" ,"|"):
        operator = peek_type()
        consume(operator)
        right = parse_xor_expr()

        left = {
            "op": operator,
            "operand1": left,
            "operand2": right
        }
    return left

# : unaryExpr ( '^' xorExpr)?
def parse_xor_expr():
    #unaryExpr ('^' xorExpr )
    left = parse_unary_expr()
    if peek_type() == ("^"):
        consume("^")
        # call xor once we find the correct character '^'
        right = parse_xor_expr()

        return {
            "op": "^",
            "operand1": left,
            "operand2": right
        }
    return left

# :'~' unaryExpr
def parse_unary_expr():
    if peek_type() == "~":
        consume("~")
        # | primaryExpr
        operand = parse_unary_expr()

        return {
            "op": "~",
            "operand1": operand,
            "operand2": None
        }
    return parse_primary_expr()

#: INTEGER
# | '(' expr ');
def parse_primary_expr():
    token_type = peek_type()
    if token_type == "INTEGER":
        token = consume("INTEGER")
        text = token[1]

        # remove underscores
        cleaned = text.replace("_", "")

        if cleaned.startswith(("0x", "0X")):
            return int(cleaned, 16) #base 16 number

        return int(cleaned, 10) #base 10 number 
    if token_type == "(":
        consume("(")

        result = parse_expr()

        consume(")")
        return result

    if peek() is None:
        raise SyntaxError("Expected expression, but we reached the end of the input")
    raise SyntaxError("expected expression, found " + str(peek()[1]) + " instead")

def main():
    global tokens
    global pos

    try:
        source = sys.stdin.read()

        tokens = lex(source)
        pos = 0

        result = parse_program()
        print(json.dumps(result, separators=(",", ":")))

    except (SyntaxError, ValueError) as error:
        print("error: " + str(error), file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()

