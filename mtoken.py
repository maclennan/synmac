""" Inputs and outputs token streams.
    To do:
    See if some way to allow lexical command to begin file.
    """
import sys

class Mtoken:
    """Lexical Tokens"""
    
    OpenQuote = '"'
    CloseQuote = '"'
    SuperQuote = '\\'
    Flag = '\\'
    ExtraAlpha = ""
    
    def equals(self, other) -> bool:
        """Compare this token with another for equality"""
        if self.kind != other.kind:
            return False
        elif self.kind == 'endline' or self.kind == 'endfile':
            return True
        else:
            return self.chars == other.chars
            
    def show(self):
        """Print description of a token."""
        print( "{" + self.kind + "'" + self.chars + "'}")

    def string(self):
        """Translate token into string for display purposes."""
        return self.chars + " "

class endline(Mtoken):
    """token for end and beginning of line"""
    def __init__(self):
        self.kind = 'endline'
        self.chars = "<newline>"

class word(Mtoken):
    """token for single- or multi-character token"""
    def __init__(self, characters):
        self.kind = 'word'
        self.chars = characters

class whitespace(Mtoken):
    """token of arbitrary whitespace"""
    def __init__(self, characters):
        self.kind = 'whitespace'
        self.chars = characters

class stringlit(Mtoken):
    """string literal town"""
    def __init__(self, characters):
        self.kind = 'stringlit'
        self.chars = characters
        
class endfile(Mtoken):
    """end of file token"""
    def __init__(self):
        self.kind = 'endfile'
        self.chars = "<eof>"
        
class tokensout:
    """Write tokens to output file."""
    
    def put(t: Mtoken):
        write = sys.stdout.write
        if type(t) == endline:
            write('\n')
        elif type(t) == word or type(t) == whitespace:
            write(t.chars)
        elif type(t) == stringlit:
            write(t.chars)
        elif type(t) == endfile:
            sys.stdout.close()
        else:
            pass
            
class tokensin:
    """Read tokens from input file"""
    
    nextch = None
    
    def __init__(self, fn = ''):
        if fn == '':
            self.infile = sys.stdin
        else:
            self.infile = open(fn, 'r')

            
    def close(self):
        self.infile.close()
    
    def start(self):
        """prime input token stream"""
        self.nextch = self.infile.read(1)
    
    def next(self) -> Mtoken:
        """get next token from input stream"""
        
        ch = self.nextch
        if ch == '':
            return endfile()
        elif ch == '\n':
            self.nextch = self.infile.read(1)
            if self.nextch == Mtoken.Flag: # cannot be first in file
                self.process_lexical_command()
            return endline()
        elif ch == Mtoken.OpenQuote:
            return stringlit(self.acceptstring(Mtoken.CloseQuote))
        elif ch.isalnum() or ch in Mtoken.ExtraAlpha:
            return word(self.acceptword())
        elif self.iswhitechar(ch):
            return whitespace(self.acceptwhitespace())
        else:
            t = word(ch)
            self.nextch = self.infile.read(1)
            return t
            
    def acceptstring (self, cq: str) -> str:
        s = ""
        read = self.infile.read
        ch = read(1)
        while ch != cq:
            if ch == "":
                print('*** Unterminated string "' + s + '"', file=sys.stderr)
                return s + "<end of file>"
            if ch == Mtoken.SuperQuote:
                ch = read(1)
            s += ch
            ch = read(1)
        self.nextch = read(1)
        return s

    def acceptword(self) -> str:
        s = self.nextch
        read = self.infile.read
        ch = read(1)
        while ch.isalnum() or (ch in Mtoken.ExtraAlpha and ch != ''):
            s += ch
            ch = read(1)
        self.nextch = ch
        return s

    def iswhitechar(self, ch: str) -> bool:
        return ch == ' ' or ch == '\t'
    
    def acceptwhitespace(self) -> str:
        s = self.nextch
        read = self.infile.read
        ch = read(1)
        while self.iswhitechar(ch):
            s += ch
            ch = read(1)
        self.nextch = ch
        return s
    
    def process_lexical_command(self):
        """ Process a lexical command for altering various lexical parameters.
        """
        self.nextch = self.infile.read(1)
        t = self.next()
        if type(t) != word:
            print("*** Illegal lexical command: " + t.string(), file=sys.stderr)
            return
        command = t.chars
        if self.nextch == ' ':
            self.acceptwhitespace()
        t = self.next()
        if type(t) != stringlit:
            print("*** Illegal '" + command + "' argument: " + t.string(), file=sys.stderr)
            return           
        arg = t.chars
        if command == "alpha": 
            set_extra_alpha(arg)
        elif command == "quotes": 
            set_quotes(arg)
        elif command == "superquote":
            set_superquote(arg)
        elif command == "flag":
            set_flag(arg)
        else:
            print("*** Illegal lexical command: " + command, file=sys.stderr)
        return

def set_extra_alpha(arg: str):
    Mtoken.ExtraAlpha = arg
    return

def set_quotes(arg: str):
    if len(arg) != 1 and len(arg) != 2:
        print("*** Quotes must be string of one or two characters, not " + arg, file=sys.stderr)
    Mtoken.OpenQuote = arg[0]
    if len(arg) == 2:
        Mtoken.CloseQuote = arg[1]
    else:
        Mtoken.CloseQuote = arg[0]
    return

def set_superquote(arg: str):
    if len(arg) != 1:
        print("*** Superquote must be single character, not " + arg, file=sys.stderr)
    Mtoken.SuperQuote = arg[0]
    return

def set_flag(arg: str):
    if len(arg) != 1:
        print("*** Flag must be single character, not " + arg, file=sys.stderr)
    Mtoken.Flag = arg
    return
