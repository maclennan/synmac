#!/Applications/anaconda/bin/python
""" Macro
    This macroprocessor does eager evaluation and is essentially like a LISP eval
    function in a macro context.
    
    To do:
        Change processing of #set so one macro.
        Revise processing of dedents so preceding &param includes final/newlines
        whitesapce before the terminating indent.
        Decide if 'expand' needed and exact syntax.
        Decide on ordering of intrinsic rules.
        In trying alternate versions of a macro, if it has to backtrack over
        large pieces of text that have side-effects, it will cause these to be
        executed multiple times. Possible solutions: (1) decrease chances with
        something like primaries above. (2) finalize parsing before any 
        evaluations. (3) put side-effects on a stack where they can be deleted 
        for failed parses.
        Decide if need lambdas.
        Make work for unicode as well as ASCII.
"""
import sys
from collections import deque
import inspect
from mtoken import *

class Macro:

    MacroFlag = word('#')
    MacroCommand = word('set')
    OpenMetaQuote = word('{')
    CloseMetaQuote = word('}')    
    MacroParameter = word('&')
    MacroParameterTrimmed = word('~')
    MacroParameterQuoted = word('\'')
    MacroNewline = word('$')
    MacroEndline = word('#')
    MacroCommit = word('!')
    MacroDedent = word('dedent')
    MacroDefine = word('syntax')
    MacroDefineAs = word('means')
    MacroDefineEnd = word('endsyntax')
    MacroDeclare = word('pattern')
    MacroDeclareEnd = word('endpattern')
    Include = word('include')
    MacroTrim = word('trim')
    MacroEndTrim = word('endtrim')
    MacroExpand = word('expand')
    MacroEndExpand = word ('endexpand')
    
    intrinsic_rules = [] # built-in rules for system parameters
    defined_rules = []   # user defined macros
    tracing = ''         # trace level
    rule_dump = False    # flag to dump rules after each definition
    BTwarning = 0        # backtrack warning flag

class PatnParameter(Mtoken):
    """ Pattern Parameter Flag"""
    def __init__(self):
        self.kind = 'parameter'
        self.chars = '\\' + Macro.MacroParameter.chars

class PatnParameterTrimmed(Mtoken):
    """ Pattern Trimmed Parameter Flag"""
    def __init__(self):
        self.kind = 'parametertrimmed'
        self.chars = '\\' + Macro.MacroParameterTrimmed.chars
        
class PatnParameterQuoted(Mtoken):
    """ Pattern Quoted Parameter Flag"""
    def __init__(self):
        self.kind = 'parameterquoted'
        self.chars = '\\' + Macro.MacroParameterQuoted.chars

class PatnNewline(Mtoken):
    """ Pattern Newline Flag"""
    def __init__(self):
        self.kind = 'newline'
        self.chars = '\\' + Macro.MacroNewline.chars

class PatnEndline(Mtoken):
    """ Pattern Newline Flag"""
    def __init__(self):
        self.kind = 'endline'
        self.chars = '\\' + Macro.MacroEndline.chars

class PatnCommit(Mtoken):
    """ Pattern Commit Flag"""
    def __init__(self):
        self.kind = 'commit'
        self.chars = '\\' + Macro.MacroCommit.chars

class PatnDedent(Mtoken):
    """ Dedent delimiter"""
    def __init__(self):
        self.kind = 'dedent'
        self.chars = '\\' + Macro.MacroDedent.chars

class TokenSeq:
    """Token sequence."""
    
    def __init__(self, initials=[]):
        self.sequence = deque(initials)

    def empty(self) -> bool:
        return len(self.sequence) == 0
        
    def first(self) -> Mtoken:
        return self.sequence[0]
    
    def read_first(self) -> Mtoken:
        return self.sequence.popleft()

    def skip_first(self):# -> TokenSeq
        self.sequence.popleft()
        return self
        
    def restore_first(self, t: Mtoken):
        self.sequence.appendleft(t)

    def second(self):
        return self.sequence[1]

    def last(self) -> Mtoken:
        return self.sequence[-1]

    def skip_whitespace(self):
        while not self.empty() and type(self.first()) == whitespace:
            self.skip_first()
            
    def trim_whitespace(self):
        while not self.empty() and type(self.last()) == whitespace:
            self.sequence.pop()
    
    def append(self, t: Mtoken):# -> TokenSeq
        self.sequence.append(t)
        return self
    
    def extend(self, tokens):# -> TokenSeq
        self.sequence.extend(tokens.sequence)
        return self

    def display(self):
        for t in self.sequence:
            tokensout.put(t)
            print("\n")

    def show(self):
        for t in self.sequence:
            t.show()

    def string(self, limit=1000):
        s = ""
        for i in range(min(len(self.sequence), limit)):
            s += self.sequence[i].chars
        if limit < len(self.sequence):
            s += " ..."
        return s

    def strpad(self):
        s = ""
        for t in self.sequence:
            s += t.chars + " "
        return s

class Pattern:
    def __init__(self, tokens):
        self.pattern = tokens
    
    def strpad(self):
        return self.pattern.strpad()

class Template:
    def __init__(self, tokens):
        self.template = tokens

    def strpad(self):
        return self.template.strpad()

class Intrinsic (Template):
    """Reference to processor for built-in commands."""
    def __init__(self, fn):
        self.processor = fn

    def strpad(self):
        return "intrinsic"

class Constant (Template):
    """Macro that expands as self."""
    def __init__(self, tokens):
        self.template = tokens

    def strpad(self):
        return "constant"

class Rule:
    def __init__(self, A: Pattern, S: Template):
        self.analysis, self.synthesis = A, S
       
def process():
    """Process input file containing macro definitions and calls."""
    initialize_rules()
    eprint("Synmac v.7 processing text.")
    success, expansion, eof, pos = expand(read_tokens(), endfile(), 0, 0)
    write_tokens(expansion)
    eprint("Done!")
    
def expand(tokens: TokenSeq, delimiter: Mtoken, dedent: int, pos: int) \
    -> (bool, TokenSeq, TokenSeq, int):
    """Expand all macro calls in token sequence (up to delimiter) and return
    resulting token sequence, along with success flag and remainder of tokens.
    Expansion is terminated by specified delimiter outside of
    any macro calls. The delimiter is left in the remainder sequence.
    If the delimiter is the dedent code, then it is considered matched by a line
    whose first nonblank has a position less or equal to the dedent parameter,
    which is the indent level of any macro of which this is an actual.
    The pos parameter is the line position at which the sequence begins.
    Returns success, expanded sequence, remainder of original sequence,
    and updated line position.
    Returns failure if the token sequence ends before the delimiter is
    found.
    """
    tprint(2, "Expanding: " + tokens.string(25)) #####
    tprint(2, "Delimiter = " + delimiter.string()) #####
    tprint('dedent', "Expanding: " + tokens.string(25) + \
           "\nDelimiter = " + delimiter.string()) #####
    expanded = TokenSeq()
    remainder = tokens
    while not remainder.empty(): # fail, no delimiter      
        t = remainder.first()
        while type(t) == whitespace: #copy whitespace
            expanded.append(t)
            remainder.skip_first()
            pos += len(t.chars) # increase position
            t = remainder.first()
        if type(t) == endline:
            pos = 0 # reset position in line
            tprint('dedent', "Indent = 0 for " + remainder.string(25)) #####
            if type(delimiter) == PatnNewline:
                tprint(1, "Expanded: " + expanded.string()) ####
                tprint(2, "Remainder: " + remainder.string(25)) ####
                return (True, expanded, remainder, pos)
            elif type(delimiter) == PatnDedent:
                tprint('dedent', "Looking for " + str(dedent) + " dedent") #####
                indent = find_next_indent(remainder)
                if indent <= dedent: #indent delimitation
                    tprint('dedent', "Dedent " + str(dedent) + " delimitation") ####
                    tprint(1, "Expanded: " + expanded.string()) ####
                    tprint(2, "Remainder: " + remainder.string(25)) ####
                    return (True, expanded, remainder, pos)
                else: #copy non-delimiting newline
                    tprint(4, "No dedent delimitation") ####
                    expanded.append(t)
                    remainder.skip_first()
                    tprint(3, "Partial expansion: " + expanded.string()) ####
                    tprint(3, "Rest of tokens: " + remainder.string(25)) ####
                    t = remainder.first()
                    tprint(3, "Will compare " + t.string() + "to " + delimiter.string()) ####
            else: #copy non-delimiting newline
                expanded.append(t)
                remainder.skip_first()
                tprint(3, "Partial expansion: " + expanded.string()) ####
                tprint(3, "Rest of tokens: " + remainder.string(25)) ####
                t = remainder.first()
                tprint(3, "Will compare " + t.string() + "to " + delimiter.string()) ####
        elif delimiter.equals(t):
            tprint(1, "Expanded: " + expanded.string()) ####
            tprint(2, "Remainder: " + remainder.string(25)) ####
            return (True, expanded, remainder, pos)
        else: #evaluate next expression
            tprint(4, "Next token = " + t.string()) #####
            value, remainder, pos = eval_expr(remainder, pos, pos)
            tprint(3, "Result of evaluation: " + value.string()) ####
            tprint(3, "Rest of tokens: " + remainder.string(25)) ####
            expanded.extend(value)
            tprint(3, "Partial expansion: " + expanded.string()) ####
    tprint(1, "Failure seeking " + delimiter.string() +\
           "in " + remainder.string(25)) ####
    return (False, TokenSeq(), tokens, pos)

def find_next_indent(tokens: TokenSeq) -> int:
    """Determines indent level of next line that is not entirely whitespace
    (if such as line exists, otherwise returns 0)
    """
    pos = 0
    rem = tokens.sequence
    for t in rem:
        if type(t) == endline:
            pos = 0
        elif type(t) == whitespace:
            pos += len(t.chars)
        else:
            tprint('dedent', "Detected indent = " + str(pos))
            return pos
    tprint('dedent', "No detected indent = " + str(pos))
    return pos

def eval_expr(tokens: TokenSeq, dedent: int, pos: int) -> (TokenSeq, TokenSeq, int):
    """Evaluates one expression from the beginning of token sequence.
    
    The expression is either a closed macro call, a quoted token sequence, 
    a string literal, or a single undefined token. Leading whitespace has
    been removed.
    Returns the value of the expression (e.g., an expanded macro) and
    remainder of input token sequence.
    Also accepts and returns line position, which is used for indent-delimited
    parameters. The dedent parameter determines the indent level that will
    delimit these parameters.
    """
    t = tokens.first()
    if type(t) == stringlit:
        tokens.skip_first() # skip the string lit
        return (TokenSeq([t]), tokens, pos + len(t.chars) + 2) #count quotes
    elif Macro.OpenMetaQuote.equals(t):
        raw_tokens, remainder, pos = unevaled_tokens(Macro.CloseMetaQuote, tokens, pos)
        return (raw_tokens, remainder, pos)
    else:
        success, new_tokens, remainder, pos = try_rules(tokens, dedent, pos)
        if success:
            tprint('evalexpr', "Rule application returns: " + new_tokens.strpad())
            tprint('evalexpr', "Remainder: " + remainder.string(25))
            return (new_tokens, remainder, pos)
            if Macro.tracing == 'evalexpr':
                new_tokens.show()
                remainder.show()
        else: # return first token unevaluated
            tokens.skip_first()
            tprint('evalexpr', "Constant token = " + TokenSeq([t]).strpad())
            tprint('evalexpr', "Remainder: " + tokens.string(25))
            if Macro.tracing == 'evalexpr': # What is this for?
                TokenSeq([t]).show()
                tokens.show()
            return (TokenSeq([t]), tokens, pos + len(t.chars))

def try_rules(tokens: TokenSeq, dedent: int, pos: int) -> (bool, TokenSeq, TokenSeq, int):
    """Tries applying all the rules to beginning of token stream.
    Applies first rule (if any) that matches.
    Accepts source token sequence, indent level that will be used for delimiting an expression
    sequence delimited by a dedent, and beginning line position.
    Returns success flag, original or expanded sequence, remainder, and updated line position.
    """
    rules = Macro.intrinsic_rules + Macro.defined_rules
    for R in rules:
        tprint('tryrule', "Trying rule: " + R.analysis.pattern.strpad()) ####
        success, expansion, remainder, pos = try_apply(R, tokens, dedent, pos)
        if success:
            tprint('tryrule', "Success in try_rules, expansion: " + expansion.strpad()) ####
            tprint('tryrule', "Try rules, remainder:" + remainder.string(25)) ####
            return (True, expansion, remainder, pos)
    tprint('tryrule', "No success") ####
    return (False, TokenSeq(), tokens, pos)

def try_apply(rule: Rule, tokens: TokenSeq, dedent: int, pos: int) \
    -> (bool, TokenSeq, TokenSeq, int):
    """Attempts to apply rule to beginning of token sequence.
    Accepts rule, source token sequence, indent level that delimits the expression
    sequence of which this is part, and beginning line position.
    Returns (success, expanded tokens, remaining tokens, new line position)
    """
    success, bindings, remainder, pos = try_parse(rule.analysis.pattern, tokens, dedent, pos)
    if success:
        synthesis = rule.synthesis
        tprint('tryapply',"Successful parse: " + rule.analysis.pattern.strpad()) ######
        tprint('tryapply', "Identified synthesis: " + synthesis.strpad()) ######
        if type(synthesis) == Intrinsic:
            tprint('tryapply', "Intrinsic found: " + rule.analysis.pattern.strpad()) ####
            tprint('tryapply', "Remainder: " + remainder.string(25)) ######
            return (True, synthesis.processor(bindings), remainder, pos)
        elif type(synthesis) == Constant:
            tprint('tryapply', "Constant found: " + rule.analysis.pattern.strpad()) ####
            return (True, subst(bindings, synthesis.template), remainder, pos)
        else:
            tprint('macros', "Text: " + tokens.string(20)) #####
            tprint('macros', "Matches: " + rule.analysis.pattern.strpad()) #####
            return (True, apply(bindings, synthesis.template), remainder, pos)
    else:
        return (False, TokenSeq(), tokens, pos)

def try_parse(pattern: Pattern, tokens: TokenSeq, dedent: int, pos: int) \
    -> (bool, dict, TokenSeq, int):
    """Attempts to match pattern to beginning of token sequence.
    Accepts pattern, token stream, dedent, and line position.
    The dedent parameter is the indent level that will delimit the expression
    sequence of which this is part.
    Returns (success, bindings, remaining tokens, updated line position).
    Success of the parse is indicated by success. If the parse was successful,
    then bindings map formals to actuals.
    """
    rest_pattern = TokenSeq(pattern.sequence)
    remainder = TokenSeq(tokens.sequence)
    bindings = dict()
    delimiter = 1
    commit = False
    if Macro.BTwarning > 0: orig_length = len(remainder.sequence)
    while len(rest_pattern.sequence) > 0:
        element = rest_pattern.read_first()
        success, commitflag, bindings, remainder, pos = \
          match(element, rest_pattern, bindings, remainder, dedent, pos)
        if commitflag: commit = True
        if not success:
            tprint(4, "Unsuccessful match: " + element.chars + ", Rest = " + tokens.string()) #####
            if commit and delimiter > 1:
                eprint("*** Syntax error in macro {" + pattern.strpad() + "}")
                eprint("*** Text: " + tokens.string(25))
            tprint(3, "Backtracking from seeking '" + element.chars + "' in '" +\
                   pattern.strpad() + "',\n        rest: " + tokens.string(25)) ####
            if Macro.BTwarning > 0 and\
              orig_length - len(remainder.sequence) > Macro.BTwarning:
                eprint("*** Excessive backtrack warning in macro {" + pattern.strpad() + "}")
                eprint("***   In text: " + tokens.string(25))
            return (False, None, tokens, pos)
        tprint(3, "Successful match: " + element.chars + ", Rest = " + remainder.string(10)) #####
        tprint(4, "Length of remaining pattern = " + str(len(pattern.sequence))) ####
        delimiter += 1
    tprint('dedent',
           "Successful parse with indent " + str(dedent) + \
           " of " + pattern.string(10) + "\n rest: " + remainder.string(50))
    return (True, bindings, remainder, pos)

def match(element: Mtoken, pattern: TokenSeq, bindings: dict, tokens: TokenSeq, \
          dedent: int, pos: int) -> (bool, bool, dict, TokenSeq, int):
    """Matches pattern element against token stream.
        
    Accepts pattern element, rest of pattern, param bindings, token stream,
    indent level, and line position.
    Returns success flag, commit flag, and updated param bindings, token stream,
    and line position.
    Consumes whatever matches the pattern element (except for endlines and dedents).
    A newline element matches and consumes a newline. An endline element matches a
    newline but doesn't consume it (so it can delimit several macros).
    A dedent element is matched if next indent level is less than or equal to
    the dedent parameter (representing the indent level of the current expression).
    No text is consumed, since that indent level may delimit several parameters/macros.
    If the element is the parameter flag, then the next pattern element is read
    as a parameter name, which is bound to the following expanded token
    sequence. Updated parmeter bindings are returned.
    """
    tprint('match', "Looking for element: " + element.string()) ####
    tprint('match', "First token = " + tokens.first().string()) ####
    if type(element) == PatnParameter: # process large parameter
        formal = pattern.read_first()
        tprint('match', "Formal name = &" + formal.string()) ####
        delimiter = pattern.first() 
        tprint('match', "Delimiter = " + delimiter.string()) ####
        # bind formal to following expanded sequence (actual)
        tprint('match', "Potential actual: " + tokens.string(50)) ####
        success, actual, remainder, pos = expand(tokens, delimiter, dedent, pos)
        if not success:
            tprint('match', "Failure to find delimiter " + delimiter.string()) ####
            return (False, False, None, tokens, pos)
        tprint('match', "Actual: " + actual.string(50)) #####
        tprint('match', "Remainder: " + remainder.string(50)) ####
        bindings[formal.chars] = actual
        tprint('match', "Value of formal = " + bindings[formal.chars].string(50)) ######
        return (True, False, bindings, remainder, pos)
    elif type(element) == PatnParameterTrimmed: # process primary parameter
        formal = pattern.read_first()
        tprint('match', "Formal name = ~" + formal.string()) ####
        # bind formal to following expanded sequence (actual)
        tokens.skip_whitespace()
        tprint('match', "Potential actual: " + tokens.string(50)) ####
        actual, remainder, pos = eval_expr(tokens, dedent, pos)
        #actual.skip_whitespace() # skip leading whitespace
        #actual.trim_whitespace() # trim trailing whitespace
        tprint('match', "Actual: " + actual.string(50)) #####
        tprint('match', "Remainder: " + remainder.string(50)) ####
        bindings[formal.chars] = actual
        tprint('match', "Value of formal = " + bindings[formal.chars].string(50)) ######
        return (True, False, bindings, remainder, pos)
    elif type(element) == PatnParameterQuoted: # process quoted parameter
        formal = pattern.read_first()
        tprint('match', "Formal name = '" + formal.string()) ####
        delimiter = pattern.first()
        tprint('match', "Delimiter = " + delimiter.string()) ####
        # bind formal to following unevaluated sequence (actual)
        success, raw_tokens, remainder, pos = unevaled_actual(delimiter, tokens, dedent, pos)
        if not success:
            tprint('match', "Failure to find delimiter " + delimiter.string()) ####
            return (False, False, None, tokens, pos)
        tprint('match', "Actual: " + raw_tokens.string(50)) #####
        tprint('match', "Remainder: " + remainder.string(50)) ####
        bindings[formal.chars] = raw_tokens
        tprint('match', "Value of formal = " + bindings[formal.chars].string(50)) ######
        return (True, False, bindings, remainder, pos)
    else:
        # skip any whitespace (is this needed? Yes!)
        while not tokens.empty() and type(tokens.first()) == whitespace:
            pos += len(tokens.first().chars)
            tokens.skip_first()
        tprint('match', "Next after whitespace = " + tokens.first().string()) ####
        if type(element) == PatnNewline and type(tokens.first()) == endline:
            return (True, False, bindings, tokens.skip_first(), 0)
        elif type(element) == PatnEndline and type(tokens.first()) == endline:
            return (True, False, bindings, tokens, 0)
        elif type(element) == PatnDedent:
            tprint('dedent', "Finding indent in " + tokens.string(50)) ####
            indent = find_next_indent(tokens)
            tprint('dedent', "Found " + str(indent) + " vs. " + str(dedent))
            if indent <= dedent:
                tprint('dedent', "Dedent delimitation") ####
                return (True, False, bindings, tokens, pos)
        elif type(element) == PatnCommit:
            return (True, True, bindings, tokens, pos)
        elif element.equals(tokens.first()):
            pos += len(tokens.first().chars)
            return (True, False, bindings, tokens.skip_first(), pos)
        else:
            return (False, False, bindings, tokens, pos)

def apply(bindings: dict, template: TokenSeq) -> TokenSeq:
    """Expands template after applying bindings of formals to actuals.
    Accepts bindings, macro body template.
    Returns evaluated macro body.
    """
    body = subst(bindings, template)
    body.append(endfile())
    #Evaluate body as though begins on new line
    success, result, eof, pos = expand(body, endfile(), 0, 0)
    tprint(1, "Expanded body: " + result.string())
    tprint('macros', "Yields: " + result.string(20))
    return result

def subst(bindings: dict, template: TokenSeq) -> TokenSeq:
    """Substitutes actual parameters for formal parameters."""
    body = TokenSeq()
    for t in template.sequence:
        if type(t) == word and t.chars in bindings:
            body.extend(bindings[t.chars])
        else:
            body.append(t)
    tprint(2, "Substituted template: " + body.string()) ####
    return body

def unevaled_tokens(delim: Mtoken, tokens: TokenSeq, pos: int) -> (TokenSeq, TokenSeq, int):
    """Accept tokens up to specified delimiter and assemble into token
    sequence. The open meta quote is still in the input token stream.
    The open and close meta quotes are deleted from the result, but it preserves embedded,
    matched meta quotes.
    """
    s = TokenSeq()
    tokens.skip_first() # opening metaquote
    while not tokens.empty():
        t = tokens.first()  # peek at first token
        if delim.equals(t):
            pos += len(t.chars)
            tokens.skip_first() # skip close quote
            return (s, tokens, pos)
        elif t.equals(Macro.OpenMetaQuote): #process embedded quotation
            pos += len(t.chars)
            # extract sequence between embedded quotes
            raw_tokens, remainder, pos = unevaled_tokens(Macro.CloseMetaQuote, tokens, pos)
            s.append(Macro.OpenMetaQuote) # restore open quote
            s.extend(raw_tokens) # return interior of quotation
            s.append(Macro.CloseMetaQuote) # restore close quote
            pos += len(Macro.CloseMetaQuote.chars)
            tokens = remainder # continue processing the token sequence
        elif type(t) == endline:
            pos = 0
            s.append(t)
            tokens.skip_first()
        else: # ordinary, unquoted token
            s.append(t)
            pos += len(t.chars)
            tokens.skip_first()
    eprint("*** Unterminated metaquotation, begins: " + s.string(50))
    return (s, tokens, pos)

def unevaled_actual(delim: Mtoken, tokens: TokenSeq, dedent: int, pos: int) \
    -> (bool, TokenSeq, TokenSeq, int):
    """Accept tokens up to specified delimiter and assemble into token
        sequence. Return boolean indicator if delimiter is found
        (if not, it may trigger bactracking in caller). Leave delimiter
        in the remainder. Preserve embedded, matched meta quotes.
        """
    s = TokenSeq()
    while not tokens.empty():
        t = tokens.first()  # peek at first token
        if delim.equals(t) or type(delim) == PatnNewline and type(t) == endline:
            return (True, s, tokens, pos)
        elif type(delim) == PatnDedent and type(t) == endline:
            indent = find_next_indent(tokens)
            if indent <= dedent:
                tprint(4, "Dedent delimitation") ####
                return (True, s, tokens, pos)
            else:
                s.append(t)
                pos = 0
                tokens.skip_first()
        elif t.equals(Macro.OpenMetaQuote): #process embedded quotation
            # extract sequence between embedded quotes
            raw_tokens, remainder, pos = unevaled_tokens(Macro.CloseMetaQuote, tokens, pos)
            s.append(Macro.OpenMetaQuote) # restore open quote
            s.extend(raw_tokens) # return interior of quotation
            s.append(Macro.CloseMetaQuote) # restore close quote
            pos += len(Macro.OpenMetaQuote.chars) + len(Macro.CloseMetaQuote.chars)
            tokens = remainder # continue processing the token sequence
        else: # ordinary, unquoted token
            s.append(t)
            pos += len(t.chars)
            tokens.skip_first()
    #eprint("*** Undelimited unevaluated actual, begins: " + s.string(50))
    return (False, s, tokens, pos)

def read_tokens(fn = '') -> TokenSeq:
    """Read specified input file (assumed stdin) into a token sequence.
    """
    s = TokenSeq()
    inp = tokensin(fn)
    inp.start()
    t = inp.next()
    while not (type(t) == endfile or (type(t) == word and t.chars == 'eof')):
        s.append(t)
        t = inp.next()
#    inp.close() #### omit for the sake of pause command
    return s.append(endfile())
    
def write_tokens(tokens: TokenSeq):
    """Write token sequence to output file."""
    for t in tokens.sequence:
        tokensout.put(t)

def initialize_rules():
    """Initialize intrinsic rule table."""
    Macro.intrinsic_rules = []
    Macro.intrinsic_rules.append(Rule(
        Pattern(TokenSeq([
            Macro.MacroDefine, PatnParameter(), word('pattern'),
            Macro.MacroDefineAs, PatnParameter(), word('template'),
            Macro.MacroDefineEnd])),
        Intrinsic(define_macro)))
    Macro.intrinsic_rules.append(Rule(
        Pattern(TokenSeq([
            Macro.MacroDeclare, PatnParameter(), word('pattern'),
            Macro.MacroDeclareEnd])),
        Intrinsic(define_constant_macro)))
    Macro.intrinsic_rules.append(Rule(
        Pattern(TokenSeq([
            Macro.MacroExpand, PatnParameter(), word('parameter'),
            Macro.MacroEndExpand])),
        Intrinsic(expand_actual)))
    Macro.intrinsic_rules.append(Rule(
            Pattern(TokenSeq([
                    Macro.MacroFlag, Macro.Include,
                    PatnParameter(), word('filename'),
                    PatnNewline()])),
            Intrinsic(include_file)))
    Macro.intrinsic_rules.append(Rule(
            Pattern(TokenSeq([
                    Macro.MacroFlag, Macro.MacroTrim,
                    PatnParameter(), word('text'),
                    Macro.MacroEndTrim])),
            Intrinsic(trim_actual)))
    Macro.intrinsic_rules.append(Rule(
            Pattern(TokenSeq([Macro.MacroFlag, word('endline')])),
            Intrinsic(end_the_line)))
    make_intrinsic_rule('metaquotes', set_metaquotes)
    make_intrinsic_rule('syntax', set_syntax_keywords)
    make_intrinsic_rule('pattern', set_pattern_keywords)
    make_intrinsic_rule('long', set_long)
    make_intrinsic_rule('short', set_short)
    make_intrinsic_rule('uneval', set_uneval)
    make_intrinsic_rule('command', set_command)
    make_intrinsic_rule('newline', set_newline)
    make_intrinsic_rule('dedent', set_dedent)
    make_intrinsic_rule('commit', set_commit)
    make_intrinsic_rule('alpha', set_extra_alpha)
    make_intrinsic_rule('trace', set_tracing)
    make_intrinsic_rule('backtrack', set_backtrack_warning)
    make_intrinsic_rule('definitions', set_rule_dump)
    make_intrinsic_rule('message', set_message)
    make_intrinsic_rule('pause', set_pause)
    eprint("Initialization complete.")

""" All the intrinsic command processors return a TokenSeq (often empty) """

def define_macro(binding: dict) -> TokenSeq:
    """Process a macro definition."""
    success, pattern = process_pattern(binding['pattern'])
    if not success:
        eprint("*** Illegal macro pattern definition: ")
        eprint(binding['pattern'].strpad())
        return TokenSeq()
    Macro.defined_rules.insert(0,
                               Rule(
        Pattern(pattern),Template(binding['template'])))
    if Macro.tracing.isdigit() and int(Macro.tracing) >= 1:
        eprint("Rule defined: " + pattern.strpad()) ####
        eprint(" => " + binding['template'].strpad()) ####
    if Macro.rule_dump: dump_rules()
    return TokenSeq()

def define_constant_macro(binding: dict) -> TokenSeq:
    """Process a constant macro definition."""
    success, pattern = process_pattern(binding['pattern'])
    if not success:
        eprint("*** Illegal macro pattern definition: ")
        eprint(binding['pattern'].strpad())
        return TokenSeq()
    Macro.defined_rules.insert(0, Rule(Pattern(pattern),
                                       Constant(TfromP(pattern))))
    tprint('defconst', "Constant rule defined: " + pattern.strpad()) ####
    if Macro.rule_dump: dump_rules()
    return TokenSeq()

def TfromP(pattern: TokenSeq) -> Template:
    """Make a template from a pattern by eliminating parameter and commit
    flags and converting newline elements.
    """
    ts = TokenSeq()
    for t in pattern.sequence:
        if not (type(t) == PatnParameter or \
                type(t) == PatnParameterTrimmed or \
                type(t) == PatnParameterQuoted or \
                type(t) == PatnCommit):
            if type(t) == PatnNewline or type(t) == PatnDedent:
                ts.append(endline())
            else:
                ts.append(t)
    tprint('defconst', "Generated template: " + ts.strpad()) ####
    return ts

def process_pattern(in_pattern: TokenSeq) -> (bool, TokenSeq):
    """Process pattern to ensure in correct format.

    Pattern must begin and end with delimiters (ignoring whitespace),
    unless it ends with a primary (short, ~) formal.
    A parameter flag must be followed by a word (ignoring whitespace),
    and parameters must be separated by non-parameter words. This doesn't
    check for illegally redefining meta characters, system macros, etc.
    Pattern flags are converted to internal pattern elements.
    """
    pattern = remove_whitespace(in_pattern)
    # ensure initial delimiters
    delimited, good_pattern = required_delimiters(pattern)
    if not delimited:
        in_pattern.show()
        return illegal_pattern("No initial delimiter.", in_pattern)
    #check and convert remaining pattern elements
    while not pattern.empty():
        # delimiters have been processed, so should be parameter
        t = pattern.read_first()
        if Macro.MacroParameter.equals(t):
            good_pattern.append(PatnParameter())
        elif Macro.MacroParameterTrimmed.equals(t):
            good_pattern.append(PatnParameterTrimmed())
        elif Macro.MacroParameterQuoted.equals(t):
            good_pattern.append(PatnParameterQuoted())
        else:
            pattern.show()
            return illegal_pattern("Missing parameter flag: ", in_pattern)
        if pattern.empty():
            return illegal_pattern(
                "Missing parameter name after parameter flag: ", in_pattern)
        else: # append formal name
            good_pattern.append(pattern.read_first())
        # check for another delimiter sequence
        delimited, delimiters = required_delimiters(pattern)
        if not delimited: # allow final short parameter
            if type(good_pattern.sequence[-2]) == PatnParameterTrimmed:
                return (True, good_pattern)
            else:
                return illegal_pattern(
                        "No delimiters after parameter: ", in_pattern)
        good_pattern.extend(delimiters)
    return (True, good_pattern)

def remove_whitespace(tokens: TokenSeq) -> TokenSeq:
    """ Removes whitespace from a token sequence. The input sequence is
    consumed.
    """
    visible_tokens = TokenSeq()
    while not tokens.empty():
        if type(tokens.first()) != whitespace \
          and type(tokens.first()) != endline:
            visible_tokens.append(tokens.first())
        tokens.skip_first()
    return visible_tokens

def required_delimiters(pattern: TokenSeq) -> (bool, TokenSeq):
    """ Checks for sequence of one or more delimiters. Returns bool indicating
    required delimiters are present and sequence of converted delimiters.
    """
    delimiters = TokenSeq()
    if pattern.empty():
        return (False, None)
    t = pattern.read_first()
    # check for at least one delimiter and convert
    if type(t) != word or \
      t.equals(Macro.MacroParameter) or \
      t.equals(Macro.MacroParameterTrimmed) or \
      t.equals(Macro.MacroParameterQuoted):
          return (False, None) # not a delimiter
    delimiters.append(convert_pattern_element(t))
    # convert remaining cosecutive delimiters
    while not pattern.empty(): #convert consecutive delimiters
        t = pattern.first()
        if type(t) != word or \
          t.equals(Macro.MacroParameter) or \
          t.equals(Macro.MacroParameterTrimmed) or \
          t.equals(Macro.MacroParameterQuoted):
            break
        delimiters.append(convert_pattern_element(t))
        pattern.skip_first()
    return (True, delimiters)

def convert_pattern_element(t: Mtoken) -> Mtoken:
    if t.equals(Macro.MacroNewline):
        return PatnNewline() # convert newline flag
    elif t.equals(Macro.MacroEndline):
        return PatnEndline() # convert endline flag
    elif t.equals(Macro.MacroDedent):
        return PatnDedent()  # convert dedent flag
    elif t.equals(Macro.MacroCommit):
        return PatnCommit()  # convert commit flag
    else:
        return t             # ordinary delimiters
    

def illegal_pattern(diagnostic: str, pattern: TokenSeq) -> (bool, TokenSeq):
    eprint("*** " + diagnostic)
    eprint("  {" + pattern.strpad() + "}")
    return (False, pattern)

def expand_actual(bindings) -> TokenSeq:
    """Expand actual parameter (applying any embedded macros)."""
    tprint(3, "Expanding: " + bindings['parameter'].string()) ####
    #ts = [t for t in bindings['parameter'].sequence
    #      if not t.equals(Macro.OpenMetaQuote)
    #      and not t.equals(Macro.CloseMetaQuote)]
    ts = bindings['parameter'].sequence
    success, expanded, eof, pos = expand(TokenSeq(ts).append(endfile()),
                           endfile(), 0, 0)
    tprint(3, "Expanded: " + expanded.string()) ####
    return expanded
    
def include_file(binding: dict) -> TokenSeq:
    actual = binding['filename']
    actual.skip_whitespace()
    filename = actual.first().chars
    success, expansion, eof, pos = expand(read_tokens(filename), endfile(), 0, 0)
    write_tokens(expansion)
    return TokenSeq()
                                 
def trim_actual(binding: dict) -> TokenSeq:
    actual = binding['text']
    actual.skip_whitespace()
    actual.trim_whitespace()
    return actual

def make_intrinsic_rule(keyword: str, processor):
    Macro.intrinsic_rules.append(Rule(
        Pattern(TokenSeq([
            Macro.MacroFlag, Macro.MacroCommand, 
            word(keyword), PatnParameter(),
            word('parameter'), PatnNewline()])),
        Intrinsic(processor)))

### on following, need to take first nonblank token of parameter
    
def set_metaquotes(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    original_actual = TokenSeq(actual.sequence.copy())
    keywords = remove_whitespace(actual).sequence
    if len(keywords) != 2:
        eprint("*** Two delimiters required for metaquotes:",
              original_actual.strpad())
    else:
        Macro.OpenMetaQuote = keywords[0]
        Macro.CloseMetaQuote = keywords[1]
        initialize_rules() # rebuild intrinsic rule table
    return TokenSeq()

def set_syntax_keywords(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    original_actual = TokenSeq(actual.sequence.copy())
    keywords = remove_whitespace(actual).sequence
    if len(keywords) != 3:
        eprint("*** Three delimiters required for syntax:",
              original_actual.strpad())
    else:
        Macro.MacroDefine = convert_newline(keywords[0])
        Macro.MacroDefineAs = convert_newline(keywords[1])
        Macro.MacroDefineEnd = convert_newline(keywords[2])
        initialize_rules() # rebuild rule table
    return TokenSeq()

def set_pattern_keywords(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    original_actual = TokenSeq(actual.sequence.copy())
    keywords = remove_whitespace(actual).sequence
    if len(keywords) != 2:
        eprint("*** Two delimiters required for pattern:",
              original_actual.strpad())
    else:
        Macro.MacroDeclare = convert_newline(keywords[0])
        Macro.MacroDeclareEnd = convert_newline(keywords[1])
        initialize_rules() # rebuild rule table
    return TokenSeq()

def convert_newline(t: Mtoken) -> Mtoken:
    if t.equals(Macro.MacroNewline):
        return PatnNewline() # convert newline flag
    else:
        return t

def set_long(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    Macro.MacroParameter = actual.first()
    initialize_rules() # rebuild intrinsic rule table
    return TokenSeq()

def set_short(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    Macro.MacroParameterTrimmed = actual.first()
    initialize_rules() # rebuild intrinsic rule table
    return TokenSeq()

def set_uneval(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    Macro.MacroParameterQuoted = actual.first()
    initialize_rules() # rebuild intrinsic rule table
    return TokenSeq()

def set_command(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    Macro.MacroFlag = actual.first()
    initialize_rules() # rebuild intrinsic rule table
    return TokenSeq()

def set_newline(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    Macro.MacroNewline = actual.first()
    initialize_rules() # rebuild intrinsic rule table
    return TokenSeq()

def set_dedent(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    Macro.MacroDedent = actual.first()
    initialize_rules() # rebuild intrinsic rule table
    return TokenSeq()

def set_commit(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    Macro.MacroCommit = actual.first()
    initialize_rules() # rebuild intrinsic rule table
    return TokenSeq()

def set_tracing(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    flag = actual.first()
    #if flag.chars.isdigit():
    #    Macro.tracing = int(flag.chars)
    #    print("Trace level is {0}.".format(Macro.tracing))
    #else:
    #    print("*** Illegal trace option '" + flag.chars +
    #          "', which should be an integer.")
    Macro.tracing = flag.chars
    return TokenSeq()

def set_rule_dump(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    flag = actual.first()
    Macro.rule_dump = flag.chars == 'on'
    return TokenSeq()

def set_message(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    print(">>>", actual.first().chars)
    return TokenSeq()

def set_pause(bindings: dict) -> TokenSeq:
    eprint("*** Paused at", bindings['parameter'].strpad())
    eprint("*** Enter anything to continue.")
    sys.stdin.readline()
    return TokenSeq()

def end_the_line(bindings: dict) -> TokenSeq:
    return TokenSeq([endline()])

def set_backtrack_warning(bindings: dict) -> TokenSeq:
    actual = bindings['parameter']
    actual.skip_whitespace()
    level = actual.first().chars
    if level.isdigit():
        Macro.BTwarning = int(level)
        eprint("Backtrack warning is {0}.".format(Macro.BTwarning))
    else:
        eprint("*** Illegal backtrack warning limit '" + level +
              "', which should be an integer.")
    return TokenSeq()

def dump_rules() -> TokenSeq:
    eprint("Macros:")
    for R in Macro.intrinsic_rules + Macro.defined_rules:
        eprint("Syntax {" + R.analysis.strpad() + "}")
        eprint(" => {" + R.synthesis.strpad() +"}")
    return TokenSeq()

def tprint(topic, output: str):
    """Print tracing information conditional on tracing flag."""
    if Macro.tracing == topic:
        eprint(">>"  + output)
    elif Macro.tracing.isdigit() and type(topic) == int and \
      topic <= int(Macro.tracing):
          eprint(">>" + "  " * topic + output)

def eprint(*args, **kwargs):
    """Print to stderr"""
    print(*args, file=sys.stderr, **kwargs)

process()
