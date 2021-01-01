
op_prec = {
    "-" : 2,
    "+" : 2,
    "/" : 3,
    "*" : 3,
    "^" : 4
}

def is_left_assoc(op):
    if op == "^":
        return False
    else:
        return True

symbols = ["*", "+", "-", "/", "^", "(", ")"]

def tokenize(input_str):
    tokens = []
    input_q = list(input_str)
    currentnum = 0
    while input_q:
        tok = input_q.pop(0)
        if (tok in symbols and currentnum != 0):
            tokens.append(currentnum)
            tokens.append(tok)
            currentnum = 0
        elif (tok in symbols):
            tokens.append(tok)
        else:
            currentnum = currentnum * 10 + int(tok)
    return tokens

def op_check(op_stack, op):
    if not op_stack: 
        return False
    topop = op_stack[-1]
    op_p = op_prec.get(op)
    top_p = op_prec.get(topop)
    return ((topop != "(") and ((top_p > op_p) or (top_p == op_p and is_left_assoc(op))))

def shunting_yard(input_str):
    tokens = tokenize(input_str)
    op_stack = []
    output_q = []
    while tokens:
        tok = tokens.pop(0)
        if (isinstance(tok, int)):
            output_q.append(tok)
        elif (tok in op_prec.keys()):
            while (op_check(op_stack, tok)):
                output_q.append(op_stack.pop())
            op_stack.append(tok)
        elif (tok == "("):
            op_stack.append(tok)
        elif (tok == ")"):
            while (op_stack[-1] != "("):
                output_q.append(op_stack.pop())
            if (op_stack[-1] == "("):
                op_stack.pop()
    while op_stack:
        output_q.append(op_stack.pop())
    return output_q

def main():
    eq = input("Enter valid equation: ")
    output = shunting_yard(eq)
    print(output)

if __name__ == "__main__":
    main()
