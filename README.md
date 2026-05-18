# C Compiler in OCaml

A progressively built C compiler written in OCaml, following a multi-stage compiler architecture from lexical analysis to x86-64 assembly generation.

This project currently supports a subset of C and is being expanded incrementally to model real compiler design principles including recursive descent parsing, intermediate representations (IR), stack-backed code generation, and instruction fixups.

## Current Features

### Frontend
- Lexical analysis (tokenization)
- Recursive descent parser
- Abstract Syntax Tree (AST) construction
- Syntax error detection for malformed programs

### Language Support
Currently supported C subset:

```c
int main(void) {
    return 42;
}
```

Unary expressions:

```c
int main(void) {
    return -2;
}
```

Nested unary expressions:

```c
int main(void) {
    return ~(-2);
}
```

Supported operators:
- Unary negation (`-`)
- Bitwise complement (`~`)

Recognized but intentionally rejected:
- Decrement (`--`)

### Backend
- TACKY intermediate representation (IR)
- AST → TACKY lowering
- TACKY → x86-64 assembly code generation
- Pseudoregister allocation
- Stack slot assignment
- Memory-to-memory instruction fixups
- Assembly emission (AT&T syntax)

---

## Compiler Pipeline

```text
C Source Code
    ↓
Lexer
    ↓
Parser
    ↓
Abstract Syntax Tree (AST)
    ↓
TACKY Intermediate Representation
    ↓
Assembly AST
    ↓
x86-64 Assembly Output
```

Example transformation:

### Input

```c
return ~(-2);
```

### AST

```text
Return
  Unary Complement
    Unary Negate
      Constant 2
```

### TACKY IR

```text
tmp.0 = -2
tmp.1 = ~tmp.0
return tmp.1
```

### Final Assembly

```asm
.globl _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $8, %rsp
    movl $2, -4(%rbp)
    negl -4(%rbp)
    movl -4(%rbp), %r10d
    movl %r10d, -8(%rbp)
    notl -8(%rbp)
    movl -8(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret
```

---

## Project Structure

```text
c-compiler/
│
├── src/
│   ├── ast.ml          # High-level AST definitions
│   ├── lexer.ml        # Lexical analyzer
│   ├── parser.ml       # Recursive descent parser
│   ├── tacky.ml        # Intermediate representation definitions
│   ├── tackygen.ml     # AST → TACKY lowering
│   ├── asm.ml          # Assembly IR definitions
│   ├── codegen.ml      # TACKY → assembly lowering
│   ├── emit.ml         # Assembly text emission
│   └── driver.ml       # Compiler pipeline entrypoint
│
├── tests/
│   ├── return_42.c
│   └── unary.c
│
├── Makefile
└── README.md
```

---

## Build

Requires:
- OCaml
- Make
- Clang/GCC

Compile:

```bash
make
```

Clean build artifacts:

```bash
make clean
```

---

## Usage

Compile a C source file:

```bash
./mycc tests/unary.c
```

Generated assembly:

```bash
cat out.s
```

On Apple Silicon (ARM Macs), run x86 output via:

```bash
clang -arch x86_64 out.s -o unary
./unary
echo $?
```

Expected output:

```bash
1
```

---

## Design Decisions

### Recursive Descent Parsing
Expressions are parsed recursively to support arbitrary nesting:

```c
~(-(~2))
```

### TACKY Intermediate Representation
Direct AST → assembly codegen becomes messy once expressions become nested.

Introducing TACKY separates:
- expression flattening
- hardware-specific instruction generation

Example:

```text
Nested AST:
~(-2)

Flattened TACKY:
tmp.0 = -2
tmp.1 = ~tmp.0
return tmp.1
```

### Pseudoregister Lowering
Temporary variables are initially represented abstractly:

```text
tmp.0
tmp.1
```

Then lowered into real stack locations:

```text
-4(%rbp)
-8(%rbp)
```

### Instruction Fixups
x86 forbids memory-to-memory moves:

Invalid:

```asm
movl -4(%rbp), -8(%rbp)
```

Fixed via scratch register:

```asm
movl -4(%rbp), %r10d
movl %r10d, -8(%rbp)
```

---

## Roadmap

### Completed
- [x] Integer constants
- [x] Return statements
- [x] Unary negation
- [x] Bitwise complement
- [x] Nested unary expressions
- [x] TACKY IR
- [x] Stack-backed temporaries
- [x] Instruction fixups

### In Progress
- [ ] Binary operators
- [ ] Operator precedence parsing
- [ ] Associativity rules
- [ ] Multiplication / division / modulo

### Future
- [ ] Local variables
- [ ] Assignment
- [ ] Comparisons
- [ ] Conditionals
- [ ] Control flow
- [ ] Functions with parameters
- [ ] Function calls
- [ ] Register allocation
- [ ] Optimization passes

---

## Why This Project?

This project is an exercise in understanding real compiler architecture rather than building a toy parser.

Core concepts explored:
- lexical analysis
- recursive descent parsing
- abstract syntax trees
- intermediate representations
- instruction selection
- stack frame management
- architecture-specific code generation

---

## Author

Built by Ritvik Ellendula
