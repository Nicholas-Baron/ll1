# LL1 Checker

An LL1 grammar checker

## Motivation

- Bison is an old, C-based, LR checker/parser generator
  + Its syntax is simple enough and easy to teach
  + It lacks creature comforts of EBNF

[Prior Work](https://github.com/geekysuavo/ll1)

## Usage

`cargo run -- example_grammar.txt`

## Syntax

There are only 3 kinds of statements for this program.
All end in semicolons for clarity.

### Start rule
The start rule must be explicitly declared.
Only one start rule may be declared in a file.

Example: `start start_rule;`

### Terminals
Terminals are explicitly listed in a terminal statement.

Example: `terminal terminal1 terminal2;`

### Rules
Rules follow Bison's syntax with the added support for EBNF.

| Syntax | Meaning |
| ------ | ------- |
| `%empty` | Empty rule (mandatory) |
| `item1 item2` | A sequence of 2 items |
| `a | b` | Item a or Item b |
| `[ contents ]` | optional (0 or 1) contents |
| `{ contents }` | repetition (1 or more) contents |

Example: `rule_name : option1 | item1 item2 | [ item3 item4 ];`

### Comments
Comments may start with either `//` or `#` and terminate at the newline, as they are *not* statements.
