# META: Mini-Erlang Typing Application

Mini-Erlang Typing Application is a static analysis of polymorphic types tool. This tool uses a subset of the CoreErlang language to analyse source code expressions and give type information to the user.

## How to use the tool

These are the bash commands to use the tool:

* **meta&#46;sh**: Runs the tool with a list of options and files to analyse. The working folder of this script is the *tests* folder.
* **build&#46;sh**: Builds the tool using the Erlang compiler command. The compiled BEAM files will be in the folder *./out/production/meta*.
* **generate&#46;sh**: This script generates the Erlang code to parse Mini-Erlang source code files. It will be necessary only when a modification is done in the *minierlang&#46;yrl* file.
* **debugb&#46;sh**: Builds the tool like *build&#46;sh* but adding the debug info needed by the Erlang debugger.
* **debug&#46;sh**: Runs the Erlang debugger to debug the tool if needed.

For example, to build and run the tool you can use this command:

```
clear ; ./build.sh ; ./meta.sh map.txt
```

The main options of the tool are:

* **-h**, **--help**: Shows the help message with the tool's options.
* **-v**, **--version**: Shows the version of the tool.
* **-c**, **--code**: Shows the source code of the files being analysed.

## Mini-Erlang syntax

This follow is the BNF syntax for the Mini-Erlang language:

```
expression  ::= variable
              | literal
              | "fun" "(" variables ")" "->" expression
              | "[" expression "|" expression "]"
              | "{" expressions "}"
              | variable "(" variables ")"
              | "let" variable "=" expression "in" expression
              | "letrec" fundefs "in" expression
              | "case" variable "of" clauses "end"
              | "receive" clauses "after" variable "->" expression

clause      ::= pattern "when" expression "->" expression

pattern     ::= variable
              | literal
              | "[" pattern "|" pattern "]"
              | "{" patterns "}"

lit         ::= ATOM | INTEGER | FLOAT | "[]"

expressions ::= expression | expression "," expressions
variables   ::= variable   | variable "," variables
clauses     ::= clause     | clause "," clauses
patterns    ::= pattern    | pattern "," patterns
```

## Types syntax

In the following table we will see the types shown by the tool:

| Name    | Form                       | Description |
| ------- | -------------------------- | ----------- |
| `PAIRS` | `PAIR; ...`                | Branches of the execution |
| `PAIR`  | `TYPE where COND, ... end` | Type of the branch with constraints |
| `COND`  | `VAR <= TYPE`              | Subseteq constraint |
|         | `LIT <= TYPE`              | Subseteq condition |
|         | `VAR := TYPE`              | Match constraint |
| `TYPE`  | `LIT`                      | Singleton set that represents a literal value |
|         | `VAR`                      | Type variable used in polymorphic types |
|         | `none()`                   | Set with no values |
|         | `any()`                    | Set with all values |
|         | `atom()`                   | Set with all atom values |
|         | `integer()`                | Set with all integer values |
|         | `float()`                  | Set with all float values |
|         | `nelist(TYPE, TYPE)`       | Set of non-empty lists where the first type represents the body and the second the termination |
|         | `{TYPE, ...}`              | Set of tuples where each component has a type |
|         | `TYPE + TYPE`              | The union of types |
|         | `TYPE when COND, ...`      | Type with constraints |
|         | `FUNC; ...`                | Functional type overloaded |
| `FUNC`  | `forall VAR, ...: (TYPE, ...) -> TYPE` | Functional scheme type |

Remember that `[]` is the literal that represents the empty list. Atoms, integer and floats literals are the same used in Erlang.
