# CombinatorC
A C-style programming language that compiles into Factorio combinator blueprints. There have been similar projects such as [Factoriogen](https://github.com/Jobarion/factoriogen), but I have a significantly different vision for CombinatorC.

I intend to create a language that is useful for designing complex circuits that are actually used for real Factorio gameplay, and not just as an academic curiosity. My original plan was to create a C-style language, but as development continues, it is likely that the language will change into something that more resembles something like Verilog in order to make it more practical. As such, I might the change the name, stay tuned! 

The compiler is built in OCaml, and uses [Dune](https://dune.build/)

Executables for Ubuntu, Mac, and Windows can be found in the releases section of the repository. 

To build from source, install dependencies using:

`opam install . --deps-only`

Then, run `dune build`. 

## Usage 

Simply supply the file name containing the CombinatorC program as a command line argument. The compiler will print a Factorio blueprint string to `stdout`.

For example, on Windows:

`combc-windows.exe input.txt`

Optionally, you can set the flag `--output-json` to output a JSON string representing the blueprint instead of a Factorio blueprint string. 

The output blueprint will have the name of the input file.

## Language 

A CombinatorC program starts with optional compiler directives, and is followed by an optional list circuit bindings that are terminated by semicolons. A final output circuit is required, which can either be a circuit binding or an expression. This should not be terminated by a semicolon.  

Programs will be compiled into a set of circuits, each including an input pole and output pole. Wire the intended circuit inputs to the input pole. 

Comments can be writted using `\\`.

### Signals 

Currently supported signals are the capital letters, for example `A`. Signals are used to set the output of a circuit binding, and also can be used in expressions. Support for more signals and arbitrary temporary variable naming is planned. 

### Expressions

An expression is either: 

- A signal (e.g. `B`)
- An integer literal (e.g. `10`, `0`, `-1234`)
- A boolean literal `true` or `false`  (these are converted to integer literals `1` and `0` respectively)
- A mathematical expression composed using operators

Signals used in expressions are interpreted as input signals, and the resulting circuit will include a constant combinator wired to the input pole that initially sets every signal to `1`. 

Programs that do not use circuit bindings and instead consist of a single expression will use the checkmark output signal. 

#### Operators

There are two types of operators, numeric operators and boolean operators. 

Numeric operators compute mathematical results as expected. Supported operations: `+`,`-`,`*`,`/`,`%`, `**`, `<<`,`>>`,`|`,`&`,`^`. Note: `**` is exponentiation, and `^` is bitwise XOR. 

Here is an example numeric expression: `A + (B - 4) ** (3 >> 6 * C)`

Boolean operators compute a boolean result `1` or `0`. Supported operations: `&&`,`||`,`!`.

Here is an example boolean expression: `(A && B) || !C`

Numeric operators and boolean operators can be combined arbitrarily, for example:

`10 + ((A || B) && (C % 2)) + !5`

This is because boolean operators produce either `1` or `0`, and input values `0` are interpreted as `false`, while any other input is interpreted as `true`. 

Note that this means `!!<exp>` is not necessarily equal to `<exp>`, since the first NOT operator will output either `0` or `1`. This means the expression `!!<exp>` can be used to yield the truth value `1` or `0` from an expression. 

Precedence of operators adheres to the standards of C++. 

### Circuit Bindings 

A circuit binding has the syntax `circuit <output_signal> = <expression>`. If your program has only a single circuit, it should not be terminated by a semicolon. Here is an example circuit binding: 

`circuit D = (A + B - C) / 45`

This produces a circuit with the output signal `D`. 


### Compiler Directives 

A compiler directive has the format `#<DIRECTIVE> <ARG>`. Directives and arguments should either be all uppercase, or all lowercase. Arguments are specific to a given directive. 

Currently, the only supported directive is `LAYOUT`, which determines how circuits will be laid out on the Factorio grid. `LAYOUT` takes either the argument `IDENTITY` or `NAIVE`. 

`IDENTITY` simply places the circuits in a line, wrapping around to the start if necessary, forming a rectangle. This is the default behavior. 

`NAIVE` randomly places each combinator in a position that allows it to connect to all of its necessary inputs and outputs. Not recommended. 

A more sophisticated layout strategy using simulated annealing is planned for future versions. 

### More Details

More details on the grammar can be found by looking at [parser.mly](src/parser/parser.mly).

## Example Program 

Here is an example program:

    #LAYOUT IDENTITY
    
    circuit D = 10 + ((A || B) && (C % 2)) + !5;

    \\ This is a comment

    circuit E = 45 + D;

    (65 + 12) >> 2


This program will layout the circuits using the `IDENTITY` strategy (this line is unncessary, because this is the default behavior). It will produce three distinct circuits:
- The first circuit has output signal `D`, and takes inputs `A`, `B`, and `C`. 
- The second circuit has output signal `E`, and takes input `D`. **This is not the same signal `D` as the output of the first circuit, it is a separate input and not related to the first.**
- The third circuit has output signal checkmark, and takes no inputs. 

Note that if you intend a circuit binding to be the last output, you must write out the whole binding expression, and not just the output signal. For example:

    circuit D = 10 + ((A || B) && (C % 2)) + !5;
    circuit E = 45 + D

If you instead wrote: 

    circuit D = 10 + ((A || B) && (C % 2)) + !5;
    circuit E = 45 + D;
    E

This would be interpreted as three separate circuits, with the final circuit consisting of just a single expression that is equal to the input signal `E`. 

## Future Plans 

I have a lot of potential ideas for how to extend the language and improve functionality, and would love to hear suggestions from people who design circuits in Factorio. What follows is the list of planned additions to the language and compiler:

- Improved layout strategy using simulated annealing to optimize space used
- Temporary variables 
- Conditional expressions 
- Circuit composition operations, such as union (of the outputs) and concatenation 
- Import of external circuits via a blueprint string
- Built-in useful circuit constructs, such as timers, filters, latches, and memory cells
- Operations with wildcard signals (I don't know what this will look like)
- Much more! Stay tuned.
