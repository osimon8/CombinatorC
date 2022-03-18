# CombinatorC
A C-style programming language that compiles into Factorio combinator blueprints. There have been similar projects such as [Factoriogen](https://github.com/Jobarion/factoriogen), but I have a significantly different vision for CombinatorC.

I intend to create a language that is useful for designing complex circuits that are actually used for real Factorio gameplay, and not just as an academic curiosity. My original plan was to create a C-style language, but as development continues, it is likely that the language will change into something that more resembles something like Verilog in order to make it more practical. As such, I might the change the name, stay tuned! 

The compiler is built in OCaml, and uses [Dune](https://dune.build/).

**Executables for Ubuntu, Mac, and Windows can be found in the releases section of the repository.** 

To build from source, install dependencies using:

    opam install . --deps-only

Then, run `dune build`. 

## Usage 

Simply supply the file name containing the CombinatorC program as a command line argument. The compiler will print a Factorio blueprint string to `stdout`.

For example, on Windows:

    combc-windows.exe input.txt

Optionally, you can set the flag `--output-json` to output a JSON string representing the blueprint instead of a Factorio blueprint string. 

The output blueprint will have the name of the input file.

### Running on Windows

If you encounter an error that states you are missing "zlib1.dll", here is the fix: 

- Download the ZIP file available [here](https://www.dll-files.com/download/7e4b778eb31fdf5a52c7476bb78bdb1d/zlib1.dll.html?c=alFOcnFjelJTMmF1ZjhhV01CK0NZdz09)
- Unzip it, and place "zlib1.dll" into C:\Windows\System32

This error may be possible on other operating systems. If you encounter it, please let me know in the "Issues" tab, and follow the steps for installing dynamically linked libraries on your OS.

### Running on Mac 

By default MacOS will block executables from unknown developers. To get around this, navigate to the install location and run the following commands:

    xattr -d com.apple.quarantine combc-mac
    chmod +x combc-mac

Now, you should be able to run the compiler normally.

## Language 

A CombinatorC program starts with optional compiler directives, and is followed by an optional list of circuit bindings that are terminated by semicolons. A final output circuit is required, which can either be a circuit binding or an expression. This should not be terminated by a semicolon.  

Programs will be compiled into a set of circuits, each including an input pole and output pole. Wire the intended circuit inputs to the input pole. 

Comments can be written using `//`.

### Signals 

Currently supported signals are the capital letters, for example `A`. Signals are used to set the output of a circuit binding, and also can be used in expressions. Support for more signals and arbitrary temporary variable naming is planned. 

### Expressions

An expression is either: 

- A signal (e.g. `B`)
- An integer literal (e.g. `10`, `0`, `-1234`)
- A boolean literal `true` or `false`  (these are converted to integer literals `1` and `0` respectively)
- A mathematical expression composed using operators
- A conditional expression

Signals used in expressions are interpreted as input signals, and the resulting circuit will include a constant combinator wired to the input pole that initially sets every signal to `1`. 

Programs that do not use circuit bindings and instead consist of a single expression will use the checkmark output signal. 

#### Operators

There are two types of operators, numeric operators, decision operators, and boolean operators. 

Numeric operators compute mathematical results as expected. Supported operations: `+`, `-`, `*`, `/`, `%`, `**`, `<<`, `>>`, `|`, `&`, `^`

 Note: `**` is exponentiation, and `^` is bitwise XOR. 

Example numeric expression:

    A + (B - 4) ** (3 >> 6 * C)

Decision operators compute a boolean result `1` or `0` from numeric operands. Supported operations: `==`, `!=`, `>`, `>=`, `<`, `<=`

Example decision expression:

    A > 4  

Boolean operators compute a boolean result `1` or `0`. Supported operations: `&&`, `||`, `!`, `===`, `!==`

`===` and `!==` are logical equality and logical inequality respectively. These should be used if you want to see if the truth value of two values are equal.

Example boolean expression:

    E && (A && !B) || (C === D) 

Numeric, decision, and boolean operators can be combined arbitrarily, for example:

`10 + ((A || B) && !(C % 2) || (D <= 4)) - !5`

This is because boolean values are equivalent to numeric values `1` or `0`, and input values `0` are interpreted as `false`, while any other input is interpreted as `true` for boolean operators. 

Note that this means `!!<exp>` is not necessarily equal to `<exp>`, since the first NOT operator will output either `0` or `1`. This means the expression `!!<exp>` can be used to yield the truth value `1` or `0` from an expression. 

Precedence of operators adheres to the standards of C++. 

### Conditional Expressions

Conditional expressions check to see if some expression evaluates to `true`, and if it does outputs some expression, otherwise it outputs some other expression. These are super useful! Conditionals have syntax `if <exp> then <exp> else <exp>`. Here is an example conditional expression:

    if A || (B && C) then 2 * B else 45

 Note that conditionals are themselves expressions, so they can be used as operands in other expresions. This also means they can be chained together to construct "else if" branches of a conditional. Here is an example of an expression using both of those properties:

    (E + 10) - 
    if A || (B && C) then
         2 * B 
    else if A then
        A
    else 
        D

This expression checks to see if either `A` evaluates to true, or both `B` and `C` evaluate to true. If those conditions hold, it outputs two times the value of `B`. Otherwise, if `A` evaluates to true, the value of `A` is output. Otherwise, `D` is output. The result of that whole expression is subtracted from 10 added to `E`. 

You can also use the coalesce operator `??`, where `<exp1> ?? <exp2>` is shorthand for `if <exp1> then <exp1> else <exp2>`. 

### Note on Equality

 The logical equaliy and inequality operators (`===` and `!==`) should be used for comparing equality of booleans, not the numerical equality operators (`==` and `!=`). The need for these operators arises from the fact that booleans are just values `1` or `0`. So there is ambiguity in the intended meaning of an expression. For example, consider the following expression where the input wire carries signals `A=10` and `B=1`.

    if A === B then 
        1
    else 
        -1 

  This statement would return 1, since `A` and `B` are both non-zero and are therefore logically equal. However, if the expression was instead

    if A == B then 
        1
    else 
        -1 

    This statement would return -1, since `A=10` and `B=1` and are therefore not numerically equal, even though they are logically equal. Essentially, when you want the value you are handling to be treated as a boolean, use logical equality and inequality. 

### Circuit Bindings

A circuit binding has the syntax `circuit <output_signal> = <expression>`. If your program has only a single circuit, it should not be terminated by a semicolon. Here is an example circuit binding: 

    circuit D = (A + B - C) / 45

This produces a circuit with the output signal `D`. 


### Compiler Directives 

A compiler directive has the format `#<DIRECTIVE> <ARG>`. Directives and arguments should either be all uppercase, or all lowercase. Arguments are specific to a given directive. 

Currently, there are only two supported directives: `PRIMARY` and `LAYOUT`.

`PRIMARY` determines the wire color that the compiler will use to connect combinators, only using the other color when necessary. It takes arguments `RED` or `GREEN`. `RED` is the default setting.

`LAYOUT` determines how circuits will be laid out on the Factorio grid and  takes either the argument `IDENTITY` or `NAIVE`. 

- `IDENTITY` simply places the circuits in a line, wrapping around to the start if necessary, forming a rectangle. This is the default behavior. 
- `NAIVE` randomly places each combinator in a position that allows it to connect to all of its necessary inputs and outputs. Not recommended. 

A more sophisticated layout strategy using simulated annealing is planned for future versions. 

### More Details

More details on the grammar can be found by looking at [parser.mly](src/parser/parser.mly).

## Example Program 

Here is an example program:

    #PRIMARY GREEN
    
    circuit D = 10 + ((A || B) && (C % 2)) + !5;

    // This is a comment

    circuit E = if D > 45 then F else G + 100;

    (65 + 12) >> 2


This program will layout the circuits using green as the primary wire color, only using red wires when necessary. It will produce three distinct circuits:
- The first circuit has output signal `D`, and takes inputs `A`, `B`, and `C`. 
- The second circuit has output signal `E`, and takes inputs `D`, `F`, and `G`. **This is not the same signal `D` as the output of the first circuit, it is a separate input and not related to the first.**
- The third circuit has output signal checkmark, and takes no inputs. The compiler will attempt to evaluate literal expressions like this immediately, so a constant combinator outputting the result value will be produced

Note that if you intend a circuit binding to be the last output, you must write out the whole binding expression, and not just the output signal. For example:

    circuit D = 10 + ((A || B) && (C % 2)) + !5;
    circuit E = if D > 45 then F else G + 100 

If you instead wrote: 

    circuit D = 10 + ((A || B) && (C % 2)) + !5;
    circuit E = if D > 45 then F else G + 100;
    E

This would be interpreted as three separate circuits, with the final circuit consisting of just a single expression that is equal to the input signal `E`. 

## Future Plans 

I have a lot of potential ideas for how to extend the language and improve functionality, and would love to hear suggestions from people who design circuits in Factorio. What follows is the list of planned additions to the language and compiler:

- Temporary variables 
- Circuit composition operations, such as union (of the outputs) and concatenation 
- Improved layout strategy using simulated annealing to optimize space used
- Import of external circuits via a blueprint string
- Built-in useful circuit constructs, such as timers, filters, latches, and memory cells
- Operations with wildcard signals (I don't know what this will look like)
- Much more! Stay tuned.
