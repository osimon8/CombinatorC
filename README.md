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

A CombinatorC program starts with optional compiler directives, and is followed by a list of commands. Commands are terminated by semicolons. 

Programs will be compiled into a set of circuits, each including an input pole and output pole. Wire the intended circuit inputs to the input pole. 

Single line comments can be written using `//`, and multiline comments can be started with `/*` and closed with `*/`. 

### Commands 

CombinatorC currently supports 3 different commands. They are:

- Cirucit binding 
- Variable definition 
- Output   

A CombinatorC program can have as many outputs as you want, each distinct output will produce a separate circuit. 

### Circuit Bindings

A circuit binding has the syntax `circuit <name> : <output_signal> = <numeric_expression>`.  Here is an example circuit binding: 

    circuit my_circuit : D = (A + B - C) / 45;

This produces a circuit with the output signal `D`. You can use then use the bound variable name "my_circuit" to refer to the circuit in future commands, such as outputs. 

Circuits can be marked as *concrete* by writing `concrete circuit` instead of `circuit` in a binding. This informs the compiler that you want the circuit to be considered separate from any other circuits it is composed with, and should not be merged in with those other circuits when the compiler lays down all the circuits on the Factorio grid. This isn't very useful unless you want the logical components of your circuit to be clearly separate for future purposes. I don't recommend using this, as it is a new feature and could be buggy. 

### Variable Definition 

Temporary variables for use in the code can be defined with the following syntax:

    <type> <name> = <expression>

The supported types are: 

- int 
- signal 
- condition 
- circuit 

`int` types are numeric expressions, and `signal` types are signals.

`condition` types are numeric expressions that can directly map to a decider combinator's config. This essentially means that it must be an expression of the form `<signal> <c_op> <expression>`, where `c_op` is either `==`, `!=`, `>`, `>=`, `<`, or `<=`, and `expression` is a numeric expression without any signals. 

`circuit` types are circuits, which can be the name of a circuit that was previously bound, a pattern call, or a `for` expression. 

Variables used in expressions must have already been defined earlier in the code before they can be used. Duplicate variable names are not allowed. 

### Output 

An output command has the following syntax: 

    output <circuit>

This informs the compiler to output the provided circuit. Multiple output statements are allowed, except in the body of a `for` loop. 

To output a circuit at a specific location, use this syntax:

    output <circuit> at (x,y)

Where `x` and `y` are numeric expressions. The coordinate `(x,y)` represents the location on the Factorio grid the circuit will be placed at. Circuits will be placed relative to the origin `(0,0)` by default, so assigning a manual location will override that and instead place it at the provided location.

Using the `at` syntax also automatically converts the circuit to a concrete circuit. This is only relevant for outputs within the body of a `for` loop. Within the body of a `for` loop, using the `at` syntax will place the circuits relative to the location of the overall circuit. This allows you to place circuits relative to each other, and then output the resulting circuit from the `for` loop somewhere else. 

Note that increasing `y` means moving *down* on the Factorio grid, so if you want to place a circuit above the other circuits, you should use a negative `y` value.  

### Signals 

Signals in Factorio have type "virtual", "item", or "fluid". To express a virtual signal in CombinatorC, write `signal-<name>`, for example `signal-2`. To express an item or fluid signal, write `<type>-<name>`, for example `item-copper-ore` or `fluid-water`. 

As a shorthand, single capital letters are interpreted as the corresponding virtual signal. For example, `A` is interpreted as `signal-A`. 

### Numeric Expressions

A numeric expression is either: 

- A signal (e.g. `B`)
- An integer literal (e.g. `10`, `0`, `-1234`)
- A boolean literal `true` or `false`  (these are converted to integer literals `1` and `0` respectively)
- A mathematical expression composed using operators
- A conditional expression

Signals used in numeric expressions are interpreted as input signals, and the resulting circuit will include a constant combinator wired to the input pole that initially sets every signal to `1`. 

#### Numeric Operators

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

 The logical equaliy and inequality operators (`===` and `!==`) should be used for comparing equality of booleans, not the numerical equality operators (`==` and `!=`). The need for these operators arises from the fact that booleans are just values `1` or `0`. So there is ambiguity in the intended meaning of an expression. Essentially, when you want the value you are handling to be treated as a boolean, use logical equality and inequality. 

### Circuit Composition 

A circuit can be *composed* with another circuit to produce a single new circuit. There are two types of compositions:

- Concatenation   ( `@` )
- Union   ( `\/` )

Concatenation connects the outputs of the first circuit to the inputs of the next circuit, and union combines all of the inputs of both circuits, feeds them all to both circuits, and combines the outputs of both circuits. Here is an example composition using concatentation, assuming the circuits "a" and "b" have been previously defined:

    a @ b 

Concatenation has a higher precedence than union, and concrete circuits composed with regular circuits will produce a resulting circuit that is concrete. 

There is a potential issue with union when the output signal of one circuit matches the input signal of the other circuit. The compiler will print a warning if this conflict is detected, as it may result in a circuit that doesn't function as intended. To avoid this, make sure the output and input signals are distinct when unioning circuits.  

### Pattern Calls 

Circuits can be produced by calling a *pattern*. You can think of these as functions that return circuits. There are currently 3 built-in patterns:

- `counter(int max_value, signal o_sig)`
- `counter2(int max_value, signal i_sig, signal o_sig)`
- `lamp(condition cnd)`

`counter` starts at 0 and counts up by 1 every tick, and takes as an argument the maximum value it will count to before looping back to 0. There are 60 ticks in one second (unless you're playing Factorio on a TI-84 calculator or have a megabase). The argument `o_sig` is the signal that the counter will output. 

`counter2` functions much the same as `counter`, except it also takes the argument `i_sig` which is the input signal that it will use to increment by. This is useful if you want a counter that counts by some value other than 1. 

`lamp` outputs a lamp that has the enabled condition set to the provided argument `cnd`. It also outputs a concrete circuit, so you can layout lamps in some special way that will be preserved by the compiler. Any signals used in the condition are also output.

User defined patterns are currently not supported, but I have plans to add them soon. 

### For loops 

Circuits can also be produced by a `for` loop expression. They have the following syntax: 

    <for> <name>=<numeric_expression1> <to/downto> <numeric_expression2> {
        <command>;
        <command>;
        ...
        <output>;
    }

For loops will initialize an `int` variable with name "name" to the value `numeric_expression1`, and will increment it by 1 (if `to` is used) or decrement it by 1 if (`downto`) is used until the value of `numeric_expression2` is reached. At each iteration, the sequence of commands inside the braces will be executed (the bound variable "name" can be used in this context). The sequence of commands must have only a single output command. 

There are two types of `for` loops, `for_concat` and `for_union`. The output circuit produced by the command sequence will be composed (using concatenation with `for_concat`, and union with `for_union`) with the next circuit output by iteration, yielding a single final output circuit.  

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

Let's put it all together! Here's an example program:

    #PRIMARY GREEN   

    // bounce light back and forth at varying rates

    int n = 10; 
    int t = 100;

    signal ctr-out = signal-0;
    signal rate-out = R;
    int r = 7 * t;

    // increase the rate over time 
    circuit div : rate-out = rate-out / t + 1;
    circuit rate = counter(r - 1, rate-out) @ div;

    // count up, to be used to determine which lamp is on
    circuit add : ctr-out = ctr-out + 1;
    circuit ctr = counter2(t * 2 - 1, rate-out, ctr-out) @ add;

    // a line of n lamps 
    circuit lamps = for_concat i=0 to (n - 1) {
        condition cnd = ctr-out == i;
        output lamp(cnd) at (i, 0);
    };

    int t_by_n = t / n;

    // first half of counter, light up from left, second half light up from right
    circuit cmpt : ctr-out = if ctr-out <= t then
                                ctr-out / t_by_n 
                            else
                            -(ctr-out / t_by_n - 2 * (n - 1));

    output rate @ ctr @ cmpt;
    output lamps at (0, 8);


This program produces line of lamps in which a light will bounce back and forth between the endpoints at a varying rate. Let's see it in action: 

https://user-images.githubusercontent.com/14896850/160725738-cd4cbfa9-7599-423e-9fa1-bfb1078e4e0f.mp4

The circuits seen in the video were generated entirely using the above program, with the exception of wiring the output pole from the combinators to the input pole of the lamps.

## Future Plans 

I have a lot of potential ideas for how to extend the language and improve functionality, and would love to hear suggestions from people who design circuits in Factorio. What follows is the list of planned additions to the language and compiler:

- User-defined patterns 
- More built-in useful circuit constructs, such as filters, latches, and memory cells
- Improved layout strategy using simulated annealing to optimize space used
- Import of external circuits via a blueprint string
- Proper support of operations with wildcard signals (I don't know what this will look like)
- Much more! Stay tuned.
