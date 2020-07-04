
# lips: The non-dumb text preprocessor

**Version 2 is out!**

One sultry summer's afternoon, I tried to write an essay using Markdown. I wanted to have references, and I didn't want to have to manually update the reference IDs to maintain numerical order if I moved text around. Attempting to use the C preprocessor for Markdown immediately didn't go well, and then I tried m4. m4 was much better, but it failed when I tried to pass arguments to a macro that contained commas. Either it doesn't work at all or it's too hard to figure out how to quote stuff, so I dumped it and wrote an extremely simple preprocessor in Common Lisp that doesn't have these problems.

It sports:

- An elegant but powerful Common Lisp-based macro system
- Automatic paragraph separation (e.g. auto-insert `<p>` tags if you want)
- Smart quotes
- more things

## The basic idea

Taking inspiration from LaTeX, all text passes through the preprocessor unaltered except when prefixed by a `\` character. The `\` character interprets the subsequent text as a lisp object and evaluates it, printing the result of the evaluation. That's it.

## But that seems too simple

It's actually not, because it's all we need to make a killer macro system. lips's real workhorse is a special form named `MACRO`, and with it you can do stuff like this:

    \(macro hello (firstname lastname)
       (% "Hello there, ~a ~a!" firstname lastname))

    \hello{Jimbones}{McGee}

This produces the text

    Hello there, Jimbones Mcgee!

In this example, we begin with a `\` character to make lips evaluate our `MACRO` form as code. The `MACRO` call defines a textual macro named `HELLO`, taking one argument, that we can call below with the `\hello{args...}` syntax.

### What's actually going on there?

Under the hood, `MACRO` is defining a Lisp symbol macro named `HELLO`. The symbol macro gets expanded when `\` calls `MACROEXPAND` on the next Lisp object it reads, and the expansion reads _N_ arguments and calls your function with them. 

Because it works this way, you can put any lisp code after the `\`.

That means that you are by no means limited to using `MACRO`. Everything you see used is stuff you could have written yourself. You can use `DEFUN`, `READ`, `WRITE`, and every other Common Lisp function you like. The only constants are: whatever your code reads (from `*STANDARD-INPUT*`) comes from the input file, and whatever your code writes (to `*STANDARD-OUTPUT*`), or returns, gets written to the output file.

btw, to cause lips to emit a literal `\` character in your writing, escape it by prefixing it with another `\` like so: `\\`.

## Deeper topics

### Varargs

By calling the `READ-MACRO-ARGUMENT` function in your code, you can read any number of `MACRO`-style arguments from the input stream. Example:

    \(defsym read-2 ()
       (% "Got ~a and ~a!" (read-macro-argument) (read-macro-argument)))
    
    \read-2{hey}{ho}
    
Produces

    Got hey and ho!

### Nested macro usage

You can use macros inside macros just fine. Using the `READ-2` example from above:

    \read-2{outside-1-a \read-2{inside-1}{inside-2} outside-1-b}{outside-2}
    
Produces

    Got outside-1-a Got inside-1 and inside-2! outside-1-b and outside-2

### Nesting braces

Braces can be nested in macro arguments just fine, but they have to be balanced. Using the `READ-2` example from above:

    \read-2{one{{}}two}{three}

Produces

    Got one{{}}two and three!
    
To use an unbalanced closing or opening brace, escape it with backslash like `\}`.

### Paragraphs

You can set the variable `LIPS:*PARAGRAPH-BEGIN*` and `LIPS:*PARAGRAPH-END*` to a value or function to have them printed before/after paragraphs begin/end.

### Smart quotes

Set `LIPS:*USE-SMART-QUOTES*` to `t` to enable smart-quoting. Smart-quoting respects opening braces, brackets, parentheses, and dashes.

    \(setv lips:*use-smart-quotes* t)
    And "then" there 'were' none

Produces

    And “then” there (‘were’) none
    
### Markup and formatting

lips tracks certain state based on what gets output to the file. For example, it needs to know what the last character was, because if it's whitespace, then the next " character will be a left-side smart quote, and if not, it will be a right-side smart quote. Another example is that it needs to know when to emit paragraph tags (like `<p>` for HTML).

But lips only cares about text that is _visible_ to the reader of the output document. It doesn't want smart quotes to be affected by some macro outputting some invisible HTML tags or something. So you need to be careful which output functions you use to generate content.

Function return values, and things printed with the `%` or `$` functions, are counted as visible text. Everything else, including things printed with native Common Lisp functions like `FORMAT` and `PRINC`, along with the `%!` and `$!` functions, are considered to be invisible _markup_, and bypass all internal lips record keeping.

## Invocation

When invoked with no command-line arguments, lips operates on the text supplied through the standard input stream. Otherwise, the command-line arguments are treated as the names of files which are processed in order of appearance. The special filename "-" indicates that the standard input stream should be processed at that point.

For example,

    lips <foo.lips          # Process foo.lips
    lips foo.lips bar.lips  # Process foo.lips and bar.lips, in that order
    lips foo.lips <bar.lips # Process foo.lips only
    lips foo.lips - baz.lips <bar.lips # Process foo.lips, bar.lips, and baz.lips, in that order

## Builtin functions/macros

### `%` _format-string_ _format-args..._ => _nothing_

Format string and print to output file. Format specifiers are according to the [`FORMAT`](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_format.htm) function.

Equivalent to Common Lisp's `FORMAT` function except doesn't take a stream argument.

### `%!` _format-string_ _format-args..._ => _nothing_

Same as `%` but for markup. Markup is not visible to the reader of the document (think HTML tags or something) and so bypasses some functionality (like smart quotes, paragraph separation) that is only concerned with visible text.

### `$` _object_ => _nothing_

Print _object_ in a human-readable way.

### `$!` _object_ => _nothing_

Same as `$` but for markup.

### `INCLUDE` _filename_ => _nothing_

Treats the contents of the file _filename_ as if they had appeared in place of the call to `INCLUDE`.

### `INCLUDE-DEFS` _filename_ => _nothing_

Loads the given _filename_ as plain Common Lisp source code.

## Convenience functions

### `DEFN` _name_ _lambda-list_ _body..._ => _nothing_

Same as `DEFUN` except it does not evaluate to the name of the function.

### `DEFSYM` _name_ _lambda-list_ _body..._ => _nothing_

Defines a function and creates a symbol macro that executes it. The difference between e.g. using `DEFSYM` vs `DEFN` to define a name `BLAH` is that with `DEFSYM`, it can be called like `\BLAH`, whereas with `DEFN` it must be called with parentheses like `\(BLAH)`.

### `DEFV` _name_ _value_ => _nothing_

Same as `DEFPARAMETER` except it does not evaluate to the name of the variable.

### `SETV` _place_ _value_ => _nothing_

Same as `SETF` but without a return value.

### `READ-MACRO-ARGUMENT` &optional (_opener_ _{_) (_closer_ _}_) _macro-name_ => _string_

Reads an argument to a textual macro (think `\blah{arg 1}{arg 2}` where a "macro argument" is `arg 1` or `arg 2` etc), enclosed in the given characters _opener_ (default `{`) and _closer_ (default `}`). Other macro calls can be nested in the macro argument and will be expanded.

Initial whitespace is skipped.

Openers and closers can be nested as long as they are matched. Unmatched openers and closers can be produced by prefixing them with a backslash.

If any character is encountered (after skipping whitespace) before _opener_ is read, then an error is signalled.

The optional argument _macro-name_ is used for error messages. If you use `READ-MACRO-ARGUMENT` inside a `DEFSYM` or `MACRO` body, then _macro-name_ will automatically get filled in for you.

### `ADD-FINISH-HOOK` _function_ => _nothing_

Adds the function to the beginning of a list of functions that are called when the end of the input stream is reached.

## Variables

### `*PARAGRAPH-END*` and `*PARAGRAPH-BEGIN*`

These values or functions are printed or called, if non-NIL, between paragraphs.
    
### `*USE-SMART-QUOTES*` 
 
When non-NIL, converts " and ' to smart-quote equivalents. Respects opening braces, brackets, parentheses, and dashes.

### `*HOT-CHAR*` 

The character used to set off macro expansions. Defaults to `\`.

## Examples

### Simple example

Input:

    ohayou\\\\\\

Output:

    ohayou\\\
    
### Simple macros

Input:

    \(macro hello (firstname lastname)
       (% "Hello there, ~a ~a!~%" firstname lastname)
       ($ "Goodbye ")
       ($ (list 1 2 3)))
    
    \hello{Jimbones}{McGee}
    
Output:

    Hello there, Jimbones Mcgee!
    Goodbye (1 2 3)
    
This demonstrates usage of the `%` and `$` functions. The `$` function can print any object (a list in this example). 
    
### Variable definition

Input:

    \(defv blah "text")
    \blah

Output:

    text
    
### Function definition/calling

Input:

    \(defn add (&rest nums)
        (% "Sum of ~{~a ~}: " nums)
        ($ (apply #'+ nums)))
    \(add 1 2 3)
    
Output:

    Sum of 1 2 3 : 6

### Finish hooks

These are useful for adding footers, etc. They are called in reverse order of registration.

Input:

    \(add-finish-hook (lambda () "Bye there!"))
    Hi there!
    
Output:

    Hi there!
    Bye there!

### Executing arbitrary lisp code

The true power of using lisp as a preprocessor is evident when writing more complicated definitions.

Input:

    \(progn
        ;; Load ASDF, then
        (asdf:load-system :drakma)
    
        (let ((input (drakma:http-request "http://google.com"
                                          :want-stream t)))
            #| Use input... |#
            (close input)))
    
## Dependencies

lips depends on the [UNIX-OPTS package](http://quickdocs.org/unix-opts/), which it bundles, so you don't need anything but a Lisp interpreter.

## Compatibility

   I have only tested lips with SBCL, but it shouldn't be using any platform-specific code, so others should be fine. Pull requests for compatibility fixes are welcome.
