// Print out ASCII and high byte values in both forms suitable for
// easy optimisation to printed strings and difficult to optimise
// character by character output
//
// Note the "high bytes" are printed both as greater than 128 and
// as negative values
//
// Robert de bath 2015
// This title is very easy to optimise and includes an ESC character
>>>>>>
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
++++++++++++++++
[
    >+++
    >++++
    >+++++++
    >++++++
    >++
    >+
    <<<<<<-
]
>>>>>...
<<<<.
+.
+.
+.
+.
+.
+.
+.
+.
+.
>+.
+.
+.
+.
+.
+.
<---------.
+.
+.
+.
+.
+.
+.
+.
+.
+.
>-----.
+.
+.
+.
+.
+.
<---------.
+.
+.
+.
+.
+.
+.
+.
+.
+.
>-----.
+.
+.
+.
+.
+.
<---------.
+.
+.
+.
+.
+.
+.
+.
+.
+.
>-----.
+.
+.
+.
+.
+.
>>>>------.
<-----.
<-----.
<<<--------.
>>---.
<---.
>++++++++.
---..
>++++++++++.
<----.
++++++.
>>+++++.
<--.
+++++.
-------.
<--.
>.
++.
<++.
>++.
<--.
>>.
<<+.
>.
<+.
>>-----.
<----------.
<-------.
>>>.
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
// This first should be very easy to optimise so with luck we get
// a literal string with all the characters in the generated code
>>>>
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
++++++++++++++++
[
    >+++
    >++++++++
    >++
    >++
    <<<<-
]
>.
++++++++++.
>----.
>.................................
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
<.
<<++++++++++.
>---------.
>>-----.
<.
>++++++.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
+.
>.
<<.
<<.
>>>>
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
// This is the same again but it is rather more difficult to optimise
+++++++++++
[
    >+++
    >>+++++++++++
    >++++
    >+++++
    >+
    <<<<<<-
]
>-
>>+++
>++++.
>+++.
>-
<<<.
<<
[
    -
    <+
    >>+
    <
]
<
[
    -
    >+
    >.
    <<
]
>
[
    -
    <+
    >>.
    +
    <
]
>>.
>>>.
<<+.
>.
<<.
<<<
[
    -
    >+
    >.
    +
    <<
]
>-
[
    -
    <+
    >>.
    +
    <
]
<+.
>>>.
>>>.
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<<
[
    -
]
// Next the high bytes above 128
++++++++++.
+
[
    >+++
    >>+++++++++++
    >++++
    >+++++
    >+
    <<<<<<-
]
>-
>>+++
>++++++.
>+++.
>-
<<<.
<<
[
    -
    <+
    >>+
    <
]
<
[
    -
    >+
    >.
    <<
]
>
[
    -
    <+
    >>++++
    <
]
<
[
    -
    >+
    >.
    +
    <<
]
>>>.
>>>.
<<+.
>.
<<.
<-
<
[
    -
    <+
    >>+.
    <
]
<
[
    -
    >+
    >+.
    <<
]
>>>.
>>>.
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
// Last the negative byte values
++++++++++.
+
[
    >+++
    >>+++++++++++
    >++++
    >+++++
    >+
    <<<<<<-
]
>-
>>+++
>++++++.
>+++.
>-
<<<.
<<
[
    -
    <+
    >>+
    <
]
<
[
    -
    >+
    >.
    <<
]
>
[
    -
    <+
    >>----
    <
]
<
[
    -
    >+
    >.
    +
    <<
]
>>>.
>>>.
<<+.
>.
<<.
<<
[
    -
    <+
    >>.
    +
    <
]
<
[
    -
    >+
    >.
    +
    <<
]
>>+
>.
>>>.
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<
[
    -
]
<