<h2>Stringy - A String Based Esoteric Programming Language</h2>
<h3>Introduction</h3>
<p>Stringy is an esoteric programming language being developed by me, Josh Danis. <br/><br/>Stringy programs are a string of ASCII characters, printable non-letter characters (and <code>\DEL</code>) are each commands which change the string in some way. When the program terminates whatever is left of the string is printed on the command line.<br/><br/> The Stringy interpreter was originally written in Haskell. This initially was as a challenge to myself. Subsequently, some of the I/O with Stringy made it difficult in Haskell. I've since written an interpreter in Python.</p>
<h3>Commands</h3>
<p>Each character in a a Stringy string is and ASCII character. Printable non-letter characters and the special case of <code>\DEL</code> each represent a command which is operated on the string itself. Hence each character can be thought of as both memory and commands simultaneously. The command pointer starts at character 0. When a command is executed generally it is dropped from the string. As such the command pointer doesn't change as subsequent command shifts right (this isn't always the case but is helpful). The <code>.</code> character is the terminate command. At which point the program ends and whatever is left is printed to the terminal. <br/><br/> Here are the commands with examples. Unless otherwise stated assume the command pointer is at 0. Also note the before and after examples below are not meant to be the starting program and its end result but only its incremental result. I will, where possible show a <code>Hello, World!</code> program highlighting the function in question.</p>

<table>
    <tr>
        <td><strong>Command</strong></td>
        <td><strong>Description</strong></td>
        <td><strong>Example</strong></td>
    </tr>
    <tr>
        <td><code>SPACE</code></td>
        <td>Read user input string</td>
        <td>User enters <code>"Hello, World!"</code>: <code>&nbsp;.</code><br/> Incremental result: <code>Hello, World!.</code><br/> Note the command pointer in the example will then be at 13 the <code>.</code></td>
    </tr>
    <tr>
        <td><code>!</code></td>
        <td>Back jump</td>
        <td>Moves the command pointer back to skip the previous command.<br/><code>!Hello, World.!</code><br/>Incremental result command pointer at 12, <code>.</code>: <code>Hello, World.!</code></td>
    </tr>
    <tr>
        <td><code>"</code></td>
        <td>Upper case</td>
        <td></td>
    </tr>
    <tr>
        <td><code>#</code></td>
        <td>Length</td>
        <td>Returns the length of the preceding string as and ASCII character<br/>Pointer at 7 <code>ello., #(2^(!,World!</code> -> <code>ello., \a(2^(!,World!</code></td>
    </tr>
    <tr>
        <td><code>$</code></td>
        <td>Skip</td>
        <td>Ingores the next command. Increments the command pointer.<br/>Pointer at 5 <code>Hello$,. World!</code> -> <code>Hello,. World!</code> pointer at 6</td>
    </tr>
    <tr>
        <td><code>%</code></td>
        <td>Modulo</td>
        <td>Performs the modulo function on preceding characters with the right char<br/>Pointer at 1 <code>h%H(,(o(l(l(e(H.World!</code> -> <code>&nbsp;(,(o(l(l(e(H.World!</code> pointer at 7</td>
    </tr>
    <tr>
        <td><code>&amp;</code></td>
        <td>undefined</td>
        <td></td>
    </tr>
    <tr>
        <td><code>'</code></td>
        <td>Lower case</td>
        <td></td>
    </tr>
    <tr>
        <td><code>(</code></td>
        <td>Throw forward</td>
        <td>Takes the next character and throws it to the front of the string.<br/>Pointer at 2 <code>el(H.lo, World!</code> -> <code>Hel.lo, World!</code> pointer at 3.</td>
    </tr>
    <tr>
        <td><code>)</code></td>
        <td>Throw backward</td>
        <td>Takes the next character and throws it to the back of the string.<br/><code>)!.Hello, World</code> -> <code>.Hello, World!</code></td>
    </tr>
    <tr>
        <td><code>*</code></td>
        <td>Operate on One Character</td>
        <td>Uses the right char to operate on only the left char and not the whole preceding string.<br/>Pointer at 5 <code>Hellp*]., World!</code> -> <code>Hello., World!</code></td>
    </tr>
    <tr>
        <td><code>+</code></td>
        <td>Addition</td>
        <td>Adds the right-char's value to all preceding string. Note this consumes the right char.<br/>Pointer at 5 <code>'DKKN+!., World!</code> -> <code>Hello., World!</code></td>
    </tr>
    <tr>
        <td><code>,</code></td>
        <td>Goto 0</td>
        <td>Returns the command pointer to 0.<br/>Pointer at 6 <code>.Hello,, World!</code> -> <code>.Hello, World!</code></td>
    </tr>
    <tr>
        <td><code>-</code></td>
        <td>Subtract</td>
        <td>Subtracts the right-char's value from all preceding string. Note this consumers the right char.<br/>Pointer at 1 <code>i-!.ello, World!</code> -> <code>H.ello, World!</code></td>
    </tr>
    <tr>
        <td><code>.</code></td>
        <td>Terminate</td>
        <td><code>.Hello, World!</code> -> <code>Hello, World!</code> note that here is is what actually will print to the terminal.</td>
    </tr>
    <tr>
        <td><code>/</code></td>
        <td>Divide</td>
        <td></td>
    </tr>
    <tr>
        <td><code>0</code>-<code>9</code></td>
        <td>Append digit</td>
        <td>Multiplies all preceding string by 10 and add the corresponding digit to the chars' decimal value.<br/>Pointer at 1 <code>\a2.ello, World!</code> -> <code>H.ello, World!</code></td>
    </tr>
    <tr>
        <td><code>:</code></td>
        <td>Save function</td>
        <td>Takes the preceding string and keeps it so as to be called again by the function. If <code>:</code> is called again any previously saved function is overwritten and lost.<br/>Pointer at 5 <code>World:Hello$,$ |.!</code> -> <code>Hello$,$ |.!</code> pointer at 0</td>
    </tr>
    <tr>
        <td><code>;</code></td>
        <td>Subroutine call</td>
        <td>This treats the preceding string as its own Stringy program and executes it to termination before returning to the originating program. Note that the subroutine must contain its own <code>.</code> command to escape to the original function. Also note that the subroutine does not print its result to the terminal, instead it "prints" its result into the originating function.<br/>Pointer at 9 <code>$'DKKN+!.;., World!</code>, treats <code>$'DKKN+!.;</code> as its own program which runs to completion result is <code>Hello., World!</code></td>
    </tr>
    <tr>
        <td><code>&lt;</code></td>
        <td>Shift left</td>
        <td>Bit shifts all preceding string to the left<br/>Pointer at 1 <code>$&lt;.ello, World!</code> -> <code>H.ello, World!</code></td>
    </tr>
    <tr>
        <td><code>=</code></td>
        <td>Equal to</td>
        <td>Tests whether the left and right chars are equal if so return <code>1</code> else <code>0</code><br/>Pointer at 1 <code>A=A{.Hello, World!</code> -> <code>1{.Hello, World!</code></td>
    </tr>
    <tr>
        <td><code>&gt;</code></td>
        <td>Shift right</td>
        <td>Bit shifts all preceding string to right<br/>Pointer at 1 <code>C&gt;(),.Hello, World</code> -> <code>!(),.Hello, World</code></td>
    </tr>
    <tr>
        <td><code>?</code></td>
        <td>Shuffle</td>
        <td>Shuffles preceding string. That is splits the string in two (for odd length its the left heavy) and interleaves the characters, like shuffling a deck of cards.<br/>Pointer at 13 <code>Hlo ol!el,Wrd?.</code> -> <code>Hello, World!.</code></td>
    </tr>
    <tr>
        <td><code>@</code></td>
        <td>Goto</td>
        <td>Changes the command pointer to decimal value of the right char, in the new string! Note the command pointer wraps so if this is a value outside the length of the string it will be modulo the length.<br/><code>@&nbsp;................................\DEL.Hello, World!</code> -> <code>................................\DEL.Hello, World!</code> pointer at 32 the <code>\DEL</code></td>
    </tr>
    <tr>
        <td><code>[</code></td>
        <td>Increment</td>
        <td>Increments preceding string by 1. If you're cringing that this points towards the negative numbers on the Cartesian plane, so am I, but I wanted to mirror <code>&lt;</code><br/>Pointer at 2 <code>Gd[.llo, World!</code> -> <code>He.llo, World!</code></td>
    </tr>
    <tr>
        <td><code>\</code></td>
        <td>Greater than</td>
        <td>If the left-char is greater than the right-char return <code>1</code> otherwise return <code>0</code>. Note this consumes the left and right chars.<br/>Pointer at 1 <code>B\A{.Hello, World!</code> -> <code>1{.Hello, World!</code></td>
    </tr>
    <tr>
        <td><code>]</code></td>
        <td>Decrement</td>
        <td>Decrements preceding string by 1.<br/>Pointer at 2 <code>If].llo, World!</code> -> <code>He.llo, World!</code></td>
    </tr>
    <tr>
        <td><code>^</code></td>
        <td>Swap</td>
        <td>Swaps the right and left chars on either side of it.<br/>Pointer at 1 <code>e^H.llo, World!</code> -> <code>He.llo, World!</code> pointer at 2</td>
    </tr>
    <tr>
        <td><code>_</code></td>
        <td>Copy</td>
        <td>Creates n copies of the preceding string where n is the decimal value of the right char. Note this eats the right char.<br/>Pointer at 2 <code>lo_\STX?^,^ ^Wrld(e(H.!</code> -> <code>lolo?^,^ ^Wrld(e(H.!</code> pointer at 4.<br/>If you're curious what this complete program looks like in a type-able form: <br/><code>$2-0(_(o(l,?^,^ ^Wrld(e(H.!</code></td>
    </tr>
    <tr>
        <td><code>`</code></td>
        <td>undefined</td>
        <td></td>
    </tr>
    <tr>
        <td><code>{</code></td>
        <td>If statement</td>
        <td>If the left char decimal value is odd then then if-statement code is kept otherwise it is dropped. Note that if there is a corresponding <code>}</code> this is dropped in case of true, otherwise everything between is dropped. If no <code>}</code> exists only right char is kept or dropped. Note this eats the left char. <br/>Pointer at 1 <code>1{.Hello, World!</code> -> <code>.Hello, World!</code></td>
    </tr>
    <tr>
        <td><code>|</code></td>
        <td>Call function</td>
        <td>Calls a function save by <code>:</code> and pointer is at end of this code. If no function was saved it injects and empty string.<br/>Pointer at 7 <code>Hello, |.!</code> -> <code>Hello, World.!</code> pointer at 12. See example for <code>:</code> above</td>
    </tr>
    <tr>
        <td><code>}</code></td>
        <td>If statement - closing brace</td>
        <td>No behavior of its own yet needs <code>{</code><br/>Pointer at 1 <code>0{Whoops!}.Hello, World!</code> -> <code>.Hello, World!</code></td>
    </tr>
    <tr>
        <td><code>~</code></td>
        <td>Reverse</td>
        <td>Reverses the preceding string.<br/>Pointer at 13 <code>!dlroW ,olleH~.</code> -> <code>Hello, World!.</code></td>
    </tr>
    <tr>
        <td><code>\DEL</code></td>
        <td>Delete</td>
        <td>Deletes the Preceding string. Note though this is a command it is not type-able so some creativity needs to be employed to get the character in there.<br/>Pointer at 32 <code>................................\DEL.Hello, World!</code> -> <code>.Hello, World!</code></td>
    </tr>
</table>

<h3>Hello, World! - An Exhibition</h3>
<p>Here's a list of all the <code>Hello, World!</code> programs from above in their initial executable state. That is to say a lot of the examples above don't have the command pointer at 0, and/or aren't type-able and are mid operation as it were. Below in order of occurrence are the programs above. Note that some examples above are actually iterations of the same program (the <code>@</code> and <code>\DEL</code> examples for instance) thus are show in order of earliest example appearance.</p>
<ul>
    <li><code>&nbsp;.</code> not strictly an actual <code>Hello, World!</code> program</li>
    <li><code>!Hello, World.!</code></li>
    <li><code>ello$.$,$ #(2^(!,World!</code></li>
    <li><code>Hello$,. World!</code></li>
    <li><code>h%H(,(o(l(l(e(H.World!</code></li>
    <li><code>el(H.lo, World!</code></li>
    <li><code>)!.Hello, World</code></li>
    <li><code>Hellp*]., World!</code></li>
    <li><code>$'DKKN+!., World!</code></li>
    <li><code>$.Hello,, World!</code></li>
    <li><code>i-!.ello, World!</code></li>
    <li><code>.Hello, World!</code></li>
    <li><code>$(-!2.ello, World!</code></li>
    <li><code>World:Hello$,$ |.!</code></li>
    <li><code>$9-0(@,$'DKKN+!.;., World!</code></li>
    <li><code>$$&lt;.ello, World!</code></li>
    <li><code>A=A{.Hello, World!</code></li>
    <li><code>C&gt;(),.Hello, World</code></li>
    <li><code>!Hlo ol!el,Wrd?.</code></li>
    <li><code>$4-0(@,%.`0],(?$~[,.Hello, World!</code></li>
    <li><code>Gd[.llo, World!</code></li>
    <li><code>B\A{.Hello, World!</code></li>
    <li><code>If].llo, World!</code></li>
    <li><code>e^H.llo, World!</code></li>
    <li><code>$2-0(_(o(l,?^,^ ^Wrld(e(H.!</code></li>
    <li><code>$1{.Hello, World!</code></li>
    <li><code>$0{Whoops!}.Hello, World!</code></li>
    <li><code>!!dlroW ,olleH~.</code></li>
</ul>
