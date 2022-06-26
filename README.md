# Get started

- **Install gprbuild**
```
$ sudo apt-get update -y
$ sudo apt-get install -y gprbuild
```

<hr>

- **Running the files**
```
$ gprbuild <filename>.adb

$ ./<filename>

... Output here
```

<hr>

- **File details**

```
$ <filename>.adb => this is an basic Ada file

$ <filename>.ads => this is used for Subprograms

$ <filename>.ali => this is for logs (after compiling)
```

## Project 
- This is a small example how to create a multi-language project.

The core-base is written in Ada, while the headers and helper functions are written in raw C. 


Don't know Ada? Read this:
<hr>

- **Language basics**

```
A subprogram in Ada can be either a procedure or a function. A procedure, as illustrated above, does not return a value when called.

with is used to reference external modules that are needed in the procedure. This is similar to import in various languages or roughly similar to #include in C and C++. We'll see later how they work in detail. Here, we are requesting a standard library module, the Ada.Text_IO package, which contains a procedure to print text on the screen: Put_Line.

Greet is a procedure, and the main entry point for our first program. Unlike in C or C++, it can be named anything you prefer. The builder will determine the entry point. In our simple example, gprbuild, GNAT's builder, will use the file you passed as parameter.

Put_Line is a procedure, just like Greet, except it is declared in the Ada.Text_IO module. It is the Ada equivalent of C's printf.

Comments start with -- and go to the end of the line. There is no multi-line comment syntax, that is, it is not possible to start a comment in one line and continue it in the next line. The only way to create multiple lines of comments in Ada is by using -- on each line. For example:
```
```

--  We start a comment in this line...
--  and we continue on the second line...
```

Small example:
```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Greet is
begin
   --  Print "Hello, World!" to the screen
   Put_Line ("Hello, World!");
end Greet;
```

This version utilizes an Ada feature known as a use clause, which has the form use package-name. As illustrated by the call on Put_Line, the effect is that entities from the named package can be referenced directly, without the package-name. prefix.

<hr>

> If/Then/Else

This section describes Ada's if statement and introduces several other fundamental language facilities including integer I/O, data declarations, and subprogram parameter modes.

Ada's if statement is pretty unsurprising in form and function:
```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Check_Positive is
   N : Integer;
begin
   --  Put a String
   Put ("Enter an integer value: ");

   --  Read in an integer value
   Get (N);

   if N > 0 then
      --  Put an Integer
      Put (N);
      Put_Line (" is a positive number");
   end if;
end Check_Positive;
```

```
The if statement minimally consists of the reserved word if, a condition (which must be a Boolean value), the reserved word then and a non-empty sequence of statements (the then part) which is executed if the condition evaluates to True, and a terminating end if.

This example declares an integer variable N, prompts the user for an integer, checks if the value is positive and, if so, displays the integer's value followed by the string " is a positive number". If the value is not positive, the procedure does not display any output.

The type Integer is a predefined signed type, and its range depends on the computer architecture. On typical current processors Integer is 32-bit signed.

The example illustrates some of the basic functionality for integer input-output. The relevant subprograms are in the predefined package Ada.Integer_Text_IO and include the Get procedure (which reads in a number from the keyboard) and the Put procedure (which displays an integer value).
```
`a slight variation on the example, which illustrates an if statement with an else part:`

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Check_Positive is
   N : Integer;
begin
   --  Put a String
   Put ("Enter an integer value: ");

   --  Reads in an integer value
   Get (N);

   --  Put an Integer
   Put (N);

   if N > 0 then
      Put_Line (" is a positive number");
   else
      Put_Line (" is not a positive number");
   end if;
end Check_Positive;
```
<hr>

> For loops

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Greet_5a is
begin
   for I in 1 .. 5 loop
      --  Put_Line is a procedure call
      Put_Line ("Hello, World!" & Integer'Image (I));
      --        ^ Procedure parameter
   end loop;
end Greet_5a;
```
This will print "Hello, World!" fives times to the console.

<hr>

> Cases
```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Check_Direction is
   N : Integer;
begin
   loop
      Put ("Enter an integer value: ");
      Get (N);
      Put (N);

      case N is
         when 0 | 360 =>
            Put_Line (" is due north");
         when 1 .. 89 =>
            Put_Line (" is in the northeast quadrant");
         when 90 =>
            Put_Line (" is due east");
         when 91 .. 179 =>
            Put_Line (" is in the southeast quadrant");
         when 180 =>
            Put_Line (" is due south");
         when 181 .. 269 =>
            Put_Line (" is in the southwest quadrant");
         when 270 =>
            Put_Line (" is due west");
         when 271 .. 359 =>
            Put_Line (" is in the northwest quadrant");
         when others =>
            Put_Line (" Au revoir");
            exit;
      end case;
   end loop;
end Check_Direction;
```

```
This program repeatedly prompts for an integer value and then, if the value is in the range 0 .. 360, displays the associated quadrant or axis. If the value is an Integer outside this range, the loop (and the program) terminate after outputting a farewell message.

The effect of the case statement is similar to the if statement in an earlier example, but the case statement can be more efficient because it does not involve multiple range tests.

Notable points about Ada's case statement:

The case expression (here the variable N) must be of a discrete type, i.e. either an integer type or an enumeration type. Discrete types will be covered in more detail later discrete types.

Every possible value for the case expression needs to be covered by a unique branch of the case statement. This will be checked at compile time.

A branch can specify a single value, such as 0; a range of values, such as 1 .. 89; or any combination of the two (separated by a |).

As a special case, an optional final branch can specify others, which covers all values not included in the earlier branches.

Execution consists of the evaluation of the case expression and then a transfer of control to the statement sequence in the unique branch that covers that value.

When execution of the statements in the selected branch has completed, control resumes after the end case. Unlike C, execution does not fall through to the next branch. So Ada doesn't need (and doesn't have) a break statement.
```

<hr>

> Imperative language - Declarative regions

As mentioned earlier, Ada draws a clear syntactic separation between declarations, which introduce names for entities that will be used in the program, and statements, which perform the processing. The areas in the program where declarations may appear are known as declarative regions.

In any subprogram, the section between the is and the begin is a declarative region. You can have variables, constants, types, inner subprograms, and other entities there.

We've briefly mentioned variable declarations in previous subsection. Let's look at a simple example, where we declare an integer variable X in the declarative region and perform an initialization and an addition on it:
```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   X : Integer;
begin
   X := 0;
   Put_Line ("The initial value of X is "
             & Integer'Image (X));

   Put_Line ("Performing operation on X...");
   X := X + 1;

   Put_Line ("The value of X now is "
             & Integer'Image (X));
end Main;
```
Nested example:
```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure Nested is
   begin
      Put_Line ("Hello World");
   end Nested;
begin
   Nested;
   --  Call to Nested
end Main;
```

A declaration cannot appear as a statement. If you need to declare a local variable amidst the statements, you can introduce a new declarative region with a block statement:
```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Greet is
begin
   loop
      Put_Line ("Please enter your name: ");

      declare
         Name : String := Get_Line;
         --               ^ Call to the
         --                 Get_Line function
      begin
         exit when Name = "";
         Put_Line ("Hi " & Name & "!");
      end;

      --  Name is undefined here
   end loop;

  Put_Line ("Bye!");
end Greet;
```

Warning:
```
The Get_Line function allows you to receive input from the user, and get the result as a string. It is more or less equivalent to the scanf C function.

It returns a String, which, as we will see later, is an Unconstrained array type. For now we simply note that, if you wish to declare a String variable and do not know its size in advance, then you need to initialize the variable during its declaration.
```

<hr>

> If expressions
Here's an alternative version of an example we saw earlier; the if statement has been replaced by an if expression:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Check_Positive is
   N : Integer;
begin
   Put ("Enter an integer value: ");
   Get (N);
   Put (N);

   declare
      S : constant String :=
        (if N > 0 then " is a positive number"
         else " is not a positive number");
   begin
      Put_Line (S);
   end;
end Check_Positive;
```

```
The if expression evaluates to one of the two Strings depending on N, and assigns that value to the local variable S.

Ada's if expressions are similar to if statements. However, there are a few differences that stem from the fact that it is an expression:

All branches' expressions must be of the same type

It must be surrounded by parentheses if the surrounding expression does not already contain them

An else branch is mandatory unless the expression following then has a Boolean value. In that case an else branch is optional and, if not present, defaults to else True.

Here's another example:
```

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   for I in 1 .. 10 loop
      Put_Line (if I mod 2 = 0 then "Even" else "Odd");
   end loop;
end Main;
```

<hr>

> Case expressions

Analogous to if expressions, Ada also has case expressions. They work just as you would expect.
```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   for I in 1 .. 10 loop
      Put_Line (case I is
                when 1 | 3 | 5 | 7 | 9 => "Odd",
                when 2 | 4 | 6 | 8 | 10 => "Even");
   end loop;
end Main;
```
