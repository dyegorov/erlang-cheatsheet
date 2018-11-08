# erlang-cheatsheet
# shell
## start
```
$ erl
```
exit with `ctrl+c` or
```
2> q().
ok
```
abort with `ctrl+g` - (abort with ^G)

## edit commands
^ means ctrl+
```
^A = line start
^B = line end
^T = change last 2 symbols
Tab = try to autofill (current module or funct)
```
# variables
Variable name MUST start with capital letter (technically can start with `_` but used only for unused vars).
```
1> X = 21.
21
2> X.
21
```
Variables are immutable:
```
3> X = 3.
** exception error: no match of right hand side value 3
```
`_` is anonymous variable. Variable can be cleared using `f(Variable).`:
```
13> X.
2
14> f(X).
ok
15> X.
* 1: variable 'X' is unbound
```

# pattern matching
left = right
* `right` is computed
* `left` tries to match
# boolean operations
```
16> true and false.
false
17> false or true.
true
18> true xor false.
true
19> not false.
true
20> not (true and true).
false
```
`or` and `and` always calculates both sides. For lazy calculation use `andalso` and `orelse`.
```
21> 5 =:= 5.
true
22> 1 =:= 0.
false
23> 1 =/= 0.
true
24> 5 =:= 5.0.
false
25> 5 == 5.0.
true
26> 5 /= 5.0.
false
```
```
27> 1 < 2.
true
28> 1 < 1.
false
29> 1 >= 1.
true
30> 1 =< 1.
true
```
# data types
## float
`/` operation always return float
```
5> 4/2.
2.0
```
integer division is `div`
division remainder is `rem`
```
3> 5 div 2.
2
4> 5 rem 2.
1
```
numbers in other bases use `base#value`
```
6> 2#101010.
42
7> 8#0677.
447
8> 16#AE.
174

```
## atom
* Used to represent non-numeric constants
* All atoms are global
* Starts with lowercase letter
* Letters, numbers, `_`, `@` can be used: `jea@somehost`, `@_long_name`
* Can be quoted and break naming rule: `'Monday'`,`'some atom with space'`
* Do not generate atoms. Atoms table has limited size. Use them as consts.
* Some atoms are reserved words: `after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse query receive rem try when xor`
## tuple
Organize and group your values! If you know their number.
```
5> Person = {person, {name, {first, joe},{last,armstrong}},{footsize,42}}.
{person,{name,{first,joe},{last,armstrong}},{footsize,42}}

6> Point = {point, 10, 45}.
{point,10,45}
```
{atom, element} = `tagged tuple`.
Extract data from tuple
```
2> {point, X, Y} = Point.
{point,10,45}
3> X.
10
4> Y.
45

6> {_,{_, {_, Who},_},_} = Person.
{person,{name,{first,joe},{last,armstrong}},{footsize,42}}
7> Who.
joe
```
## list
* `[1,2,3,4,5]` is a list
* Consist of head (1st element) and tail (all other elements as list): [H|T]
* `[]` is empty list
Create list
```
8> ThingsToBuy = [{apples,10},{pears,6},{milk,3}].
[{apples,10},{pears,6},{milk,3}]
```
Add several elements to head
```
9> MoreThingsToBuy = [{oranges,4},{newspaper,1} | ThingsToBuy]. 
[{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}]
```
Extract elements from list
```
10> [FirstThing | LeftToBuy] = MoreThingsToBuy.
[{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}]
11> FirstThing.
{oranges,4}
12> LeftToBuy.
[{newspaper,1},{apples,10},{pears,6},{milk,3}]
```
Add lists and remove lists
```
31> [1,2,3,4,5] -- [1,2,4].
[3,5]
32> [1,2,3,4,5] -- [5].
[1,2,3,4]
33> [1,2,3] ++ [4,5].
[1,2,3,4,5]
```
Get list head
```
34> hd([1,2,3,4]).
1
```
Get list tail
```
35> tl([1,2,3,4]).
[2,3,4]
```
`[1|2]` is `improper list`
```
36> length([1|2]).
** exception error: bad argument
     in function  length/1
        called as length([1|2])
37> length([1|[]]).
1
```
Proper list always ends with `[]`.
### list comprehensions
```
[F(X) || X <- L].
```
Means map f(x) to L.
```
1> L = [1,2,3,4,5].
[1,2,3,4,5]
2> lists:map(fun(X) -> 2*X end, L).
[2,4,6,8,10]
3> [2*X || X <- L].
[2,4,6,8,10]
```
Most 
```
[X || Qualifier1, Qualifier2, ...]
```
Qualifier is:
* Generator. Pattern <- ListExpression.
* Filter. Return true or false.
```
EvenNumbers = [N || N <- [1, 2, 3, 4], N rem 2 == 0]. % [2, 4]
```
Generator can also filter via pattern matching
```
1> [ X || {a, X} <- [{a,1},{b,2},{c,3},{a,4},hello,"wow"]].
[1,4]
```
```
38> [2*N || N <- [1,2,3,4]].
[2,4,6,8]
```
We can use boolean constraints (like `X rem 2 =:= 0`)
```
39> [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
[2,4,6,8,10]
```
You can have more than one comprehension!
```
44> [X+Y || X <- [1,10], Y <- [2,3,4]].
[3,4,5,12,13,14]
```
Can be used as filters
```
40> Weather = [{toronto,rain},{london, fog},{boston, fog}].
[{toronto,rain},{london,fog},{boston,fog}]
41> FoggyPlaces = [X || {X, fog} <- Weather].
[london,boston]
```

## bit data
```
1> Color = 16#F09A29.
15768105
2> Pixel = <<Color:24>>.
<<240,154,41>>
3> Pixel.
<<240,154,41>>
4> Color.
15768105
5> Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
6> <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
7> Pix1.
13970820
9> <<R:8, G:8, B:8>> = <<Pix1:24>>.
<<213,45,132>>
10> <<R:8, Rest/binary>> = Pixels.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
```
<<Pattern, Rest/binary>> ~= [Head|Tail]
```
13> RGB = [ {R,G,B} || <<R:8, G:8, B:8>> <= Pixels ].
[{213,45,132},{64,76,32},{76,0,0},{234,32,15}]
14> << <<R:8, G:8, B:8>> || {R, G, B} <- RGB >>.
<<213,45,132,64,76,32,76,0,0,234,32,15>>
```

## string
uses doublequotes
```
13> Name = "John".
"John"
```
Strings are just lists of integers:
```
14> Name.
"John"
15> [1|Name].
[1,74,111,104,110]
```
`1` could not be printed as letter.
```
16> [74,111,104,110].
"John"
```
`$` can be used to get char code:
```
17> J = $J.
74
19> O = $o.
111
20> H = $h.
104
21> N = $n.
110
22> [J,O,H,N].
"John"
```
## module
* *.erl
* compiles into *.beam
* exports functions with arity (number of args)
* imports functions with arity
```
-module(geometry).
-export([area/1]).
-import(lists, [map/2, sum/1]).
area({rectangle, Width, Ht}) -> Width * Ht;
area({circle, R}) -> 3.14159 * R * R.
```
compile and use it:
```
1> c(geometry).
{ok,geometry}
2> geometry:area({rectangle, 10, 5}).
50
3> geometry:area({circle, 1.4}).
6.15752
```
## commas, dots and semicolons
* `,` separates arguments
* `.` separates functions and expressions
* `;` separates different clauses
```
Pattern1 ->
  Expressions1;
Pattern2 ->
  Expressions2;
```
## function arity
Is a number of function arguments
Use helpers with the same name:
```
sum(L) -> sum(L, 0).
sum([], N) -> N;
sum([H|T], N) -> sum(T, H+N).
```
## anonymous functions (funs)
```
8> Double = fun(X) -> 2*X end.
#Fun<erl_eval.6.118419387>
9> Double(4).
8
```
Higher order function - returns other functions or uses functions as arguments
```
10> Even = fun(X) -> (X rem 2) =:= 0 end.
#Fun<erl_eval.6.118419387>
11> Even(8).
true
12> lists:map(Even, [1,2,3,4,5,6,7,8]).
[false,true,false,true,false,true,false,true]
13> lists:filter(Even, [1,2,3,4,5,6,7,8]).
[2,4,6,8]
```
## for loop
there is no for loop in erlang) but we can create it:
```
for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].
```

## arithmetic expressions
1. +X, -X (unary)
2. X*Y, X/Y, bnot X (bit not), X div Y (integer division), X rem Y (integer remainder), X band Y (bit and)
3. X+Y, X-Y, X bor Y (bit or), X bxor Y (bit excl or), X bsl N (bit shift left), X bsr N (bit shift right)

## guards
You can use guards in the heads of function definitions where they areintroduced by the `when` keyword, or you can use them at any place in the
language where an expression is allowed.
```
max(X, Y) when X > Y -> X;
max(X, Y) -> Y.
```
A guard is a series of guard expressions, separated by commas (`,`).
The guard `GuardExpr1, GuardExpr2, ..., GuardExprN` is true if all the guard
expressions `GuardExpr1`, `GuardExpr2`, ..., `GuardExprN` evaluate to `true`.
```
is_cat(A) when is_atom(A), A =:= cat -> true;
is_cat(A) -> false.
is_dog(A) when is_atom(A), A =:= dog -> true;
is_dog(A) -> false.
```
Control predicates:
```
is_atom(X), is_binary(X), is_constant(X), is_float(X), is_function(X), is_function(X,N) (with arity N)
is_integer(X), is_list(X), is_number(X), is_pid(X), is_port(X), is_reference(X), is_tuple(X)
is_record(X, Tag), is_record(X,Tag,N)
```
`true` guard is used as 'others' or 'default'
```
if
Guard -> Expressions;
Guard -> Expressions;
...
true -> Expressions
end
```

## records
```
-record(Name, {
%% the next two keys have default values
key1 = Default1,
key2 = Default2,
...
%% The next line is equivalent to
%% key3 = undefined
key3,
...
}).
```