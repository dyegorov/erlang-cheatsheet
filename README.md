# erlang-cheatsheet
## shell
### start
```
$ erl
```
exit with ctrl+c

### edit commands
^ means ctrl+
```
^A = line start
^B = line end
^T = change last 2 symbols
Tab = try to autofill (current module or funct)
```
## variables
Variable name MUST start with capital letter
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
`_` is anonymous variable.
## pattern matching
left = right
* `right` is computed
* `left` tries to match
## data types
### float
`/` operation always return float
```
5> 4/2.
2.0
```
### atom
* Used to represent non-numeric constants
* All atoms are global
* Starts with lowercase letter
* Letters, numbers, `_`, `@` can be used: `jea@somehost`, `@_long_name`
* Can be quoted and break naming rule: `'Monday'`,`'some atom with space'`
### tuple
Create
```
5> Person = {person, {name, {first, joe},{last,armstrong}},{footsize,42}}.
{person,{name,{first,joe},{last,armstrong}},{footsize,42}}

6> Point = {point, 10, 45}.
{point,10,45}
```
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
### list
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
### string
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
