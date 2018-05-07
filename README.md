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
## pattern matching
left = right
* `right` is computed
* `left` tries to match
