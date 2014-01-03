econcur
=======

Automatically make sequential Erlang code concurrent at compile time using parse transformation. Highly experimental.

econcur spawns a new process for every sequential statement, the process sends the result of the execution back to the parent which then receives it only when any assigned variables are required by succeeding statements. For example, econcur translates this:
```
test(T) ->
	X = long_process(T),
	Y = long_process(T + 1),
	Z = long_process(X),
	lists:sum([X, Y, Z]).
```
into this:
```
test(T) ->
    begin
        __ParentProc = self(),
        spawn(fun() ->
                     __ParentProc ! {'__exec_result','X',long_process(T)}
              end)
    end,
    begin
        __ParentProc = self(),
        spawn(fun() ->
                     __ParentProc ! {'__exec_result','Y',long_process(T + 1)}
              end)
    end,
    receive
        {'__exec_result','X',X} ->
            begin
                __ParentProc = self(),
                spawn(fun() ->
                             __ParentProc ! {'__exec_result','Z',long_process(X)}
                      end)
            end,
            receive
                {'__exec_result','Y',Y} ->
                    receive
                        {'__exec_result','Z',Z} ->
                            lists:sum([X,Y,Z])
                    end
            end
    end.
```

## Usage ##

Make sure econcur is loaded and findable by Erlang, then compile your module(s) with econcur as a parse_transform. For example, add the following to a module:

```
-compile({parse_transform, econcur}).
```
This could also be acheived through the emakefile and rebars systems.

## Future Work ##

* Add suport for anonymous functions.
* Add support for begin ... end.
* Add a method of enforcing that the code should not spawn other processes. Perhaps through a special symbol?
* Test, test and test again! Parse transformations are notorious for adding hard to find bugs to code, so if this were to be used in production it would have to be incredibly well tested.
