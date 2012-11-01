segment-tree-erl
================

Implementation of [Segment tree](http://en.wikipedia.org/wiki/Segment_tree) data structure in Erlang.

### usage ###
```erlang
1> c(segment_tree_make).
{ok,segment_tree_make}
2> 
2> segment_tree_make:make().
Recompile: interval
Recompile: segment_tree
Recompile: segment_tree_test
  All 3 tests passed.
ok
3> 
3> Sum = fun(X, null) -> X; (X, Y) -> X+Y end.
#Fun<erl_eval.12.82930912>
4> 
4> ST = segment_tree:new([1,2,3,4,5,6,7,8,9,10], Sum).
{segment_tree,{node,{node,{node,{node,{node,1,null,1,
                                            {interval,1,1}},
                                      {node,2,null,2,{interval,2,2}},
                                      3,
                                      {interval,1,2}},
                                {node,{node,3,null,3,{interval,3,3}},
                                      {node,4,null,4,{interval,4,4}},
                                      7,
                                      {interval,3,4}},
                                10,
                                {interval,1,4}},
                          {node,{node,{node,5,null,5,{interval,5,5}},
                                      {node,6,null,6,{interval,6,6}},
                                      11,
                                      {interval,5,6}},
                                {node,{node,7,null,7,{interval,7,7}},
                                      {node,8,null,8,{interval,8,8}},
                                      15,
                                      {interval,7,8}},
                                26,
                                {interval,5,8}},
                          36,
                          {interval,1,8}},
                    {node,{node,9,null,9,{interval,9,9}},
                          {node,10,null,10,{interval,10,10}},
                          19,
                          {interval,9,10}},
                    55,
                    {interval,1,10}},
              #Fun<erl_eval.12.82930912>}
5> 
5> segment_tree:fetch(interval:new(1,5), ST).
15
6> 
6> segment_tree:fetch(interval:new(1,4), ST).
10
7> 
7> segment_tree:fetch(interval:new(1,6), ST).
21
```
