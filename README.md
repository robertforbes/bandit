bandit
======

Very simple implementation of an N-armed bandit written in Erlang for experimenting with machine learning algorithms.

Building
========

cd bandit/src<br>
make<br>

Running 
=======

Example 3-armed bandit, greedy probability 0.9.<br>
from top level directory<br>
erl -pa ebin<br>
1> GB = greedy_bandit:init([{1,2}, {4,7}, {3,9}], 0.9, [0,0,0]).<br>
2> greedy_bandit:run(GB, 1000).<br>
....



