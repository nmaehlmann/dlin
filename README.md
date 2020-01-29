# dlin
dlin is an interpreter for *Linear Simultaneous Recursion Schemes* (LSRS) as described by Etienne Grandjean and Thomas Schwentick in 2002 their paper *Machine-Independent Characterizations and Complete Problems for Deterministic Linear Time*.

Programs can be formulated as a set of equations:

F<sub>i</sub> = {1, id, n, u<sub>1</sub>,  u<sub>2</sub>, ..., u<sub>k</sub>, f<sub>1</sub>, f<sub>2</sub>, ..., f<sub>i</sub>}

where 
- 1(x) = 1
- id(x) = x
- n(x) = n (the size of the input)
- u<sub>1</sub>,  u<sub>2</sub>, ..., u<sub>k</sub> are predefined unary functions (think of them as input)

and each f<sub>i</sub> is of one of the two follwing shapes:

1. (Operation) f<sub>i</sub>(x) = g(x) + g'(x) //where g,g' are from F<sub>i-1</sub>
2. (Recursion) f<sub>i</sub>(x) = g\[ep(h,x)\]x //where h is from F<sub>i-1</sub> and g is from F<sub>q</sub>
