A rough todo-list, in lieu of using a task tracker &c.

More or less a listing of functionality described in the Language Reference, with implementation hints relevant to what I'm building.

- Function analysis clean-up
    - [x] A function with no return type can omit `return` statement
- [x] String `len`
    - Should add a representation of native functions to use in `print` and `len-list`
- Lists
    - [x] Representation in Stack IR
    - [x] Implement operators, literal
        - Language does not allow for appending to a list, but we'll use it for literal construction
        - Concat will look exactly like str concat
        - Indexing is straightforward
    - [x] `len`
    - [x] Iteration `for x in L:`
        - Desugaring into while loop with indexing; this should be implemented in a pre-typechecker phase
- Classes
    A ChocoPy class is a set of variables and associated methods. A class can inherit from another class, gaining all of its member variables and methods allowing for methods to be overriden.
    A ChocoPy value is assignable to any subtype of a the value's type, and because we also allow method overrides we need a way to dynamically look up the implementation of a method for a particular instance of a clss.
    To implement this an object in memory will consist of a pointer to a dynamic dispatch table and the object's member variables.
    The dispatch table will be built symbolically during type checking. We'll ensure that the prefix of each table is the same for any subtype by appending a subtype's non-override methods to the table.
    Then a method call is a call into the dispatch table offset by the position of the method in the dispatch table.
    In the Stack IR the dispatch table will be a block with a sequence of unconditional jump instructions to the positions of the various member methods.

    - Introduce object type into type checker
    - Create class bindings in type environment
        - Is `__init__` ever explicit? Yes, and not clear if it can declare any additonal parameters.
        - Type check member var declarations, build var table (also needs matching prefix property for inheritance!)
        - Type check methods, build symbolic dispatch table
    - Generate dynamic dispatch table
    - Implement method call Instr + calling convention (self first)

    - Extend class type checking to support inheritance, maintaining common prefix properties
    - Implement assignability analysis, walking inheritance graph, including least common ancestor "join" analysis
    - (Should just work after this, calling convention and code gen don't change)


    - Layout in memory
    - Inheritance
    - Extension to type environments
    - Member function to function transformation
- "Assignability" analysis in type check instead of exact type matching
    - Crucially, None is assignable to any type, so we can create some errors at runtime
    - There's a slightly weird asymmetry here where lists [T] are assignable with None, but not `str`.

- `global` support
    - actually place globals in a ".data" segment
- `nonlocal` support
    - stack link
- Proper support for 
- Nested functions (using stack links)
- Built-in functions `input` and `__init__` for "primitives"

Runtime error handling:

- `print` / `len` wrong type
- Division by zero
- Index out of bounds (string or list)
- Operation on `None`
- Out of memory 

Longer:

- Garbage collection
- Lower to x64, first through a lower-level register IR
    - Ultimate goal would be to link against some kind of small runtime and run as a standalone executable

