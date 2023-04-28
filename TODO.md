A rough todo-list, in lieu of using a task tracker &c.

More or less a listing of functionality described in the Language Reference, with implementation hints relevant to what I'm building.

- Function analysis clean-up
    - [x] A function with no return type can omit `return` statement
- "Assignability" analysis in type check instead of exact type matching
    - Crucially, None is assignable to any type, so we can create some errors at runtime
- String `len`
- Lists
    - Representation in Stack IR
    - Implement operators, literal
    - Iteration `for x in L:`
    - `len`
- Classes
    - Layout in memory
    - Inheritance
    - Extension to type environments
    - Member function to function transformation
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

