tokeniser
- unexpected char
- unterminated string
- unterminated multiline comment
- invalid float
- invalid int
- invalid identifier

parser
- unexpected token, many cases to consider
- negative array length
- non const array length
- invalid lvalue
- invalid ref target

typer
- unexpected return type, many cases
- unexpected type, many cases
- unexpected array length
- unexpected array elem type
- proc already defined
- struct already defined
- var already defined
- proc not defined
- struct not defined
- var not defined
- projecting into non struct
- projecting into non struct ref
- deref non-pointer
- indexing non-array
- arith with non-number
- scope not defined?
- unkown struct field
- wrong arg count
- can't infer type
- unreachable code after return

- struct default expressions

eval
- a bunch of tests to see if functions return what we expect
- function calls work
- function calls isolate scope
- blocks isolate scope
- structs work, init, access and passing to other functions
- nested structs
- pass by value everythign
- references can be used, and modify underlying data
- references can be passed to functions
- references can be held in structs
- arrays are fully passed by value, even nested arrays
- slices are passed by value of the header
- precedence tests
- associativity tests