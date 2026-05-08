; Keywords
[
  "match" "insert" "put" "update" "delete"
  "define" "undefine" "redefine"
  "fetch" "select" "sort" "offset" "limit" "require" "distinct" "reduce"
  "with" "end"
  "entity" "attribute" "relation"
  "sub" "sub!" "owns" "plays" "relates" "alias" "value" "label"
  "isa" "isa!" "has" "links" "iid"
  "is" "or" "not" "try" "in" "of" "from" "as"
  "fun" "return" "struct"
  "first" "last" "check"
  "groupby"
  "asc" "desc"
  "let"
] @keyword

; Operators
[
  "==" "!=" ">=" ">" "<=" "<"
  "contains" "like"
] @operator

[
  "+" "-" "*" "/" "^" "%"
] @operator

"=" @operator
"->" @operator

; Punctuation
["(" ")" "[" "]" "{" "}"] @punctuation.bracket
["," ";" ":" "."] @punctuation.delimiter
".." @punctuation.delimiter

; Builtin functions
(builtin_func_name) @function.builtin
(reducer_stat) @function.builtin
(reducer_collect) @function.builtin
"count" @function.builtin

; Function definitions
(function_signature
  (identifier) @function)
(expression_function_name
  (identifier) @function.call)

; Types
(kind) @type.builtin
(value_type_primitive) @type.builtin
(label (identifier) @type)

; Variables
(var_named "$" (identifier_var) @variable)
(var_anonymous) @variable.builtin

; Literals
(quoted_string_literal) @string
(integer_literal) @number
(double_literal) @number.float
(decimal_literal) @number.float
(boolean_literal) @constant.builtin
(date_literal) @string.special
(datetime_literal) @string.special
(datetime_tz_literal) @string.special
(duration_literal) @string.special
(iid_value) @string.special

; Annotations
[
  "@abstract" "@cascade" "@distinct" "@independent"
  "@key" "@unique"
  "@card" "@range" "@regex" "@subkey" "@values"
] @attribute

; Comments
(comment) @comment
