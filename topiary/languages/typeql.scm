; Topiary formatting query for TypeQL
;
; Philosophy: Preserve input formatting as much as possible.
; Enforce spacing around keywords/operators.

; === Leaves (don't reformat contents) ===
[
  (comment)
  (quoted_string_literal)
  (iid_value)
  (label_scoped)
] @leaf

; === Allow blank lines ===
[
  (comment)
  (definable)
  (pattern)
  (statement)
  (query)
  (query_stage)
] @allow_blank_line_before

; === Comments ===
(comment) @append_hardline
(comment) @prepend_input_softline

; === Keyword spacing ===
[
  "fetch"
  "select"
  "sort"
  "offset"
  "limit"
  "require"
  "distinct"
  "reduce"
  "not"
  "try"
  "fun"
  "return"
  "struct"
  "let"
  "with"
] @append_space

; Clause keywords: use input_softline so input decides if newline follows
[
  "match"
  "insert"
  "put"
  "update"
  "delete"
  "define"
  "undefine"
  "redefine"
] @append_input_softline

; Keywords mid-statement: space on both sides
[
  "in"
  "from"
  "as"
  "isa"
  "isa!"
  "sub"
  "sub!"
  "owns"
  "plays"
  "relates"
  "has"
  "links"
  "is"
  "or"
  "of"
  "value"
  "contains"
  "like"
] @prepend_space @append_space

; === Operators ===
[
  "=="
  "!="
  ">="
  ">"
  "<="
  "<"
  "="
  "->"
  "+"
  "*"
  "/"
  "^"
  "%"
] @prepend_space @append_space

; Minus in expression context
(expression_value
  "-" @prepend_space @append_space
)

; === Punctuation spacing ===

; Commas: space after (default)
"," @append_space

; Commas in type definitions: preserve input line breaks
(definition_type
  "," @append_input_softline
)

; Commas in thing constraints: preserve input line breaks
(thing_constraint_list
  "," @append_input_softline
)

; Semicolons: preserve input line breaks
";" @append_input_softline

; Colons in role players: space after
(role_player
  ":" @append_space
)

; Colons in fetch entries: space after
(fetch_object_entry
  ":" @append_space
)

; Colons in function arguments: space after only
(function_argument
  ":" @append_space
)

; Colon before function block: preserve input line break
(definition_function
  ":" @append_input_softline
)

; === Kind needs trailing space ===
(kind) @append_space

; === Space after type_ref in has constraints ===
(has_constraint
  (type_ref) @append_space
  .
  [(expression_value) (expression_list) (comparison) (var)]
)

; === Annotations: space before ===
[
  "@abstract"
  "@cascade"
  "@distinct"
  "@independent"
  "@key"
  "@unique"
  "@card"
  "@range"
  "@regex"
  "@subkey"
  "@values"
] @prepend_space

; === Structure: preserve input line breaks ===
(definable) @prepend_input_softline
(pattern) @prepend_input_softline
(query_stage) @prepend_input_softline
(return_statement) @prepend_input_softline
"{" @append_input_softline
"}" @prepend_input_softline

; === Indentation ===

; Define: newline after keyword, no indent (definitions at column 0)
(query_define
  "define" @append_hardline
)

; Match/insert body: indent patterns, preserve input line break
(clause_match
  "match" @append_indent_start
  (patterns) @append_indent_end
)
(clause_insert
  "insert" @append_indent_start
  (patterns) @append_indent_end
)





; Type capability continuation: indent after first comma
; The first comma is always after label, annotations, or first type_capability
(definition_type
  (label)
  .
  "," @append_indent_start
)
(definition_type
  (annotations)
  .
  "," @append_indent_start
)
; Case 3: comma after first type_capability (which is a sub declaration)
(definition_type
  (type_capability
    (type_capability_base
      (sub_declaration)))
  .
  "," @append_indent_start
)
; Close indent at semicolon when definition had continuation
(definable
  (definition_type
    (label)
    .
    ",")
  ";" @prepend_indent_end
)
(definable
  (definition_type
    (annotations)
    .
    ",")
  ";" @prepend_indent_end
)
(definable
  (definition_type
    (type_capability
      (type_capability_base
        (sub_declaration)))
    .
    ",")
  ";" @prepend_indent_end
)

; Disjunction/conjunction/negation: indent patterns inside braces
(pattern_disjunction
  (patterns) @prepend_indent_start @append_indent_end
)
(pattern_conjunction
  (patterns) @prepend_indent_start @append_indent_end
)
(pattern_negation
  (patterns) @prepend_indent_start @append_indent_end
)

; Function body: newline + indent
(definition_function
  ":" @append_hardline @append_indent_start
  (function_block) @append_indent_end
)

; After the last query, ensure trailing newline
(source_file
  (query) @append_hardline
)
