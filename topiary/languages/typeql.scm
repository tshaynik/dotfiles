; Topiary formatting query for TypeQL
;
; Reference for Topiary captures:
;   @leaf - Don't format this node's content
;   @append_space / @prepend_space - Add space after/before
;   @append_hardline / @prepend_hardline - Add newline after/before
;   @append_spaced_softline / @prepend_spaced_softline - Space or newline
;   @append_empty_softline / @prepend_empty_softline - Empty or newline
;   @append_indent_start / @append_indent_end - Indentation
;   @allow_blank_line_before - Preserve blank lines from input
;   @delete - Remove this node
;   @do_nothing - Anchor for adjacency patterns

; === Leaves (don't reformat) ===
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
  (query)
] @allow_blank_line_before

; === Comments ===
(comment) @append_hardline
(comment) @prepend_input_softline

; === Keywords with trailing space ===
[
  "match"
  "insert"
  "put"
  "update"
  "delete"
  "define"
  "undefine"
  "redefine"
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
  "in"
  "from"
  "as"
  "fun"
  "return"
  "struct"
  "let"
  "with"
] @append_space

; Keywords with surrounding space
[
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
] @prepend_space @append_space

; === Kind (entity/attribute/relation) needs trailing space ===
(kind) @append_space

; Space after type_ref when followed by an expression/var in has/isa constraints
(has_constraint
  (type_ref) @append_space
  .
  [(expression_value) (expression_list) (comparison) (var)]
)
(isa_constraint
  (type_ref) @append_space
  .
  [(value_literal)]
)

; Space between var and order direction
(var_order
  (var) @append_space
  .
  (order)
)

; === Operators with surrounding space ===
[
  "=="
  "!="
  ">="
  ">"
  "<="
  "<"
  "="
  "->"
  "contains"
  "like"
] @prepend_space @append_space

; Arithmetic operators with surrounding space
[
  "+"
  "*"
  "/"
  "^"
  "%"
] @prepend_space @append_space

; === Punctuation ===

; Semicolons: newline after
";" @append_hardline

; Commas in thing constraint lists: space after
(thing_constraint_list
  "," @append_space
)

; Commas in relations: space after
(relation
  "," @append_space
)

; Commas in fetch object entries: softline after
(fetch_object_entries
  "," @append_spaced_softline
)

; Commas in definition types (type capabilities): softline after
(definition_type
  "," @append_spaced_softline
)

; Colons in role players: space after
(role_player
  ":" @append_space
)

; Colons in fetch entries: space after
(fetch_object_entry
  ":" @append_space
)

; === Clause-level formatting ===

; Match clause: indent patterns
(clause_match
  "match" @append_empty_softline @append_indent_start
)
(clause_match
  (patterns) @append_indent_end
)

; Insert clause: indent patterns
(clause_insert
  "insert" @append_empty_softline @append_indent_start
)
(clause_insert
  (patterns) @append_indent_end
)

; === Define/redefine/undefine ===

(query_define
  "define" @append_empty_softline @append_indent_start
)
(query_define
  (definables) @append_indent_end
)

(query_redefine
  "redefine" @append_empty_softline @append_indent_start
)

(query_undefine
  "undefine" @append_empty_softline @append_indent_start
)

; === Braces and blocks ===

; Fetch object
(fetch_object
  "{" @append_spaced_softline @append_indent_start
  "}" @prepend_spaced_softline @prepend_indent_end
)

; Pattern conjunction (braces)
(pattern_conjunction
  "{" @append_empty_softline @append_indent_start
  "}" @prepend_empty_softline @prepend_indent_end
)

; Pattern disjunction (braces for each branch)
(pattern_disjunction
  "{" @append_empty_softline @append_indent_start
  "}" @prepend_empty_softline @prepend_indent_end
)

; Pattern negation
(pattern_negation
  "{" @append_hardline @append_indent_start
  "}" @prepend_hardline @prepend_indent_end
)

; Pattern try
(pattern_try
  "{" @append_hardline @append_indent_start
  "}" @prepend_hardline @prepend_indent_end
)

; === Relations ===
(relation
  "(" @append_empty_softline
  ")" @prepend_empty_softline
)

; === Pipeline stages ===
; Each query stage should start on a new line
(query_pipeline
  (query_stage) @prepend_hardline
)
; But not the first one
(query_pipeline
  . (query_stage) @prepend_empty_softline
)

; === Annotations ===
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

; === Multiple queries in a file ===
(source_file
  (query) @append_hardline
)
