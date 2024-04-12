" syntax highlighting for Pure (github.com/prog-lang/purist)

if exists('b:current_syntax')
  finish
endif

" Keywords
syn keyword pureConditional else if when where is then can
syn keyword pureAlias alias
syn keyword pureTypedef contained type
syn keyword pureImport export as import module where

" Operators
" pure/core
syn match pureOperator contained "\(<|\||>\|||\|&&\|==\|:=\|/=\|<=\|>=\|++\|::\|+\|-\|*\|/\|//\|^\|<>\|>>\|<<\|<\|>\|%\)"
" pure/parser
syn match pureOperator contained "\(|.\||=\)"
" pure/url
syn match pureOperator contained "\(</>\|<?>\)"

" Types
syn match pureType "\<[A-Z][0-9A-Za-z_-]*"
syn keyword pureNumberType number

" Modules
syn match pureModule "\<\([a-z][0-9A-Za-z_'-\.]*\)\+\.[A-Za-z]"me=e-2
syn match pureModule "^\(module\|import\)\s\+[a-z][0-9A-Za-z_'-\.]*\(\s\+as\s\+[A-Z][0-9A-Za-z_'-\.]*\)\?\(\s\+exposing\)\?" contains=pureImport

" Delimiters
syn match pureDelimiter  "[,;]"
syn match pureBraces  "[()[\]{}]"

" Functions
syn match pureTupleFunction "\((,\+)\)"

" Comments
syn keyword pureTodo TODO FIXME XXX contained
syn match pureLineComment "--.*" contains=pureTodo,@spell
syn region pureComment matchgroup=pureComment start="{-|\=" end="-}" contains=pureTodo,pureComment,@spell fold

" Strings
syn match pureStringEscape "\\u[0-9a-fA-F]\{4}" contained
syn match pureStringEscape "\\[nrfvbt\\\"]" contained
syn region pureString start="\"" skip="\\\"" end="\"" contains=pureStringEscape,@spell
syn region pureTripleString start="\"\"\"" skip="\\\"" end="\"\"\"" contains=pureStringEscape,@spell
syn match pureChar "'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'"

" Lambda
syn region pureLambdaFunc start="\\"hs=s+1 end="->"he=e-2

" Debug
syn match pureDebug "Debug.\(log\|todo\|toString\)"

" Numbers
syn match pureInt "-\?\<\d\+\>"
syn match pureFloat "-\?\(\<\d\+\.\d\+\>\)"

" Identifiers
syn match pureTopLevelDecl "^\s*[a-zA-Z][a-zA-z0-9_]*\('\)*\s\+:\(\r\n\|\r\|\n\|\s\)\+" contains=pureOperator
syn match pureFuncName /^\l\w*/

" Folding
syn region pureTopLevelTypedef start="type" end="\n\(\n\n\)\@=" contains=ALL fold
syn region pureTopLevelFunction start="^[a-zA-Z].\+\n[a-zA-Z].\+=" end="^\(\n\+\)\@=" contains=ALL fold
syn region pureCaseBlock matchgroup=pureCaseBlockDefinition start="^\z\(\s\+\)\<case\>" end="^\z1\@!\W\@=" end="\(\n\n\z1\@!\)\@=" end="\n\z1\@!\(\n\n\)\@=" contains=ALL fold
syn region pureCaseItemBlock start="^\z\(\s\+\).\+->$" end="^\z1\@!\W\@=" end="\(\n\n\z1\@!\)\@=" end="\(\n\z1\S\)\@=" contains=ALL fold
syn region pureLetBlock matchgroup=pureLetBlockDefinition start="\<let\>" end="\<in\>" contains=ALL fold

hi def link pureFuncName Function
hi def link pureCaseBlockDefinition Conditional
hi def link pureCaseBlockItemDefinition Conditional
hi def link pureLetBlockDefinition TypeDef
hi def link pureTopLevelDecl Function
hi def link pureTupleFunction Normal
hi def link pureTodo Todo
hi def link pureComment Comment
hi def link pureLineComment Comment
hi def link pureString String
hi def link pureTripleString String
hi def link pureChar String
hi def link pureStringEscape Special
hi def link pureInt Number
hi def link pureFloat Float
hi def link pureDelimiter Delimiter
hi def link pureBraces Delimiter
hi def link pureTypedef TypeDef
hi def link pureImport Include
hi def link pureConditional Conditional
hi def link pureAlias Delimiter
hi def link pureOperator Operator
hi def link pureType Type
hi def link pureNumberType Identifier
hi def link pureLambdaFunc Function
hi def link pureDebug Debug
hi def link pureModule Type

syn sync minlines=500

let b:current_syntax = 'pure'