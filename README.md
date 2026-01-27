# llm-parse

Express simplified GBNF grammar in Haskell, submit it to llama.cpp,
parse the output responses.

See where that leads. -  _-

Grammar and prompt combo:

```haskell
  let rules = (:) <$> num <*> manyR (litR ", " *> num)
        where num = someP [Range '0' '9']
  let prompt :: String =
        "Give me a comma-separated list of integers, no other content."
```

Output:

```
> main

GBNF: root ::= [0-9]+ (", " [0-9]+ )*

Stream: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

Parse: Just ["1","2","3","4","5","6","7","8","9","10"]
> main

GBNF: root ::= [0-9]+ (", " [0-9]+ )*

Stream: 1, 2, 3, 4, 5

Parse: Just ["1","2","3","4","5"]
> main

GBNF: root ::= [0-9]+ (", " [0-9]+ )*

Stream: 1, 2, 3, 4, 5

Parse: Just ["1","2","3","4","5"]

```
