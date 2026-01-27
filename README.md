# llm-parse

Express simplified GBNF grammar in Haskell, submit it to llama.cpp,
parse the output responses.

It's an experiment with dealing with a local LLM (a small model that
is fast) in a Haskelly way. Write a grammar with a regex-applicative
style mini DSL, which then generates both the GBNF syntax for
llama.cpp to reject tokens at-source (fast and enforces right shape),
and then separately the output is parsed into the expected structure
on the Haskell side (correct, no schema mismatch).

The system prompt / user prompt should indicate the expected format to
match training data, which should amount to a human-language
description of the grammar. It might also be possible to generate that
human-language description from the grammar itself, reducing
duplication.

The usefulness of something like this to say:

* Classify some arbitrary unknown text into the given set of
  categories of interest
* Translate some loose text into a structured format.
* Quick and easy sentiment analysis.
* Auto-complete a precise next step given a context.
* Given a prompt, output a set of tool uses that might be needed to
  achieve a goal (e.g. auto-search, MCP, RAG, etc.)
* And so on.

Most of these are things an LLM can do out of the box because they're
trained on the internet. What LLMs are rubbish at is producing
something structured **_reliably_**, which is why GBNF is so
powerful. Haskell's very good at grammars and parsers, so this is
easy.

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
