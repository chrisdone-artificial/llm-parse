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
  test "Give me a comma-separated list of random integers, no other content."
       (let num = someP [Range '0' '9']
        in (:) <$> num <*> manyR (litR ", " *> num))
```

```haskell
  test "Generate a list of first names in a s-expression:"
       (let nam = someP [Range 'a' 'z', Range 'A' 'Z']
        in litR "(" *> ((:) <$> nam <*> manyR (litR " " *> nam)) <* litR ")")
```

Output:

```haskell
GBNF: root ::= [0-9]+ (", " [0-9]+ )*

Stream: 854, 219, 467, 982, 118

Parse: Just ["854","219","467","982","118"]
```

```haskell
GBNF: root ::= "(" [a-zA-Z]+ (" " [a-zA-Z]+ )* ")"

Stream: (alex andrew brian carol david emily franklin gerald hannah iris jasper katherine lauren luisa mae matthew natalie oscar pam peter quinn rachel samantha tessa upton victoria william xavier yearling zoe)

Parse: Just ["alex","andrew","brian","carol","david","emily","franklin","gerald","hannah","iris","jasper","katherine","lauren","luisa","mae","matthew","natalie","oscar","pam","peter","quinn","rachel","samantha","tessa","upton","victoria","william","xavier","yearling","zoe"]
```
