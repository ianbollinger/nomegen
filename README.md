nomegen is a library and command-line utility for randomly generating names or
words for a constructed language.

# Command-line usage

```
nomegen [-o|--out OUT] [-n|--number N] [-l|--lowercase] FILE
```

Generates `N` random names using the provided Nomicon `FILE`.

## Available options
- `-h`, `--help`: Show the help text.
- `-o`, `--out OUT`: The file to write the generated names to.
- `-n`, `--number N`: The number of names to generate (default: 1).
- `-l`, `--lowercase`: Do not capitalize names.
- `FILE`: The Nomicon file to use.
