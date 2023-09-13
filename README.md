# Chess Coding Challenge (C#) Example

### Compilation
To compile a self-contained UCI executable, first identify the appropriate runtime identifier:
- `win-x64` for Windows
- `linux-x64` for Linux

for other operating systems/errors, refer to the [docs](https://learn.microsoft.com/en-us/dotnet/core/rid-catalog).

Then run
```
make OS=<runtime identifier>
```

### Search

#### Core
- Alpha-Beta Negamax
- Quiescence Search
- Iterative Deepening

#### Move ordering
1. Hash Move
2. Captures (MVV-LVA)
3. Killer Moves
4. Quiets

#### Selectivity
- Check Extensions
- Internal Iterative Deepening


### Evaluation
- `768 -> 8x2 -> 1` Neural Network trained using a modified version of my trainer, [bullet](https://github.com/jw1912/bullet/tree/seb).
