# Chess Coding Challenge (C#) Example

Also known as Negamax Tier 2, this is an example bot for Seb Lague's
[Chess Coding Challenge](https://youtu.be/iScy18pVR58), it implements
only the most basic features for a functional chess engine. Little effort
has been made to optimise for tokens, apart from implementing Quiescence
Search inside the normal search function (rather than in a separate function).

Additionally this repository contains my Neural Network bot, on [this](https://github.com/jw1912/Chess-Challenge/tree/nn) branch.

### Search
- Alpha-Beta Negamax
- Quiescence Search
- Iterative Deepening
- Transposition Table (Ordering & Cutoffs)
- MVV-LVA for Captures

### Evaluation
- Quantised & Compressed PeSTO Piece-Square Tables
