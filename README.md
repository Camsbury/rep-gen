# rep-gen - a repertoire generator

### Motivation
Too many hours have been spent just sitting in front of a [Lichess analysis board](https://lichess.org/analysis) trying to piece together [opening repertoires](https://www.wikiwand.com/en/Chess_opening#Opening_repertoires). This program is here to automate the whole process driven by a few key hyperparameters.

### Key Parameters
#### Color
Are you generating for white or black...
#### Ratings
Choice of Lichess average ratings as seen in the opening explorer
#### Speeds
Choice of Lichess time controls as seen in the opening explorer
#### Masters?
Are we using masters' data to inform decision making? There is less data, but much higher quality games and vastly different win rates.

### Key Hyperparameters
#### Minimum Aggregate Probability
How deep the algorithm actually collects moves, based on empirical probabilities rather than just move depth. As an example a `min-prob-agg` of `0.01` would mean that each line is seen at least 1% of the time while playing on [Lichess](lichess.org), all else being equal.
#### *Initial/Asymptotic Candidate Breadth
How many candidates to consider at each branch after a superficial filtering and sorting
#### *Initial/Asymptotic Response Probability
The minimal probability that a response will occur for us to consider it when analyzing a branch
#### Strategy
This is the means for deciding the best move to make given a list of candidates. Includes one optimizer and optionally many satisficers. An example optimizer is `MinLoss`, where you are just minimizing the loss of a given line.
#### Engine Allowable Loss (Part of the `EngineFilter` Satisficer)
A value of `0.85` will allow the normalized score for a line to drop to 85% of its peak and still be accepted as a candidate.


##### *These types of parameters are fit with an exponential curve to determine how they evolve as the algorithm delves into smaller `Aggregate Probabilibies`, in order to do exponentially less work as we encounter exponentially more cases.

### "I'm skeptical"
#### Isn't spending too much time learning openings counterproductive?
Although it is true that obsessing over memorizing opening moves is not the most valuable use of most chess players' time, the goal here is to produce a meaningful tree of moves that can be relied upon to learn useful patterns at different stages of the opening, and to provide solid choices for the first couple of moves (which are certainly worth memorizing)

#### How do I know which line is most important to study?
Every line of the tree has been pruned according to a minimum `Aggregate Probability,` i.e. the chance that you will actually run into the line in the wild. Because of this, each line has equal importance at the leaves. If you want to learn incrementally, you are best served studying the longest lines first (though still likely breadth-first to a certain degree).

#### How do I actually run this?
 - [Nix](https://github.com/NixOS/nix) is used to get everything you need and I don't care enough to support any other means
 - Also don't care about productionizing, so just running everything in a `lorri` and `direnv` environment with `cabal run`
 - Pass JSON to update default values, as described by the `FromJSON` instance in `src/RepGen/Config/Type.hs`
 - In the current state, `scr/core.clj` is used to have a nice repl experience calling the above program with clojure data structures and performing analytics on the results

#### This just outputs a PGN. How is that useful?
You can import it into a [Lichess analysis board](https://lichess.org/analysis) to explore or into a tool like [the ChessTempo opening trainer](https://chesstempo.com/opening-training/) to really drill it

#### Why the amalgam of languages?
 - Haskell is my preferred choice for the core algorithm because it quite simply has the best developer experience when both describing a problem and refactoring code of all the languages I've used
 - Clojure is my preferred choice for working around the edges of the application because it is the best developer experience with quickly analyzing and manipulating data
 - Python is used for chess-specific utilities because [an amazing chess library](https://github.com/niklasf/python-chess) already exists
 - C is used to let me easily call Python from Haskell!
