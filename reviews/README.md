# Referee reports

Eight reports on drafts of the paper, written on 13 November 2026 by AI referee agents (Claude), each reading the paper, the code and the result files from one angle. Every finding was then checked by one or two further agents instructed to refute it: 48 findings were raised and none was refuted outright (most were confirmed, some in part).

The reports describe the draft as it stood when each was written, under its earlier title. The paper has been revised since, so quotations in them may no longer match the text.

| Report | Angle | What changed in response |
|---|---|---|
| [statistics.md](statistics.md) | design and inference | Recency withdrawn (two of its three tests were confounded by the length of the parts); hypotheses reported by their planned rules; Spearman's rho added; jackknife intervals for eight-cluster analyses |
| [claims.md](claims.md) | claims against evidence | Title and abstract narrowed; weaker results on long texts stated; HC3 reported by source; constants shown by fit |
| [reproducibility.md](reproducibility.md) | can it be rerun and checked | Keys redacted in the tree; licences and notices; job files and workflow scripts shipped; deviations listed; the limits of the pre-specification record stated |
| [mathematics.md](mathematics.md) | proofs and formulas | Standard error of the level corrected; merge conditions stated; float precision of span queries documented and tested; proof for relative-position weights added |
| [code.md](code.md) | implementation | Tokeniser edge case fixed; tests extended; group-term check moved into a script |
| [domain.md](domain.md) | sentiment analysis practice | Sentence-averaged VADER added as a comparator; transformer rows described as off-the-shelf; social bias, word sense and review boilerplate added to the limits |
| [machine-writing.md](machine-writing.md) | the human and machine comparison | Judge result reported with its spelling confound; refusal messages published; intensifier finding rechecked on a stricter list; claims about steering narrowed |
| [writing.md](writing.md) | prose and presentation | Placement claim restated; sections reordered; overloaded terms and several figures fixed |

Not done: a clean rerun of every analysis from an empty cache, stripping credits and footers from the film reviews, human ratings above the sentence, and a human judge for the essays. The paper's Discussion and Methods list these among its limits.
