# Sentiment Gummy Worm

A measure of sentiment for which granularity holds. Every word carries a positive charge, in hedons, and a negative charge, in dolors, taken from human word ratings. A span of text is summarised by 19 numbers, its signature. Signatures of adjacent spans merge exactly, so one pass over a book gives the score of every chapter, page and sentence, and no result depends on how the text was cut.

- Paper: [paper/sgw.pdf](paper/sgw.pdf)
- Analysis plan, committed before any evaluation data were scored: [ANALYSIS_PLAN.md](ANALYSIS_PLAN.md)
- Result files behind every number and figure: [results/](results)

## Quick start

```bash
pip install -e .
```

```python
import sgw

s = sgw.sketch(["A lovely day.", "Not good, but the pasta was wonderful."])
sgw.totals(s)                    # hedons, dolors and words of each text
whole = sgw.merge(s[0], s[1])    # the two read as one text; equals sgw.sketch of the joined text
sgw.read("Not good, but the pasta was wonderful.")
```

```bash
python3 -m sgw "Not good, but the pasta was wonderful."
python3 -m sgw path/to/book.txt
```

For a long text, score its sentences once and read any span afterwards:

```python
S = sgw.sketch(sentences)        # one row per sentence
pre = sgw.prefix(S)              # prefix table
sgw.span(pre, 120, 480)          # signature of sentences 120 to 480, from two rows
```

## Readouts

| Readout | What it is | Unit |
|---|---|---|
| `totals` | hedons, dolors, words | hedons, dolors |
| density | net charge per 1,000 words | hedons per 1,000 words; negative means dolors |
| `level` | net charge over exposure `a + charged words + kappa * buffer words` | per 1,000 units of exposure |
| `stderr` | standard error of the level from the spread of word charges | same |
| `valence` | predicted human rating, fitted on six rated corpora | standard deviations |
| `arc` | cubic fit of net charge over relative position | per 1,000 words |

One hedon is the charge of a word that every rater places at the top of the rating scale. A buffer word is a word rated within a quarter of the scale from neutral; it carries no charge.

## Results in brief

<!-- results:start -->
| | |
|---|---|
| Mean Pearson r with human ratings (five rated corpora, constants fitted on other corpora) | signature 0.51; VADER 0.49; TextBlob 0.44; hedonometer 0.42; DistilBERT 0.59; Twitter-RoBERTa 0.68 |
| Sentences per second on one processor core | signature 167,000; VADER 27,900; DistilBERT 37; DistilBERT on an RTX 3090 12,200 |
| One billion sentences | 1.7 hours on one core, 21 minutes on twelve |
| Merged against single-pass signatures, 356 novels | relative difference below 1e-13 in floating point; exact in integers |
| Chapters of novels | between-chapter variance 5.9 times the shuffled-sentence value (median) |
| VADER book score by chunk size | 0.11 with one-sentence chunks, 0.76 with whole chapters |
| Recency, three confirmatory tests | 3 of 3 supported |
| Human against machine essays, ten tone measures | AUC 0.74 plain, 0.64 when the model writes as a student; 0.63 for ChatGPT answers |
| The dial | target against achieved value, mean r 0.64 with the dial and 0.08 without; a blind judge found the dial revision more student-like in 10% of pairs |
<!-- results:end -->

## What held and what did not

- Merging is exact. In integer arithmetic the merged signature equals the single-pass signature to the last digit, and the tests check it.
- Agreement with human ratings is level with the best lexicon method and below fine-tuned transformers. The case for the signature is speed, composition and auditability.
- The 2024 hypothesis that feeling words come first is refuted. Charged words sit slightly late, and later parts weigh more in human phrase ratings. A recency hypothesis written afterwards passed three confirmatory tests on untouched data.
- Neutral words dilute a rating, but mainly when they follow the feeling.
- Word charges refined on rated text help on reviews and hurt on mixed genres.
- The standard error of a level is not a prediction interval for a human rating. Error against raters did not shrink with text length.
- Machine essays differ from student essays chiefly in an upbeat ending and in sparse use of intensifiers and negation. The separation is modest and is not a detector.
- A model can be steered to an assigned profile of the measures (the dial). That did not make its essays read more like student writing.
- Rewrites with two anti-slop prose skills moved the tone measures no more than a plain revision did.

## Tone check for drafts

```bash
python3 -m sgw tone draft.txt --genre essay     # essay, answer, review or fiction
python3 -m sgw tone draft.txt --json
```

The report names each tone measure on which a draft lies outside the middle 80% of human texts of that kind. Two ways to give it to a language model:

- MCP server: `claude mcp add sgw -- python3 /absolute/path/to/integrations/mcp_server.py` (needs `pip install mcp`). Tools: `score_text`, `tone_check`, `tone_numbers`.
- Skill: copy `integrations/claude-skill/sgw-tone` into `~/.claude/skills/`.

Use it to check tone in writing whose use of a model is disclosed. It is not built to hide authorship, and the measures are too weak to serve as a detector or to defeat one.

## Reproduce

```bash
pip install -e ".[refine]" vaderSentiment textblob transformers torch pandas pyarrow matplotlib SciencePlots
python3 scripts/fetch_data.py          # lexicons and corpora into data/ (not redistributed)
python3 scripts/build_lexicon.py
python3 tests/test_core.py
python3 scripts/exp_transformers.py    # slow on a CPU; scores are cached in data/cache
python3 scripts/exp_refine.py
python3 scripts/exp_accuracy.py        # also writes sgw/constants.json
python3 scripts/exp_position.py        # where, kernel, emobank, dilute, tree, confirm
python3 scripts/exp_books.py           # novels, tales, critics, parts
python3 scripts/exp_uncertainty.py
python3 scripts/exp_machine.py hc3 essays reference revise dial
python3 scripts/exp_speed.py           # throughput; scripts/exp_gpu.py for the graphics card
python3 scripts/make_numbers.py && python3 scripts/make_figures.py
cd paper && pdflatex sgw && bibtex sgw && pdflatex sgw && pdflatex sgw
```

The machine-written essays and revisions are in `machine_text/`, with the exact instruction given to each writing agent in the job files. Regenerating them needs a language model and will not reproduce the same texts.

## Layout

| Path | Contents |
|---|---|
| `sgw/core.py` | scan, signature, merge, prefix queries, readouts |
| `sgw/tone.py` | ten length-free measures and the comparison with human ranges |
| `sgw/refine.py` | convex refinement of word charges |
| `sgw/lexicon.tsv` | open word charges in thousandths of a hedon |
| `scripts/` | data fetching, experiments, numbers and figures |
| `tests/test_core.py` | exactness of merge, prefix queries and readouts |
| `integrations/` | MCP server and skill |
| `machine_text/` | machine essays, revisions, judging keys |
| `legacy/2024/` | scripts of the 2024 version |

## History

The 2024 version predicted an average lexicon score with a random forest. A February 2026 version on the `feature/sgw-v3-plus` branch scored words by embedding projection and did not match VADER on its own benchmark. This version keeps the aim, a number for feeling built from word values, position and buffer words, and replaces the method.

## Limits

Word ratings describe words, not events, so narrative text is best read against its own baseline. Irony and sarcasm are missed. The lexicon is English. No human ratings of chapters or books were available, so validity above the sentence rests on reviews and fairy tales. The full list is in the paper's Discussion.

## Sources and licences

- Word charges: labMT 1.0 (Dodds et al. 2011, CC BY) and the VADER word list and emoji names (Hutto and Gilbert 2014, MIT), combined in `scripts/build_lexicon.py`.
- Corpora are downloaded by `scripts/fetch_data.py` and are not redistributed. PERSUADE 2.0 is CC BY 4.0; samples are identified by essay id in `machine_text/sample.csv`.
- Code: see [LICENSE](LICENSE).

## Tools

Figures use Matplotlib with SciencePlots. The analysis procedure drew on the scientific-critical-thinking, scientific-writing and scientific-brainstorming skills of Scientific Agent Skills (Kassis et al. 2026, arXiv:2609.00065). Code, analyses and the manuscript draft were produced with Claude under the author's direction; see the paper's Methods.
