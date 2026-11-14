# Felicific signatures

A measure of sentiment for which granularity holds. Every word carries a positive charge, in hedons, and a negative charge, in dolors, taken from human word ratings. A span of text is summarised by 19 numbers, its signature. Signatures of adjacent spans merge exactly, so one pass over a book gives the score of every chapter, page and sentence, and no result depends on how the text was cut. The name comes from the felicific calculus, Bentham's proposal to measure and add pleasures and pains, whose units, hedons and dolors, the measure uses. The project began as the Sentiment Gummy Worm, which is why the Python package is called `sgw`.

- Paper: [paper/sgw.pdf](paper/sgw.pdf), "A mergeable measure of sentiment"
- Analysis plan, committed before any evaluation data were scored: [ANALYSIS_PLAN.md](ANALYSIS_PLAN.md)
- Result files behind every number and figure: [results/](results)
- Reports from the AI referee agents that reviewed the draft: [reviews/](reviews)

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
sgw.span(pre, 120, 480)          # sentences 120 to 480 from two rows: counts, totals, density, level
sgw.fold(S[120:481])             # the same span by merging its own rows: use this for the arc or the valence
```

Signatures merge exactly where a text is cut between sentences. In floating point the two-row query keeps counts and totals at any depth and loses the position sums of a short span deep in a long text, so read the arc or the predicted rating of such a span from `fold`. Rows from `sgw.sketch_exact` are integers, and for them both routes are exact.

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
| Mean Pearson r with human ratings (five rated corpora, constants fitted on other corpora) | signature 0.51; VADER 0.49 on whole texts and 0.49 averaged over sentences; TextBlob 0.44; hedonometer 0.42; DistilBERT 0.59; Twitter-RoBERTa 0.68 |
| Sentences per second on one processor core | signature 167,000; VADER 27,900; DistilBERT 37; DistilBERT on an RTX 3090 12,200 |
| One billion sentences | 1.7 hours on one core, 21 minutes with twelve processes on six cores |
| Merged against single-pass signatures, 356 novels | relative difference below 2e-13 in floating point; exact in integers |
| Chapters of novels | between-chapter variance 5.9 times the shuffled-sentence value (median) |
| VADER book score by chunk size | 0.11 with one-sentence chunks, 0.76 with whole chapters |
| Order of two parts of a phrase, length held fixed | weight of the later part minus the earlier at equal length -0.138 [-0.182, -0.090]; the raw later-part advantage is a length effect |
| Neutral words | dilute on either side of the feeling; planned test supported in 4 of 6 corpora |
| Human against machine essays, ten tone measures | AUC 0.74 plain, 0.64 when the model writes as a student; 0.63 for ChatGPT answers |
| The dial | target against achieved value, mean r 0.64 with the dial and 0.08 without; no gain in resemblance to student essays over a plain revision was detected |
<!-- results:end -->

## What held and what did not

- Merging is exact. In integer arithmetic the merged signature equals the single-pass signature to the last digit, and the tests check it.
- Agreement with human ratings is level with VADER, the best of three lexicon methods tested, on Pearson r and on Spearman rho, whether VADER scores the whole text or its sentences, and below two off-the-shelf transformer classifiers. On whole film reviews a simpler lexicon method (TextBlob) does better. The case for the signature is speed, composition and auditability.
- Merging removes the dependence on how a text is cut. It does not make the score more valid: on long texts an average of sentence scores from another method tracked human ratings better.
- The 2024 hypothesis that feeling words come first is refuted as a claim about placement. In sentences, tweets and posts charged words gather at the end; in reviews and novel chapters they are spread almost evenly.
- Whether early words weigh more with readers depends on the genre. They do in film-review sentences; the reverse holds in tweets and Yelp reviews.
- A recency hypothesis written during the work passed its three planned tests and is withdrawn. Two of the tests compared parts of unequal length and reversed once length was held fixed. A referee agent found this.
- Neutral words dilute a rating, from either side of the feeling.
- Word charges refined on rated text help on reviews and hurt on mixed genres, and they pick up attitudes toward groups of people from the training text. They are not shipped.
- The standard error of a level is not a prediction interval for a human rating.
- Machine essays put less of their feeling after intensifiers than student essays on the same assignments and, on the planned measure, close on a more positive note. The separation is modest and is not a detector.
- A model can be steered part of the way toward an assigned profile of the measures (the dial). No gain in resemblance to student writing over a plain revision was detected, and the model judge's verdict on that is confounded by spelling.
- Rewrites with two anti-slop prose skills moved the tone measures no more than a plain revision did.

## Tone check for drafts

```bash
python3 -m sgw tone draft.txt --genre essay     # essay, answer, review or fiction
python3 -m sgw tone draft.txt --json
```

The report names each tone measure on which a draft lies outside the middle 80% of human texts of that kind. Two ways to give it to a language model:

- MCP server: `claude mcp add sgw -- python3 /absolute/path/to/integrations/mcp_server.py` (needs `pip install mcp`). Tools: `score_text`, `tone_check`, `tone_numbers`.
- Skill: copy `integrations/claude-skill/sgw-tone` into `~/.claude/skills/`.

Use it to check tone in writing whose use of a model is disclosed. It is not built to hide authorship, and the measures are too weak to serve as a detector. The word ratings carry their raters' attitudes: words such as *deaf*, *disabled*, *blind* and *old* are rated negative, so a text that mentions disability or age scores lower for that alone. Do not use the scores to judge people or groups.

## Reproduce

```bash
pip install -e ".[refine]" vaderSentiment textblob transformers torch pandas pyarrow matplotlib SciencePlots scipy
python3 scripts/fetch_data.py          # lexicons and corpora into data/ (not redistributed)
python3 scripts/build_lexicon.py
python3 tests/test_core.py
python3 scripts/exp_transformers.py    # slow on a CPU; scores are cached in data/cache
python3 scripts/exp_refine.py
python3 scripts/exp_accuracy.py        # also writes sgw/constants.json; `exp_accuracy.py vader_sentences` adds one comparator without refitting
python3 scripts/exp_position.py where kernel emobank dilute tree confirm parts
python3 scripts/exp_books.py novels tales critics parts
python3 scripts/exp_uncertainty.py
python3 scripts/exp_machine.py prepare samplejobs hc3 essays reference revise dial
python3 scripts/exp_speed.py           # throughput; scripts/exp_gpu.py for a CUDA graphics card
python3 scripts/exp_speed.py readout   # cost of the calibrated readout relative to the level
python3 scripts/make_numbers.py && python3 scripts/make_figures.py
cd paper && pdflatex sgw && bibtex sgw && pdflatex sgw && pdflatex sgw
```

Things to know before rerunning:

- Clear `data/cache` first. Cached signatures are keyed by name only, so a cache made by older code is reused silently.
- The transformer checkpoints and the corpora are fetched at their current versions; no revision is pinned, and no checksum manifest is shipped.
- The machine-written essays, revisions and judgements are in `machine_text/`, with each agent's task in the job files, the dispatching scripts in `machine_text/workflows/`, and the final message of every request that left no text in `machine_text/declined_requests.json`. Regenerating them needs a language model and will not reproduce the same texts. Full transcripts are not included.
- The throughput figures came from a rented six-core machine; `results/speed.json` records it.
- The results of the first scoring run, before the deviations listed in the plan, were not kept.

## Layout

| Path | Contents |
|---|---|
| `sgw/core.py` | scan, signature, merge, prefix queries, readouts |
| `sgw/tone.py` | ten measures of how a text carries feeling, and the comparison with human ranges |
| `sgw/refine.py` | convex refinement of word charges (experimental; the refined lexicon is not shipped) |
| `sgw/lexicon.tsv` | open word charges in thousandths of a hedon |
| `scripts/` | data fetching, experiments, numbers and figures |
| `tests/test_core.py` | exact merge and prefix queries, batch against single text, float spans at depth, standard error |
| `integrations/` | MCP server and skill |
| `machine_text/` | machine essays, revisions, judging keys, workflow scripts |
| `reviews/` | reports from AI referee agents on the draft |
| `legacy/2024/` | scripts and data of the 2024 version, kept as an archive |

## History

The 2024 version predicted an average lexicon score with a random forest. A February 2026 version on the `feature/sgw-v3-plus` branch scored words by embedding projection and did not match VADER on its own benchmark. This version keeps the aim, a number for feeling built from word values, position and buffer words, and replaces the method.

## Limits

Word ratings describe words and know nothing of events, so narrative text is best read against its own baseline. Irony and sarcasm are missed, a word has one charge whatever its sense, and the ratings carry social bias (see above). The lexicon is English, and 69% of its charged entries come from VADER's list. No human ratings of chapters or books were available, so validity above the sentence rests on reviews and fairy tales, where the signature did worse than averaged sentence scores. The chapter rule for novels is approximate. The writing agents in the human and machine comparison were not blind to the project. The analysis plan was committed in a local repository over one evening and pushed when the study was complete, so its timing rests on the author's record. The full list is in the paper's Discussion.

## Sources and licences

- Word charges: labMT 1.0 (Dodds et al. 2011, CC BY) and the VADER word list and emoji names (Hutto and Gilbert 2014, MIT), combined in `scripts/build_lexicon.py`.
- Corpora are downloaded by `scripts/fetch_data.py` and are not redistributed. PERSUADE 2.0 is CC BY 4.0; samples are identified by essay id in `machine_text/sample.csv`, and four student essays are quoted in `machine_text/side_by_side.html`.
- Full attributions and licence texts: [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md).
- Code: see [LICENSE](LICENSE).

## Tools

Figures use Matplotlib with SciencePlots. The analysis procedure drew on the scientific-critical-thinking, scientific-writing and scientific-brainstorming skills of Scientific Agent Skills (Kassis et al. 2026, arXiv:2609.00065). Code, analyses and the manuscript draft were produced with Claude under the author's direction; see the paper's Methods.
