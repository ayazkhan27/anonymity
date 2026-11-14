# Referee report: code correctness

Lens: `sgw/core.py`, `sgw/tone.py`, `sgw/refine.py`, `scripts/common.py`, `scripts/exp_*.py`, `scripts/make_numbers.py`, `tests/test_core.py`.
Date of review: 2026-11-13, on the working tree as found after the crash (HEAD `a62f3b4` plus uncommitted changes).

## What I examined

Read in full: `sgw/core.py`, `sgw/tone.py`, `sgw/refine.py`, `sgw/__init__.py`, `sgw/__main__.py`, `scripts/common.py`, `corpora.py`, `exp_accuracy.py`, `exp_refine.py`, `exp_uncertainty.py`, `exp_books.py`, `exp_position.py`, `exp_machine.py`, `exp_transformers.py`, `exp_speed.py`, `build_lexicon.py`, `make_numbers.py`, `tests/test_core.py`, and the manuscript sources `abstract.tex`, `body_a/b/c.tex`, `methods.tex`, `extended.tex`, `textmacros.tex`, `numbers.tex`. Read the relevant lines of `ANALYSIS_PLAN.md` (measures, R3, HD1 to HD3) and `git status` / `git diff HEAD`.

Ran only single-core checks of under one second each, all read-only on the repository (scratch files in the session scratchpad):

- `tests/test_core.py`: 5 of 5 pass in 0.8 s.
- All 15 `results/*.json`, `sgw/constants.json`, `sgw/reference.json` parse; none is truncated.
- Signature caches `data/cache/sig_{semeval,emobank,fb}_{dev,test}.npy` are bit-identical to signatures recomputed with the current `core.py` and lexicon. The stored leave-one-corpus-out coefficients reproduce the stored test correlations for those three corpora (SGW 0.7032 / 0.4800 / 0.6166; level 0.7074 / 0.4373 / 0.5714).
- A copy of `make_numbers.py` writing to the scratchpad reproduces `paper/table_*.tex`, `excerpt.tex`, the README block and 731 of 738 macros of `paper/numbers.tex`; 7 differ (concern 5).
- Synthetic precision tests of `prefix` / `span` in floating point, `arc` against a direct least-squares fit, batch against single-text scoring, lexicon reachability, Alm label inventory, judge answer values, an exact last-fifth recomputation of ending lift on the 158 + 160 essay pairs, and a formatting scan of 6,000 HC3 records.

Things that checked out and need no change: operator scopes and window lengths agree between `scan` and `sketch_exact`; word positions are zero-based in both and the binomial shift in `_shift` is right; the Legendre change of basis in `legendre` is right; sign conventions for the position kernel are consistent (x = -1 first word, +1 last; contrast = first third minus last third) in `exp_position.kernel`, `exp_uncertainty.constants_ci` and the text; `resamples` draws whole groups and all methods share draws (same seed), so paired differences are paired; leave-one-corpus-out in `exp_accuracy.py` uses only the other corpora's train and dev portions for `a`, `kappa`, coefficients and level constants; the classifier in `exp_machine.auc` fits imputer, scaler and model inside each fold with prompts kept whole; the Alm files use exactly the labels in `VAL` (all 15,302 sentences kept); the judge file contains only answers 1 and 2, so the win coding in `dial()` is right; HD3's rule in code matches the plan; the "Dear Principal," first lines of machine essays do not move any measure (d unchanged to 0.01 with them removed); HC3 clitic rejoining leaves only stray "gon na" (33 in 6,000 answers).

## Concerns

### 1. Float `span()` returns wrong position moments, and so wrong valence and arc, for short spans inside book-length texts (major)

`core.span` computes `unmerge(pre[j], pre[i-1])`, which subtracts prefix power sums and shifts by a negative length. In double precision the cubic sum cancels catastrophically. Synthetic test with the shipped constants, one-sentence spans read from a float prefix table, last 300 sentences:

| words before the span | rms error of `valence(span)` (true s.d. about 0.6) | rms error of `arc` (true rms about 0.2) | density |
|---|---|---|---|
| 18,500 | 8e-4 | 3e-3 | exact to 1e-13 |
| 186,000 | 16 | 66 | exact to 1e-12 |
| 926,000 | 11,000 | 45,000 | exact to 1e-11 |

At 200,000 words (the length of *Moby-Dick*) the cubic moment of a sentence has a median relative error of 2.4, and at one million words a 1,000-word span's arc is off by 23%. Totals and density are fine. Chapter signatures built by forward merging (`prefix(...)[-1]`, as in `exp_books.py`) are also fine: `merge_err_max` = 1.3e-13 tests only that path.

Methods has one sentence admitting the loss ("In floating point this difference loses precision for high moments of short spans deep inside long texts"). The abstract ("One pass over a book gives the score of every chapter and sentence"), the Results ("A table of prefix signatures ... gives the signature of any contiguous run of sentences from two of its rows"), the Discussion ("exactly in integer arithmetic and to rounding error in floating point") and the README example `sgw.span(pre, 120, 480)` do not carry the caveat, and the library returns the garbage silently. The only exact implementation is `sketch_exact`, a per-token Python loop; every throughput figure is for the float path. No implementation in the repository is both the fast one and the exact one, and `test_any_span_from_two_prefix_rows` exercises object-dtype integers on 400 sentences only.

Fix: accumulate per-text sums as int64 milli-units in `aggregate` (local positions keep them small) and build prefix tables in Python integers (object dtype), or have `span` merge local rows forward when the cancellation exceeds a tolerance; add a float-path test at 200,000 words; qualify the abstract and README ("totals and density of any span; position moments need the integer table").

### 2. The exposure claim contradicts its own result file, and the coverage panel is circular (major)

Text (`body_b.tex`): "in none of the six corpora did squared error fall as exposure rose (Extended Data Fig. a)"; caption: "observed error does not fall with exposure".

- `results/uncertainty.json`, EmoBank test portion, RMSE by exposure quartile: 0.949, 0.934, 0.786, 0.800. Squared error falls by 29% from the lowest to the highest quartile (about 250 texts per band). Yelp: 0.824, 0.766, 0.773, 0.783.
- The statement rests on `s1 = 0.0` in all six fits. In `exp_uncertainty.main` that term is fitted by NNLS on the pooled training residuals of the *other five* corpora, in-sample, with exposure ranging from about 4 (EmoBank) to about 600 (film reviews). It is a between-corpus slope, not a within-corpus one, and it says nothing about the target corpus.
- Panel b ("Share of ratings inside the 90% interval implied by the fitted variance"): `slope, icpt = np.polyfit(pred, y, 1)` and `scale = sqrt(mean(err**2) / mean(sd**2))` are both estimated on the target's test portion, and coverage is then counted on the same points. With `s1 = 0` the interval is ±1.645 × test RMSE, so 89 to 93% coverage is what any roughly normal residual gives. The panel carries no information about calibration.
- The quartile masks use `>= lo` and `<= hi`, so ties at a cut are counted in two bands.

Fix: fit the variance model within each corpus on its train portion (or on the pool with corpus fixed effects), fit the linear map and scale on the target's dev portion, report the within-corpus slope with an interval, and reword the sentence for EmoBank. Drop panel b or recompute it out of sample.

### 3. Essay verdicts are read from an 8-cluster percentile bootstrap; the jackknife the code now computes is not used (major)

`common.jack` was added with the docstring "where a percentile bootstrap over clusters is too narrow", and `exp_machine.essays` stores `d_jackknife` and `decisions_jackknife`. `make_numbers.py` reads neither; the `es*CI` macros and the HL verdicts still come from `paired(Xh, Xm, g)` with eight assignments.

| | bootstrap over 8 assignments (in the paper) | delete-one-assignment jackknife (in `machine.json`, unused) |
|---|---|---|
| net, plain essays | -0.23 [-0.40, -0.02], HL1 "refuted" | [-0.48, +0.03], undecided |
| net, written as a student | +0.29 [+0.08, +0.52], HL1 "supported" | [-0.01, +0.60], undecided |
| ending lift, plain | +0.60 [+0.24, +0.93] | [+0.13, +1.07] |
| negated, plain | -0.40 [-0.66, -0.16] | [-0.73, -0.06] |

The sentence "Overall positivity goes against the prediction for plain essays ... and with it when the model imitates a student" does not survive the jackknife. The same applies to `ci_prompts` in `revise()` and `dial()` (`ci_jackknife` is stored and unused; HD1 survives it: [+0.30, +0.82]). `auc()` has only the 8-cluster bootstrap.

Fix: emit the jackknife intervals as macros, state both, and report HL1 for essays as undecided; add a jackknife to `auc()`.

### 4. "Ending lift" is not the last fifth (major for the sentence that quotes it)

`tone.profiles`: `lift = 1000 * (arc(S, linspace(0.8, 1.0, 21)).mean(-1) - (H - D) / L)`. That is the mean of the cubic fit over the last fifth. As a weight on words it is `-0.2 + 0.72x + 3.6x² + 2.8x³` (x from -1 to 1): 6.9 at the last word, 2.1 at the 80% mark, still 0.4 at 65%, slightly negative at the start; a true last-fifth mean is 5 on the last fifth and 0 elsewhere. The plan and Methods define it correctly, but:

- `body_c.tex`: "The final fifth of a machine essay sits 16 per thousand words above the essay's own density, against 3 for the students". Recomputed exactly from per-word charges on the same essays: 13.7 against 2.0 (plain), 11.7 against 1.9 (as a student). Effect sizes: 0.51 instead of 0.60, and 0.43 instead of 0.52. The correlation between the two versions is 0.88 to 0.92. The direction holds; the quoted numbers are not of the final fifth.
- `tone.LABEL["ending_lift"]` ("net charge of the last fifth minus the text's mean") and the dial sheet given to the models ("how much more positive the last fifth is than the essay as a whole") describe a quantity the tool does not compute.

Fix: either compute the exact last fifth (two `bincount`s on `scan` output, as `exp_position.confirm` already does for IMDB) or reword the sentence, the label and the sheet to "the fitted cubic arc near the end".

### 5. The working tree is mid-revision: `numbers.tex` and the PDF are older than the results, and one result file has content no script writes (minor in size, matters for provenance)

- `paper/numbers.tex` and `sgw.pdf` are from 22:16; `results/machine.json`, `position.json`, `parts.json`, `refine.json` were rewritten at 22:53 to 22:56. Regenerating gives 7 different macros, all from the dial classifier (its training set changed from excluding `pick.k` to excluding `full.k`): `dlScoreControl` 0.43 → 0.44, `dlScoreDial` 0.49 → 0.50, `dDlScoreCI` [-0.04, +0.17] → [-0.05, +0.17], `dDlScoreCIessays`, `dlAucControl` 0.53 → 0.52, `dlAucDial` 0.59 → 0.57, `dlAucDraft` 0.72 → 0.70. Three of these are quoted in the text. No conclusion changes.
- `results/refine.json` holds `group_terms`, `group_terms_down` (14) and `group_terms_up` (2). No file in the repository computes them, and the next run of `exp_refine.py` (`save("refine", out)`) will delete them. Its first column is the raw norm value, but 9 of the 20 words (for example *men* 0.235, *gay* -0.14, *obama* 0.235) lie inside the neutral band and score 0 in the shipped scorer, so "down" relative to the norm is not "down" relative to what the scorer used.
- `machine.json` and `position.json` are updated section by section (`res = json.load(...)`, one key replaced, saved), so one file mixes sections produced by different versions of the script. `cached()` and `exp_refine`'s "finished folds are kept" key on a name only, with no hash of the lexicon or code. The three signature caches I recomputed are current; the refined-charge predictions (written 20:38 to 21:03, before the signature caches) I could not recheck.
- Git index: a staged rename `sgw/lexicon_refined.tsv -> data/lexicons/lexicon_refined.tsv`. `data/` is in `.gitignore` and the code comment says the refined lexicon is "not shipped (it absorbs social bias)"; committing the index as staged publishes it under `data/`. Also staged: deletion of `legacy/2024/pythonw.exe` and `machine_text/side_by_side.md`. Unstaged: about 120 job files with paths made repository-relative, and `legacy/2024/testweet.py` / `twittertestwrite.py`, whose committed versions contain a hard-coded Twitter `bearer_token` assignment. Removing it from the working tree does not remove it from history.
- `make_numbers.py` docstring: "A result file that is missing leaves its macros as red '??' rather than stopping the build." Not so: `\providecommand{\num}` is the only fallback and `\num` is never used; a missing result file leaves its macros undefined (a LaTeX error, or a silent blank in nonstop mode).

Fix: rerun `make_numbers.py` and rebuild; move the group-term computation into `exp_refine.py`; key caches on a hash of `lexicon.tsv` and `core.py`; decide deliberately what the staged rename should publish.

### 6. Fast path and reference disagree on one input class, and the tests do not cover what the paper says they cover (minor)

- `tokens()` pads a leading space only. `.replace("' ", " ")` therefore strips a closing quote everywhere except at the very end of the string. `sketch("he called it 'wonderful'")` gives 0 hedons, `sketch_exact` gives 0, and the same text as row 0 of a batch gives 0.702 hedons (as the last row of a batch, 0 again). So a text's signature depends on its position in a batch, and the batch path disagrees with the reference that "defines the result". Fix: `text = " " + _clean(text) + " "`.
- `test_fast_path_equals_reference` draws every word from the lexicon and ends every text with `.`, `!` or `?`. It never produces an unknown word (bar one fixed string), a quoted word, an emoticon, an emoji, a digit, a non-ASCII character or a text that ends without punctuation, and never takes the `len(texts) > batch` branch (I checked that branch by hand: equal).
- Methods: "checks the propositions below for exact equality". Only Proposition 1 is tested (associativity, one unmerge, prefix spans), in object-dtype integers. README: "exactness of merge, prefix queries and readouts": the only readout test is `legendre` on the positive channel with `allclose`; `arc`, `level`, `stderr`, `features`, `valence`, all of `tone.py` and all of `refine.py` have no test.
- `arc` is the least-squares cubic only to order L⁻² (Methods says so; the Results sentence "The arc is the least-squares cubic fit" does not). Measured gap against a direct fit: 14% of the fit's rms at 10 words, 5% at 20, 0.3% at 50. The assertion in `exp_books.one_book` checks books of 20,000 words or more.

### 7. Macros that do not measure what the sentence says (minor)

- Abstract: "Against human ratings in six corpora the calibrated score reaches a mean correlation of 0.51, level with the best lexicon method (0.49) and below fine-tuned transformers (0.59 and 0.68)". All four macros are `mean_r_primary`, the mean over five corpora without Yelp. The six-corpus means are 0.52, 0.50, 0.62, 0.69.
- "Across 154,457 phrase pairs the later part gets the larger weight: 0.68 against 0.61 when both parts have at least two words". `treeNodes` counts all binary nodes; the quoted weights come from 29,636 nodes. (All nodes: difference +0.051.)
- "each of its 129 chapters" of *Moby-Dick*. The novel has 135 chapters and an epilogue; `chapters(min_words=300)` drops the short ones and their words, so `mobyWords`, `mobyHedons`, `mobyDolors` are not the whole book either.
- "16,350 entries, of which 8,408 have |c| ≥ 0.25 and carry charge". 38 of those are operator words whose charge `load_lexicon` sets to zero (*not*, *never*, *very*, *super* at 0.719 ...) and 224 are entries the tokeniser can never emit (hashtags such as `#fail`, hyphenated and multi-word VADER entries). About 8,150 entries can carry charge. 430 entries in all are unreachable.
- "the units were ... chapters of 150 of the novels": `extra_units` takes chapters of the first 150 of the 640 downloaded *files*, without the ten-chapter and 20,000-word filter that defines the 356 novels.
- "11 of 60 essays in the dial arm were left unfinished for that reason" (refusal): `dial()` counts missing files (`x is None`). Nothing the script reads distinguishes a refusal from a job that failed for another reason. Round 1 already lacks 10 dial files and 1 control file.
- `rvHuman` (1.16) is computed and never printed. The matched student essays sit farther from "the centre of student essays" than the machine drafts do (0.66) and far farther than the report-arm revisions (0.41), so a lower distance is not "closer to student writing"; the text should give the human value next to the others.
- "an interval that reaches zero" for `critBerRenWithin` = -1.2 [-2.5, +0.0]: the upper bound is +0.007. True, by 0.007 per thousand words.
- Methods quotes "from 0.521 to 0.534" for the restricted lexicons as literals, against "Every number in the text is generated from the result files".

### 8. Bootstrap grouping (minor, not quantified)

- `exp_position.where`: "Film review paragraphs" and "Novel chapters" are resampled as independent items (`np.arange(len(v))`), though paragraphs nest in reviews and chapters in novels. Their intervals in Extended Data Table 2 ("every interval excluding zero", novel chapters +0.0054 to +0.0087) are too narrow by an unknown factor.
- `exp_uncertainty.constants_ci` resamples EmoBank sentences, not documents, and holds `a`, `kappa` fixed, unlike the accuracy intervals.
- `exp_position.dilute` uses 100 resamples for Yelp; a 95% percentile interval from 100 draws is rough.
- `tree()`: "top node of each sentence" is the last matched binary node per sentence; if the root is unmatched (0.4% of nodes are) a lower node is taken instead.
- Dial control arm: achieved `after_contrast` correlates 0.43 with randomly assigned targets (draft: 0.12; the other nine control correlations are within ±0.12). With 48 essays that is p of about 0.002 for one of ten measures. Pearson on a skewed percentage is the likely cause; a rank correlation should be reported beside it.

### 9. Smaller code notes

- `refine.refine`: `Ridge(...)` uses the default `fit_intercept=True`, so an extra intercept is fitted and discarded; harmless because the readout is refitted, but not the problem Methods writes down. `n_new` counts neutral-band lexicon words as "new".
- `tokens`: every `.` ends a sentence, so "U.S." and "3.5" clear operators mid-sentence; non-ASCII punctuation outside the small table stays glued to the word (`«good»` is one unknown token); an emoji becomes several words (name words), which adds buffer words and shifts positions.
- `exp_books.novels` and `exp_speed.main` hard-code or take all 12 processes; `exp_books.critics` reads review files without the existence check that `corpora.scale` has.
- `exp_books.chapters`: the heading pattern accepts any line that starts with "chapter" plus a word, so contents lines and some prose lines split chapters; the 300-word floor removes most of the debris but also real short chapters.
- `tone.compare` rounds z before squaring; `reference()`'s docstring lists p5/p25/p75/p95 while `compare` needs p10/p90 (both are in the file).

## What I could not check

- Anything that needs an experiment script, model inference, LaTeX or a figure build. In particular: the refined-charge and pooled bag-of-words predictions (`refined_*`, `bowpool_*` caches), the transformer scores, the VADER / TextBlob / hedonometer caches, the throughput numbers, the novels run (I checked only `merge_err_max` and the summary fields against the text), and the bootstrap intervals themselves.
- Whether `core.py` or the lexicon changed between 20:27 (first refined-charge fold) and 21:05 (signature caches). The caches carry no version stamp and the first commit of `core.py` is at 21:28.
- Signature caches for SST, film reviews, Yelp and the `sigall_*` files (only SemEval, EmoBank and Facebook dev/test were recomputed).
- Whether the PDF was built from the `numbers.tex` beside it (I compared `numbers.tex` with the results, not the PDF text with `numbers.tex`).
- The size of the clustering understatement in concern 8, and whether the missing dial files are refusals.
- Figures: I did not open the PNGs; `make_figures.py` was read only around the *Moby-Dick* panel and the uncertainty panel.
- `integrations/`, `make_side_by_side.py`, `fetch_data.py` beyond the HC3 and EmoBank lines.
