# Referee report: statistical methodology and validity

Manuscript: "A mergeable measure of sentiment for text of any length" (`paper/sgw.pdf`, compiled 22:16 on 13 November 2026).
Repository state reviewed: commit `0fc5a78` plus the uncommitted edits present at about 22:30 (`README.md`, `paper/body_b.tex`, `paper/numbers.tex`, `scripts/make_numbers.py`, `machine_text/side_by_side.md`).

## What I examined

- `ANALYSIS_PLAN.md` with its deviations and four addenda, and its history in git (the plan is append-only; the only line ever removed is "None yet.").
- All of `paper/*.tex`, the generated `numbers.tex` and tables, and Figs 2, 3, 4, 7 and Extended Data Fig. 1 as PNG. The PDF text matches the sources and has no unresolved macros.
- `scripts/common.py`, `corpora.py`, `exp_accuracy.py`, `exp_position.py`, `exp_machine.py`, `exp_refine.py`, `exp_uncertainty.py`, `exp_books.py`, `exp_speed.py`, `dev_explore.py`, `make_numbers.py`; `sgw/core.py`, `sgw/tone.py`, `sgw/refine.py`.
- `results/accuracy.json`, `position.json`, `machine.json`, `constants_ci.json`, `uncertainty.json`, `parts.json`, `speed*.json`; `machine_text/` samples, keys, judge results and file times; logs under `data/`.
- Small recomputations, each a few seconds on one core, from the cached test scores, the treebank parquet files, the IMDB test file and the essay files. No experiment script, model inference or bootstrap at full size was run. My intervals use 300 to 1,000 resamples.

Numbers that I recomputed and that agree with the manuscript: R1, R2 and R3 point estimates; the ten essay contrasts in both conditions; the tone-check distances; the dial correlations (0.643 against 0.084) and gaps; the judge shares (30/48 and 5/48); the leave-one-prompt-out AUCs (0.741 and 0.643); the derived constants of the shipped model. Text shared between corpora is negligible (8 of 1,917 SST test sentences share an eight-word run with a film review in the fitting pool; 19 of 2,503 test film reviews share one with an SST training sentence).

On the question the machine-writing section was built to answer: no outcome in the repository shows that revisions guided by the measures read more like student writing. The dial is followed on the instrument's own measures (HD1). The classifier score moves the wrong way and is undecided (HD2). The judge preferred the dial revision as "more like a student's own work" in 5 of 48 pairs. The manuscript says this; the concerns below are about other claims.

## Concerns

### 1. Critical. The two treebank tests of recency are confounded with the length of the parts, and both reverse when length is held fixed

Claims affected: abstract ("what comes last counts most, a recency effect that three confirmatory tests support"; "Neutral words dilute a rating, but only when they follow it"); Introduction; "A revised hypothesis: recency" ("All three held"); the dilution section ("the comparison between the two orders, and the trend with m, are not [affected by regression to the mean]"); Discussion ("weights that rise toward the end would describe all of these results"); Fig. 3b, Fig. 4a; README.

The registered contrasts were computed as written, and I reproduce them. They do not test order. English binarised trees branch to the right, so the later part is usually the longer part, and the regression weight of a part rises with its share of the phrase's words.

R1, development and test trees, both parts of two or more tokens (11,474 nodes; `tree_nodes` from `scripts/exp_position.py`):

| Nodes | n | mean words, earlier / later | weight earlier | weight later | later minus earlier |
|---|---|---|---|---|---|
| all (registered test) | 11,474 | 4.7 / 7.7 | 0.627 | 0.686 | +0.059 |
| later part longer | 7,239 | 3.6 / 9.9 | 0.547 | 0.726 | +0.179 [+0.143, +0.217] |
| equal length | 1,474 | 3.3 / 3.3 | 0.744 | 0.579 | -0.164 [-0.257, -0.060] |
| earlier part longer | 2,761 | 8.4 / 4.4 | 0.714 | 0.598 | -0.116 [-0.167, -0.063] |

- Weight by decile of a part's own share of the words rises from 0.44 to 0.81. With ten word-share bins and one order term, later minus earlier at equal share is -0.030 [-0.060, -0.003] in the confirmatory trees and -0.016 [-0.035, -0.001] in the training trees.
- Mirrored cells (same own length, same length of the other part, order swapped; 11 cell pairs): -0.087 for the shorter part, -0.102 for the longer.
- In the equal-length stratum the two parts have the same spread of ratings (s.d. 0.135 and 0.136), so unequal reliability does not explain the reversal.
- The training trees show the same pattern: +0.208, -0.128, -0.138 in the three strata.

R2, same trees, neutral part of two or more tokens:

| Words in the rated part | kept, neutral words after (n) | kept, neutral words before (n) | before minus after |
|---|---|---|---|
| all (registered test) | 0.679 (1,690) | 0.770 (2,082) | +0.091 |
| 1 | 0.482 (681) | 0.393 (203) | -0.089 [-0.167, -0.002] |
| 2 | 0.713 (220) | 0.677 (152) | -0.036 [-0.137, +0.082] |
| 3 to 4 | 0.810 (352) | 0.724 (312) | -0.086 [-0.165, +0.002] |
| 5 to 8 | 0.848 (257) | 0.815 (486) | -0.033 [-0.101, +0.042] |
| 9 or more | 0.839 (180) | 0.850 (929) | +0.011 [-0.068, +0.092] |

- The rated part has a mean of 3.8 words (median 2) when the neutral words follow it and 9.1 (median 8) when they precede it. Matched on bins of rated-part length and neutral-part length, before minus after is -0.069 [-0.109, -0.032] in the confirmatory trees and -0.023 [-0.048, -0.002] in the training trees.
- Fig. 4a has the same composition shift. In the "after" series the mean length of the rated part falls from 8.5 words at m = 1 to 2.6 at m = 2 (58% single words); in the "before" series it stays between 7.4 and 11.8. Within one-word rated parts the share kept falls with m on both sides: 0.61, 0.52, 0.49, 0.44, 0.39 after; 0.53, 0.50, 0.38, 0.19 before. Leading neutral words dilute too.
- At m = 1 in the "after" series the neutral "word" is a full stop in 4,984 of 12,540 cases and a comma or "and" in 3,174 more.
- The claim that the comparison between orders is free of regression to the mean also needs support: among the R1 nodes 43% of earlier parts and 32% of later parts are within 0.05 of neutral, so a strong rating is more often noise on the earlier side.

R3 holds as registered, and by fifths of a review the AUCs are 0.658, 0.643, 0.647, 0.648, 0.691 (half sample, 10,975 reviews). The label is the writer's own rating, so R3 describes where writers put the verdict (both ends, the end most). It is not evidence about what a reader weighs.

The paper's own H1b result for the same sentences agrees with the stratified result: in SST the first third outweighs the last (+0.26 [+0.14, +0.39]).

Requested: re-estimate R1 and R2 with lengths held fixed and print the stratified tables; withdraw recency and one-sided dilution as findings about phrase ratings; state what the data show (a part's weight follows its share of the words, with no advantage for the later part); keep R3 as a result about writers.

Reproduction, from `scripts/`:

```python
from exp_position import tree_nodes
import numpy as np
d, _ = tree_nodes(["validation", "test"]); two = d[(d.nl >= 2) & (d.nr >= 2)]
w = lambda x: np.linalg.lstsq(np.column_stack([np.ones(len(x)), x.l, x.r]), x.p.values, rcond=None)[0][1:]
for name, s in (("later longer", two[two.nr > two.nl]), ("equal", two[two.nr == two.nl]), ("earlier longer", two[two.nr < two.nl])):
    print(name, len(s), w(s).round(3))
```

### 2. Major. Statements about front-loading and dilution do not follow the registered decision rules

- Introduction: "Front-loading fails in every corpus examined." Under the plan's rule H1b is supported in SST (+0.264 [+0.136, +0.393]), refuted in tweets and Yelp, undecided in three. The mean of the six contrasts is +0.002 (s.d. 0.31). Only H1a fails everywhere, and for Yelp its size is +0.0017 on a scale from -1 to 1, with hedons at -0.001.
- "Front-loading makes three predictions, and I tested each": the plan's third is H1c (undecided, +0.012 [-0.012, +0.032]). The text gives that place to the treebank regression, which deviation 7 lists as an unplanned exploratory addition, and does not label it there.
- H2 is registered as "fitted kappa > 0, and the fitted kappa gives a higher held-out r than kappa = 0". That contrast is in `results/accuracy.json` ("SGW, no dilution"): supported in tweets (+0.029 [+0.019, +0.040]), Facebook (+0.024 [+0.015, +0.035]), SST (+0.022 [+0.013, +0.031]) and Yelp (+0.011 [+0.002, +0.020]); undecided in EmoBank (+0.007 [-0.003, +0.020]) and film reviews (-0.002 [-0.009, +0.004]). The dilution section reports "5 of six" from a partial rank correlation that the plan's H2 does not name, and a peak in kappa read from the test portion of each corpus (`dilute()`, gains 0.009 to 0.020, no intervals). EmoBank has the strongest partial correlation (-0.27) and an undecided registered contrast.
- The partial correlation is observational. The number of buffer words is close to text length, and in Yelp and the film reviews the rating is the writer's own. The Yelp interval rests on 100 resamples.

Requested: report every hypothesis by its registered rule (H1b 1/2/3; H1c undecided; H2 4 supported, 2 undecided), reword the Introduction and abstract, and mark the partial correlations, the kappa curve and both treebank analyses as unplanned where they appear.

### 3. Major. The accuracy criterion: the registered secondary outcome is missing and removes the margin; run 1 is not archived

- Mean Pearson r over the five primary corpora: 0.506 against 0.490 for VADER. Resampling test items, the mean difference is +0.016 [+0.001, +0.033]. Across corpora it is positive in three and negative in two; a t interval over the five corpora is [-0.017, +0.049].
- The plan names Spearman rho as secondary. It is computed and stored but appears nowhere in the paper. Mean rho is 0.506 for the signature and 0.508 for VADER (difference -0.002 [-0.018, +0.015]). In film reviews the difference is +0.045 on Pearson and -0.030 on Spearman: VADER's compound score saturates on long texts, which costs it on Pearson and not on rank. The statement that the difference "excludes zero in favour of the signature in 3 of six corpora" depends on the metric.
- The level readout (0.479) is below VADER. The sentence "This meets a criterion ..." follows the sentence about the level and reads as if it referred to it.
- Deviations 1 to 3 (grid, tokeniser, bug fix) were made after the test portions had been scored. The first commit with any code or result (`4b10b12`) already contains them, and the logs hold one run. The run-1 numbers cannot be compared with the reported ones, and the margin is 0.016.
- Abstract: "in six corpora ... a mean correlation of 0.51" is the five-corpus mean (six: 0.52).

Requested: add Spearman to Extended Data Table 1 and the text; give the mean difference with an interval; restore and archive run-1 scores and report them beside the final ones.

### 4. Major. Intervals from 7 or 8 clusters, and intervals that hold six corpora fixed

Essays (8 assignments, percentile bootstrap over assignments). A t interval on the eight assignment means, and an exact sign-flip test:

| Measure | plain: paper | plain: t interval, p | as student: paper | as student: t interval, p |
|---|---|---|---|---|
| net density (HL1) | [-0.40, -0.02] "refuted" | [-0.48, +0.03], 0.078 | [+0.08, +0.52] "supported" | [-0.01, +0.60], 0.086 |
| ending lift (HL4) | [+0.24, +0.93] | [+0.13, +1.07], 0.031 | [+0.22, +0.75] | [+0.16, +0.88], 0.023 |
| intensified | [-0.83, -0.35] | [-0.92, -0.25], 0.008 | [-0.80, -0.23] | [-0.87, -0.09], 0.008 |
| negated | [-0.66, -0.16] | [-0.74, -0.06], 0.016 | [-0.52, +0.07] | [-0.63, +0.15], 0.195 |

HL4, the intensifier effect and HL5 (per-assignment AUC above 0.5 in 8 of 8, both conditions) survive. Both HL1 verdicts do not. Methods promises results that resample essays "where they differ"; HL3 for the student condition differs (refuted when essays are resampled) and is not given.

Tone check and dial. In `jobs()` and `dial_sample()` the draw `g.sample(..., random_state=...)` is made once per model with one seed, so the same positions are drawn in every model and each sample is made largely of blocks of four consecutive essays. Each experiment covers seven assignments, not eight (none from "Community service" in the tone check, none from "Summer projects" in the dial), with 13, 12, 8, 8, 7, 6, 3 and 15, 13, 8, 4, 3, 3, 2 essays after attrition. The printed judge intervals, 62% [43, 74] and 10% [5, 15], come from that resampling; the exact binomial intervals are [47, 76] and [3.5, 22.7].

Constants. `constants_ci()` resamples texts inside six fixed corpora with a and kappa fixed, and by item for EmoBank where the plan says by document. The six leave-one-corpus-out fits in `accuracy.json` give a dolor-to-hedon ratio from 1.17 to 1.81 against the printed [1.54, 1.73], a negated-hedon multiplier from -1.10 to -1.95 against [-1.85, -1.37], and a position contrast from -0.03 (tweets left out) to -0.19 against [-0.20, -0.07]. (The fit with EmoBank left out chose a = 2 and kappa = 0.2; the other five chose 4 and 0.5.) Fits to single corpora give a ratio of 0.81 in SST, 1.28 to 2.22 in tweets, Facebook posts and EmoBank, 4.48 in Yelp, and -0.25 in the film reviews. "The last third outweighs the first (-0.13 [-0.20, -0.07])" is a statement about this mix of corpora.

Requested: t intervals or a wild cluster bootstrap for assignment-level inference, with HL1 called undecided; exact intervals for judge shares; leave-one-corpus-out ranges beside the constants; a note that each revision experiment covers seven assignments.

### 5. Major. "Distance from the centre of student essays" does not measure resemblance to student writing, and the student value is left out

HT1 and HS1 use the mean squared standardised distance to the mean of student essays. Student essays themselves score 1.158 on it in the tone-check sample and 1.283 in the dial sample (`machine.json`; macros `\rvHuman` and `\dlCentreHuman` exist and are unused; Fig. 7c shows three bars and no student value). Over 15,594 student essays the mean is 1.00 and the median 0.77. Drafts average 0.66, plain revisions 0.53, report revisions 0.41: the 42nd, 31st and 21st percentiles of students. 72% of drafts and 91% of report revisions are nearer the centre than their matched student essay. A fall in this number is a move toward a typical profile that few students have. The text calls the report's change an "advantage over the control". The dial moved the same statistic from 0.63 to 0.80 against 0.57 for the control, toward the student value, and that is not reported.

Requested: show the student value; replace or supplement HT1 and HS1 with a distributional outcome (the out-of-sample classifier score and the AUC against matched students are already computed: 0.72 drafts, 0.53 plain, 0.59 dial); say that the registered distance rewards typicality.

### 6. Major. The pooled HC3 verdict hides reversals by source

"In HC3 all four predictions hold." `machine.json` holds the same tests by source, and the paper does not report them:

| Source | n | HL1 | HL2 | HL3 | HL4 |
|---|---|---|---|---|---|
| reddit_eli5 | 14,009 | supported | supported | refuted | supported |
| finance | 3,664 | supported | undecided | supported | supported |
| medicine | 1,155 | refuted (net -0.74) | refuted (dolors +0.28) | supported | refuted (lift -0.55) |
| wiki_csai | 824 | supported | supported | supported | supported |
| open_qa | 439 | undecided | undecided | undecided | undecided |

The pooled HL3 effect (-0.04) has the opposite sign to the largest source. "Two differences recur in all three comparisons": the negated share is +0.12 in open_qa and +0.03 in wiki_csai, the intensified share +0.10 and +0.11. Median lengths are 102 and 182 words and word count alone separates the classes better (0.72 against 0.63), so the standardised differences also mix in length-dependent noise in the human answers.

Requested: a by-source table and wording that the predictions hold for the pooled set and not within each source.

### 7. Major. The dial: what the registered outcomes can and cannot show

- HD1 reproduces and is stable (mean Spearman 0.585 against 0.086; 0.64, 0.65 and 0.60 for the three models with 14 or 15 essays). It covers three models: the fourth declined 11 of 15 dial requests and contributes 4 essays. Attrition differs by arm (11 against 1). "A model can be steered" should carry that scope.
- HD3 as written ("not wholly below 0.5") is passed by any share whose interval reaches 0.5; at n = 48 a share of 40% passes. It is a non-inferiority test with no margin and its "supported" label carries no information.
- The two judge answers are near complements: the dial revision was never chosen for both questions (0 of 48), and the answers point to different essays in 35 pairs. "Better written" measures polish, which is the opposite of the stated aim of the revision.
- The pooled 62% mixes two judges and four writers: 14/15 for Opus drafts (judged by Sonnet), 12/15 for Fable, 3/14 for Sonnet, 1/4 for Haiku (judged by Opus).
- The second judge question is unplanned. The plan now records it (deviation 5 of the addenda); the paper text and abstract use it without saying so. The judge's instruction is not in the repository: `machine_text/judge_jobs/*.json` hold only the assignment, the writer description and the two essays.

### 8. Minor. Labelling and scope of "confirmatory" and "untouched"

- The treebank dilution analysis, the partial correlations, the error-against-exposure check and the constants are unplanned and are not marked where they are reported; Methods lists some of them.
- "Data that had played no part": the development and test sentences were scored in the accuracy benchmark and used in H1a, H1b and the dilution analyses (deviation 8, all portions). The phrase dictionary with every rating was on disk from 19:49. Of the R1 nodes, 24% of earlier parts and 8% of later parts also occur as phrases in training trees (84% of all nodes share a part). `confirm()` entered the repository in the same commit as its results.

### 9. Minor. What the pre-registration record can support

- All seven commits are local; `origin/main` is at a 2025 commit. The hashes cited in Methods carry only timestamps set on the author's machine.
- The deviation to addendum 1 says "before any essay was generated" and first appears in commit `77d4fa2` (21:50), after the essay files (21:30 to 21:35) and the first revisions (21:42 to 21:44).
- File times do agree with the stated order for addenda 2 to 4 (held-out trees and IMDB fetched at 21:53 after the 21:50 commit; skill and dial jobs written after their commits).

### 10. Minor. Other points

- Extended Data Fig. 1a,b: `exp_uncertainty.py` fits the slope, the intercept and the variance scale on the test portion being assessed, and the fitted variance has no exposure term. Agreement with the diagonal and coverage near 90% follow by construction. The caption should say so or the panels should go.
- "Chapters differ by more than chance": the null deals sentences at random, so any serial dependence gives excess between-chapter variance wherever the cuts fall. A null that moves the chapter boundaries and keeps the order would test chapters.
- The level's standard error treats words as independent draws. The paper's own clustering results (1.1 at the sentence level in essays, 5.9 between chapters) reject that, so the chapter intervals in Fig. 5a are likely too narrow.
- Resample counts are 100 to 1,000 in the position, dilution, treebank, HC3 and AUC analyses and 500 for Spearman rho; the plan says 2,000.
- Throughput: best of three runs for the signature, one run of 600 sentences for the transformers. The factor of 4,500 does not depend on it.

## What I could not check

- Run 1: no output or log of the first scoring of the test portions exists in the repository.
- What the writing, revising and judging agents received beyond the job text. This reviewing agent's own context held more than its task (a session start hook that sets a "lazy senior developer" persona, and an index of user memory notes), so "each agent received only its task" cannot be confirmed from the job files.
- Transformer and VADER scores, the lexicon build, the novels pipeline and the throughput figures: read, not re-run.
- Full-size bootstrap intervals in the result files; I compared point estimates and recomputed intervals with fewer resamples.
- Whether EmoBank's document identifiers are correct units (110 documents in the test portion).
- References and the mathematical propositions, which are outside this lens.
