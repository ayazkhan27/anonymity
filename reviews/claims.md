# Referee report: claims against evidence

Manuscript: "A mergeable measure of sentiment for text of any length" (`paper/sgw.pdf`, sources in `paper/*.tex`).
Lens: every quantitative or directional claim in the abstract, Results and Discussion, checked against `results/*.json` and `paper/numbers.tex`; figure captions checked against the PNGs.

## What I examined

- All of `abstract.tex`, `body_a.tex`, `body_b.tex`, `body_c.tex`, `textmacros.tex`, `methods.tex`, `figures.tex`, `extended.tex`, the three table files and `excerpt.tex`, with every macro resolved from `numbers.tex`.
- All fifteen files in `results/`. I recomputed by hand or with one-line arithmetic: the five-corpus means, the ablation losses, the coefficient ratios, the speed ratios and billion-sentence times, Moby-Dick density, the pair count, the sign-test p-value. These all match the text.
- `ANALYSIS_PLAN.md` with its deviations and four addenda, against the decisions stored in the JSON files.
- All eight PNG figures, opened and compared with their captions.
- `scripts/exp_position.py`, `exp_speed.py`, `exp_uncertainty.py` (read, not run) to see what each number measures.
- Git history and file modification times, to check the ordering claims in the pre-specification paragraph.
- Two checks of my own on the held-out treebank trees (`data/sst/ptb_validation.parquet`, `ptb_test.parquet`), each under one second on one core, point estimates only, no bootstrap. They reproduce the paper's R1 and R2 numbers exactly (n = 11,474, weights 0.627 and 0.686; n = 1,690 and 2,082, shares 0.679 and 0.770) and then split them by part length. Results are in concern 1.

State of the files after the crash, as far as this lens sees it: every `results/*.json` parses, `numbers.tex` is complete, the PDF log reports 14 pages with no undefined references. Two things are out of step and are listed under concern 17: an uncommitted, unrun addition to `scripts/exp_position.py`, and a PDF and a Figure 7 that are older than the text and data they belong to.

## Concerns

### 1. Critical. Two of the three confirmatory tests of recency are confounded by the length of the parts, and the effect disappears or reverses when length is held fixed

Claims. Abstract: "what comes last counts most, a recency effect that three confirmatory tests support. Neutral words dilute a rating, but only when they follow it." Results: "All three held." Discussion: "The end of a phrase, a sentence or a review counts for more than its start."

Evidence in the paper. R1: later weight 0.69 against 0.63, difference +0.059 [+0.032, +0.085]. R2: share kept 0.68 after against 0.77 before, difference +0.091 [+0.055, +0.127] (`position.json`, `confirm`).

What I found on the same held-out nodes.

R1 (both parts two or more words, n = 11,474). The later part is the longer one in 63% of pairs; mean length 7.7 words against 4.7.

| Stratum | n | Earlier weight | Later weight | Later minus earlier |
|---|---|---|---|---|
| Later part longer | 7,239 | 0.547 | 0.726 | +0.179 |
| Equal length | 1,474 | 0.744 | 0.579 | -0.164 |
| Earlier part longer | 2,761 | 0.714 | 0.598 | -0.116 |

The longer part gets the larger weight. At equal length the earlier part does.

R2 (neutral part of two or more words). The rated part averages 3.8 words when the neutral words follow it and 9.1 words when they precede it. Share of the rating kept, by the rated part's share of the joined phrase:

| Rated part's share of the phrase | n after | n before | Kept, neutral after | Kept, neutral before |
|---|---|---|---|---|
| up to 0.25 | 461 | 88 | 0.48 | 0.43 |
| 0.25 to 0.4 | 556 | 263 | 0.62 | 0.55 |
| 0.4 to 0.5 | 227 | 207 | 0.87 | 0.73 |
| 0.5 to 0.6 | 167 | 228 | 0.81 | 0.78 |
| 0.6 to 0.75 | 194 | 496 | 0.87 | 0.83 |
| above 0.75 | 85 | 800 | 0.85 | 0.85 |

In five of six bins the rated phrase keeps more when the neutral words follow it; in the sixth the two are equal (0.845 against 0.846). The pooled difference of +0.091 is produced by the mix of lengths, the reversal the Introduction itself describes under Simpson's paradox. Binning by the rated part's length alone gives the same picture (trailing keeps more in four of five bins: 0.48 against 0.39, 0.71 against 0.68, 0.81 against 0.72, 0.85 against 0.82, 0.84 against 0.85).

What the data do support: a rating is diluted in proportion to how much of the phrase is neutral, regardless of side. That is the original dilution hypothesis, not recency.

The exploratory curve has a second problem. The text gives the endpoints, "0.78 for m=1 and 0.62 for m≥9", which reads as a gradient. The stored curve for neutral words after is 0.776, 0.624, 0.663, 0.674, 0.619 for m = 1, 2, 3-4, 5-8, 9+. It is one step between m = 1 and m = 2 and flat after that. At m = 1, which is 79% of the training pairs (36,107 of 45,895), the trailing case keeps more than the leading one (0.776 [0.768, 0.784] against 0.748 [0.741, 0.754]), the opposite of the claim. R2 was registered on m ≥ 2 after that curve had been seen. Tokens include punctuation: in the held-out trees 40% of the single trailing "neutral words" are a full stop and 13% a comma.

R3 (last fifth of an IMDB review separates the label better than the first fifth, AUC 0.696 against 0.657) is sound as computed, but the label is the writer's own verdict. It shows where writers put the verdict, not what readers weigh, and it compares last with first only, so "counts most" is not tested.

The working tree holds an uncommitted function `parts()` in `scripts/exp_position.py` whose docstring says it was "added after a referee showed that the later part of a treebank phrase is usually the longer one, which confounds the registered contrasts". It has not been run: `position.json` has no such key, and the paper and README still state the unadjusted result.

Required. Run the length-controlled analysis with intervals and report it. On the point estimates above, withdraw "what comes last counts most", "but only when they follow it", "one-sided", and "three confirmatory tests support" from the abstract, the section titles, the Discussion and the README; say that R1 and R2 met their registered rule and that the rule did not control for length.

### 2. Major. "Front-loading fails in every corpus examined" contradicts the paper's own kernel result

Claim (Introduction): "Front-loading fails in every corpus examined." Discussion: "The end of ... a sentence ... counts for more than its start."

Evidence. Under the registered rule for H1b, front-loading is supported in SST sentences: first third minus last third +0.26 [+0.14, +0.39] (`kernSst`; `make_numbers.py` itself counts `kernSupport = 1`). Film reviews are undecided with a positive point estimate, +0.46 [-0.10, +1.04]. EmoBank and Facebook are undecided. Only tweets and Yelp go against. So H1b is supported in one corpus, refuted in two, undecided in three.

The SST kernel (earlier words weigh more) and the SST tree regression (later part weighs more) use the same sentences and point opposite ways. The paper reports both and does not reconcile them. Concern 1 offers the reconciliation: the tree result is length, not order.

For H1a the Yelp centroid is +0.002 [+0.001, +0.002] on a scale from -1 to +1 with 157,452 texts, hedons alone sit earlier (-0.001 [-0.002, -0.001]), and the first tenth of a Yelp review carries 1.09 times the mean charge. "Every interval excluding zero" is true and uninformative there.

Required. State the H1b verdict per corpus as the plan defines it. Replace "fails in every corpus" with what holds: charged words do not sit early on average; reader weighting by position is mixed.

### 3. Major. The title claims text of any length; the evidence is weakest exactly where texts are long, and one unfavourable number is computed but not reported

Claims. Title and abstract: a measure "for text of any length", "level with the best lexicon method". Results: "Averaging is the problem, whatever is averaged."

Evidence.
- Whole film reviews are the only primary corpus of long texts. Calibrated signature r = 0.29 [0.26, 0.33]. TextBlob reaches 0.46, and the paired difference is -0.168 [-0.200, -0.136] (`dBlobFilm`). "Level with the best lexicon method" is a statement about the five-corpus mean; on long documents a plain lexicon method beats it by a wide margin. With constants from the same corpus the signature reaches 0.40, so the leave-one-out constants do not transfer to this length.
- `results/parts.json` holds `"SGW density r with rating": 0.232` for the same 300 reviews on which the mean of DistilBERT sentence scores reaches 0.61 and RoBERTa 0.52. The paper reports the transformer numbers from this file and omits the signature's.
- Fairy tales: merged calibrated signature 0.50 [0.34, 0.63]; VADER averaged over sentences 0.63 [0.51, 0.73] (reported, to the paper's credit).
- In every place where validity above the sentence was measured, averaging sentence scores gave the best agreement with the human target, and it improved the transformers (0.61 against 0.53; 0.52 against 0.23). The section presents averaging as the defect. What the data show is that averaged bounded scores are not invariant to the cut, and are more valid than the merged score.
- "Two transformer classifiers behave the same way": 97% of the 300 reviews are truncated at 512 tokens, so "the score of the review" is the score of its opening. The comparison is truncated text against full text, not one cut against another. No transformer was scored under two different chunkings.
- The like-for-like comparison is in Fig. 6c and deserves a sentence: at five-sentence chunks averaged density swaps more pairs of books (7.6%) than averaged VADER (4.6%); the 19% for VADER is at chapter size, for which the averaged-density series has no point.

Required. Report the 0.23. Say in the abstract and Discussion that agreement on long documents is lower than a lexicon baseline and that averaged sentence scores track human targets better. Reword "Averaging is the problem" to the invariance claim that the data support.

### 4. Major. "In HC3 all four predictions hold" is true only of the pooled sample; stored per-source results reverse it

Claim: "In HC3 all four predictions hold, and all four effects are small." Also: "Two differences recur in all three comparisons."

Evidence (`machine.json`, `hc3.by_source`). reddit_eli5 is 14,009 of 20,091 pairs.

| Source | n | HL1 | HL2 | HL3 | HL4 |
|---|---|---|---|---|---|
| reddit_eli5 | 14,009 | supported | supported | refuted | supported |
| finance | 3,664 | supported | undecided | supported | supported |
| medicine | 1,155 | refuted | refuted | supported | refuted |
| wiki_csai | 824 | supported | supported | supported | supported |
| open_qa | 439 | undecided | undecided | undecided | undecided |

All four hold in one source of five. In medicine the net-density effect is -0.74 SD and ending lift -0.55 SD, far from "small" and in the wrong direction. HL3 is refuted in the source that supplies 70% of the data. The intensifier and negation differences change sign in open_qa (+0.10, +0.12) and wiki_csai (+0.11, +0.03), so they do not "recur" within HC3 either.

For the essays, negation under the student instruction is -0.24 [-0.52, +0.07]. "An interval that reaches zero" understates an interval that extends past zero, and the abstract's "sparse use of intensifiers and negation" rests for negation on one of two essay conditions. Under that condition net density (+0.29 [+0.08, +0.52]) and hedon density (+0.34 [+0.09, +0.62]) are larger and better determined than negation, so "mainly" is not what the numbers say. By model, the negation difference is +0.11 for Fable under the student instruction.

Methods promises that "results that resample essays are given where they differ". They differ and are not given for HL3 under the student instruction (undecided by assignment, refuted by essay: clustering +0.25, essay interval [+0.04, +0.45]) and for negation (essay interval [-0.40, -0.07]).

Required. Report the per-source table. Replace "all four predictions hold" with the pooled statement and its dependence on one source. Drop "recur in all three comparisons" for negation. Give the essay-resampled results where the verdict changes.

### 5. Major. The revision and dial claims rest on a distance that students themselves score worst on, on an undecided test, and on an unplanned question to a model judge

Claims. Abstract: "A model can be steered to an assigned profile of these measures, and doing so did not make its essays read more like the students'." Text: "Their advantage over the control is -0.12"; "The dial was followed"; "A plain instruction ... did as much for the classifier and far more for the judge."

Evidence.
- Distance from the centre of student essays: drafts 0.66, plain revision 0.53, report 0.41. Student essays themselves score 1.16 (`revise.distance.human`; 1.28 in the dial sample). The macro `\rvHuman` exists and is never used. Machine drafts start closer to the centre than students are, and every revision moves them further below the student value. A lower distance is not "toward the human range", and "advantage" is the wrong word. The dial arm, which the text does not report on this measure, moves the other way (0.80 against 0.57 for the control, `dlCentre*`), toward the student value.
- Fig. 7c shows three bars with no intervals, no student reference and no skill arms, though the text cites it for the skills. The figure file is dated 21:56; the skill rewrites finished at 22:01 and the dial at 22:10. The contrast the bars suggest (report against control) is undecided on the planned test, -0.12 [-0.25, +0.01]. So is control against draft, -0.13 [-0.30, +0.03], which the text passes over.
- "Did not make its essays read more like the students'": HD2 is undecided, +0.06 [-0.04, +0.17], n = 48. A null is asserted from an interval that includes zero. Against the drafts the dial revisions did become more student-like by the paper's own classifier (machine score 0.64 to 0.49; AUC against matched students 0.72 to 0.59). The sentence needs "than a plain revision".
- "Read more like" rests on a judge question that the addendum did not list (plan, addenda deviation 5), answered by a model. The paper does not mark it as unplanned. Its interval, 10% [5, 15], comes from resampling eight assignments and is narrower than the essay-level interval the code also stores, [2, 21], and than a binomial interval for 5 of 48. Methods promises the essay-level result where it differs.
- "The dial was followed": the mean squared gap to target falls from 1.67 to 0.79, so about half remains; correlations run from 0.24 to 0.83; in the excerpt chosen by rule, two of five measures moved away from the target; and the smallest model declined 11 of its 15 dial essays, so the result describes three models, not four.
- Accounting: 60 requested, 48 analysed, 11 declined in the dial arm. The twelfth loss is a control-arm text (`missing_control: 1`) that the paper does not mention.

Required. Report the student value beside every distance, redraw Fig. 7c with it, the skill arms and intervals, or replace the distance with one that has students at the target. State HD2 as undecided in the abstract. Mark the second judge question as unplanned and give its essay-level interval.

### 6. Major. The abstract's chunking result is attributed to 356 novels; it was run on 60

Claim (abstract): "Across \nNovels{} [356] novels a conventional score moves from 0.11 to 0.76 with the size of the chunks."

Evidence. `books.json`, `summary.vader.n_books = 60`. The Results text says "For 60 of the novels" and the Fig. 6 caption says 60. The Introduction's "87% of whole chapters" beyond ±0.99 is from the same 60 and is introduced as "in the novels studied below". The README repeats 0.11 and 0.76 without a count.

"While merged signatures do not change" is an arithmetic identity (relative spread 1e-14), not a finding to set beside an empirical one. The move from 0.11 to 0.76 comes from VADER's normalisation of a sum that grows with length; it is not produced by averaging, and the paper should not let the reader infer that it is.

Required. Use `\vaderBooks` in the abstract and the Introduction.

### 7. Major. The intervals on the "plain readings" of the coefficients describe one pooled fit and understate how much the readings change between corpora

Claims: "A dolor moves the predicted rating 1.63 [1.54, 1.73] times as far as a hedon"; negated positive -1.60 [-1.85, -1.37]; "not good is bad, and not bad is neutral" (-0.01 [-0.23, +0.20]); intensifier +1.02 [+0.85, +1.19].

Evidence. The same ratios from the coefficient vectors already stored in `accuracy.json`:

| Fit | Dolor/hedon | Negated hedon | Negated dolor | Intensifier |
|---|---|---|---|---|
| Shipped (six corpora) | 1.63 | -1.60 | -0.01 | 1.01 |
| Leave-one-out pools, range over six | 1.17 to 1.81 | -1.95 to -1.10 | -0.14 to +0.10 | 0.87 to 1.09 |
| Same-corpus: EmoBank | 2.22 | -1.75 | +0.19 | 0.51 |
| Same-corpus: tweets | 1.28 | -0.51 | +0.34 | 0.93 |
| Same-corpus: Facebook | 1.64 | -0.78 | +0.07 | 1.05 |
| Same-corpus: SST | 0.81 | -0.69 | -0.78 | 0.66 |
| Same-corpus: film reviews | -0.25 | -1.23 | +1.32 | 7.89 |
| Same-corpus: Yelp | 4.48 | -4.26 | -0.39 | 1.10 |

Dropping one corpus of six moves the dolor/hedon ratio to 1.17, outside the stated interval. Within single corpora the ratio runs from below 1 to above 4, and "not bad" runs from fully reversed (SST, -0.78) to still bad (film, +1.32). The bootstrap resamples texts inside a fixed set of six corpora (`exp_uncertainty.py`, `constants_ci`), so it measures sampling error of the pooled fit and nothing about generality. The same-corpus fits also choose different a and κ (a = 32 for film and Yelp), which the paper does not mention when it says the six leave-one-out fits "chose a of 2 or 4".

Required. Present the readings as properties of this pooled fit, give the leave-one-out and same-corpus ranges, and drop the general statements about negation and negativity.

### 8. Major. "In none of the six corpora did squared error fall as exposure rose" is contradicted by the stored bands

Evidence (`uncertainty.json`, RMSE by exposure quartile, low to high). EmoBank 0.95, 0.93, 0.79, 0.80: it falls. Yelp 0.82, 0.77, 0.77, 0.78: it falls after the first quartile. Tweets 0.71, 0.70, 0.77, 0.75; Facebook 0.72, 0.77, 0.82, 0.92; SST 0.88, 0.91, 0.94, 0.95; film 0.91, 0.92, 0.95, 1.02: these rise.

The per-corpus statistic behind the claim is `corr_abs_err_sd`, which is 0.0 in all six because the fitted variance has `s1 = 0`, so the predicted standard deviation is a constant and its rank correlation with anything is undefined. `errRankPos = 0` counts that as "none positive". The `s1` itself is fitted on the pooled training data of the other five corpora, where exposure is confounded with corpus (film reviews have exposure 300 to 600 and the largest errors), so it cannot detect a within-corpus decline. Extended Data Fig. 1a does not label which point is which quartile, so the reader cannot see the direction.

The accurate statement is that error falls with exposure in EmoBank, rises in four corpora and is flat in Yelp. The conclusion drawn (the standard error is not a prediction interval) survives. The sentence and the caption ("observed error does not fall with exposure") do not.

### 9. Major. The registered dilution test gives four of six, and the text reports five of six from a different statistic

The plan's H2 rule is: fitted κ > 0 and the fitted κ gives a higher held-out r than κ = 0, interval excluding zero. `dNoDil*`: EmoBank +0.007 [-0.003, +0.020], film -0.002 [-0.009, +0.004], the other four exclude zero (`winsNoDil = 4`). The text leads with "falls ... in 5 of six corpora" from the partial rank correlation, which the plan's deviation 8 calls an estimate and not a held-out prediction, and with "peaks at a buffer weight κ between 0.2 and 1 in the same five corpora", a statement about curve maxima with gains of 0.009 to 0.020 and no intervals. EmoBank has the strongest partial correlation (-0.27) and an undecided registered test. Say so.

### 10. Minor to major. "This meets a criterion" follows the wrong number

"The level ..., which has no fitted coefficients, reaches 0.48. This meets a criterion that an earlier version of the method failed (Methods), by a small margin." The level's five-corpus mean is 0.479; VADER's is 0.490. The level fails the gate. The calibrated score (0.506) passes it by 0.016. Move the sentence. The level is also not free of fitting: its a and κ (1 and 0.2 in five of six fits) are chosen on rated development data, separately from the calibrated readout's.

The gate is a comparison of two point estimates. No interval is given for the mean difference; per corpus the difference from VADER excludes zero in two of the five primary corpora. The plan's secondary outcome, Spearman's rho, is computed in `accuracy.json` and reported nowhere. Run 1, "with the plan exactly as written", left no result file, so the statement that the widened grid and the rewritten tokeniser did not change the outcome of a gate passed by 0.016 cannot be checked.

### 11. Minor. Abstract wording on accuracy and speed

- "Against human ratings in six corpora the calibrated score reaches a mean correlation of 0.51": the mean is over five. Over six it is 0.52 against 0.50.
- "below fine-tuned transformers (0.59 and 0.68)": against DistilBERT the signature is significantly better on tweets (+0.103 [+0.062, +0.145]) and Facebook posts (+0.077 [+0.036, +0.115]) and tied on EmoBank. DistilBERT's mean lead comes from SST, the domain it was tuned on, and film reviews.
- "about 4,500 times their speed": 4,500 is DistilBERT; the RoBERTa figure is 8,700.
- The timed call is `sgw.level(sgw.sketch(x))`, the level readout (mean r 0.48), not the calibrated score (0.51) that the sentence pairs it with. The extra cost is small, but the number is not of the thing named.
- "Processor timings are the best of repeated runs": the transformers were timed once (`rate(run, sents[:n], 1)`) on 600 sentences.
- "On one core of a desktop processor" is a rented host. Twelve processes on six physical cores give 4.8 times one core; "twelve" in "21 minutes on twelve" should say processes or threads.
- `speed_gpu.json` records torch 2.3.1; Methods lists PyTorch 2.11.

### 12. Minor. Statements whose n or scope is not the one printed

- "Across 154,457 phrase pairs the later part gets the larger weight: 0.68 against 0.61 when both parts have at least two words". 154,457 is all matched training nodes; the subset's n is not given.
- "all 8 kinds of text": film-review paragraphs are cut from the film-review test portion and novel chapters come from 150 of the 356 novels; these are not eight independent bodies of text.
- "tested them on data that had played no part: the development and test trees". The phrase trees are new: `ptb_validation.parquet` and `ptb_test.parquet` are dated 21:53, after the addendum commit at 21:50, which I take as confirmation. The sentences are not: deviation 8 puts all portions of SST into the kernel, the position table and the partial correlation, and the test sentences are the SST accuracy test set. The top node of each tree is that sentence rating.
- "Two of the 320 requests produced no essay": both are in the plain condition (158 pairs against 160), which the text does not say.

### 13. Minor. Novels

- "each of its 129 chapters": the standard text of Moby-Dick has 135 chapters and an epilogue. The heading splitter lost several. Whether the "final chapter (4,844 words)" in Fig. 5b is chapter 135 alone I could not check.
- "One number for a book hides most of what its chapters do" follows from a variance ratio of 5.9 against a null that shuffles sentences. The null removes every kind of local continuity, so the ratio shows chapters are not random bags of sentences. It does not quantify "most". With 200 shuffles the smallest attainable p is 1/201, which is what the Moby-Dick entry stores.
- The two leading modes are "close to the first two Legendre polynomials". They carry 22% and 19% of the variance (`svd_var_top3`; the macro `\svdVarTwo` = 41 is unused), and smooth low-order modes are what a 10% moving window produces from any series. No shuffled-text comparison is given. The link to story shapes needs one, or the variance shares and a caveat.

### 14. Minor. The Simpson motivation has no instance in the data

The Introduction lists reversal under pooling as the third practical problem. The paper finds none: `critReversals = 0` among six critic pairs; pooled and chapter-mean density differ by 4% of the between-novel standard deviation and reorder 3.1% of pairs. The one example shown has a within-rating interval that touches zero. The text is candid about each number. The Introduction should say that the reversal is a possibility the signature guards against and that it was not observed.

### 15. Minor. Figures and captions

- Fig. 1c. The caption says "shipped a and κ for the level". The panel shows +275 ± 108, which is a = 1, κ = 0.2 (net 3.14 over 1 + 8 + 0.2 × 12). Methods and Extended Data Table 3 give the shipped constants as a = 4, κ = 0.5, which would give +174. The level's own constants are stated nowhere in the paper.
- Fig. 2b. The caption says "on one CPU core"; the panel also plots open points for the RTX 3090.
- Fig. 4a. "the share of the first phrase's rating": when the neutral words come first, the rated phrase is the second. The axis says "words"; tokens include punctuation.
- Fig. 4c. The curves are same-corpus fits with a fixed at 4 (code comment "in-corpus constants"); the caption does not say so, and the axis runs to κ = 4 while Methods gives a grid to 2.
- Fig. 6c. The averaged-density series stops at 125 sentences; the VADER series continues to "chapter". The text compares VADER's chapter value (19%) with density's maximum over the shorter range (8%).
- Fig. 7c. See concern 5.
- Extended Data Fig. 1a. Quartiles are not labelled; see concern 8.
- No figure shows the confirmatory tests R1 to R3 or the dial.

### 16. Minor. Smaller wording points

- "Dilution holds" (Introduction) and "leave it almost unchanged when they come before it": the share kept with neutral words before is 0.75 to 0.77. The paper attributes the missing quarter to regression to the mean and gives no estimate of it. The single-full-stop case (0.78) is a usable baseline and the text could say so.
- "without the neutral prior by 0.010 ... what earns its place": the neutral prior helps in two corpora, hurts in one (film, -0.005 [-0.008, -0.002]) and is undecided in three.
- "Methods trained on labelled text do better": the calibrated signature is also trained on labelled text. A bag-of-words ridge given the same five-corpus pool scores 0.55 against 0.51. This appears only later, as the comparator for refined charges.
- "so part of the pattern carries from ChatGPT answers of 2023 to Claude essays of 2026" (transfer AUC 0.67 and 0.65): no interval.
- "word count alone ... is near chance for the length-matched essays (0.54)": the interval [0.53, 0.56] excludes 0.5, and machine essays are 4% longer at the median (403.5 against 388 words).
- HC3 human answers have a median of 102 words and ChatGPT answers 182. Ending lift is a cubic fit over the last fifth; on a 30-word answer that is six words. "Length-free" needs a check that the measure's distribution does not depend on length.

### 17. Housekeeping found while checking

- `scripts/exp_position.py` has an uncommitted change that adds `parts()` (see concern 1). The file parses. It has not been run and nothing in `results/` or the paper depends on it yet.
- `paper/body_b.tex` is dated 22:17:09 and `paper/sgw.pdf` 22:16:14. The PDF may not contain the last edit to the Results. I reviewed the `.tex`.
- `fig7_machine.png` (21:56) predates the final `machine.json` (22:11).
- Commit order and file times agree with the pre-specification paragraph: job files for the essays, the skill rewrites and the dial are each dated after the commit that registers them (the skill jobs by one second). All of it, from the plan to the paper, is dated between 19:48 and 22:45 on one evening in a local repository.

## What I could not check

- I ran no experiment, model, LaTeX or figure script. Agreement between `numbers.tex` and the JSON files was checked by reading about 150 macros against their sources, not by regenerating the file.
- My two treebank checks are point estimates on the held-out trees without intervals. The pattern is consistent across bins and the samples are in the hundreds to thousands per cell, but the authors should confirm it with the bootstrap they already use. I did not repeat them on the training trees.
- Whether the essays were written by the four models named, at the settings named, with no knowledge of the hypotheses. The job files exist; the provenance of each output does not.
- Whether the judge was blind in practice, and the content of the refusals.
- The run-1 results, which are not archived.
- Timing figures. They depend on the rented host.
- References. I made no web requests and did not verify any citation against a primary record.
- The corpora themselves: split sizes, the SST and IMDB contents, the Moby-Dick chapter boundaries.
- The compiled PDF page by page. I read the sources and the PNG figures.
