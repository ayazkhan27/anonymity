# Analysis plan

Committed before any evaluation data were scored. Deviations are listed at the end of this file with the date and the reason.

## Question

Can sentiment be measured as a quantity, with a unit, that adds up from words to sentences to books, at a cost low enough to scan billions of sentences? Two ideas from the 2024 Sentiment Gummy Worm manuscript are tested along the way: that feeling words sit early in a text, and that words carrying no sentiment dilute the feeling.

## Method under test (fixed in advance)

- Word charge: human word ratings mapped to [-1, 1], zero at the neutral point. Positive part in hedons (hd), negative part in dolors (dl). Words with |charge| < 0.25 count as buffer words (the hedonometer's neutral band).
- Open lexicon: labMT 1.0 and the VADER lexicon, put on one scale by linear regression on shared words and averaged. NRC-VAD and the Warriner norms are sensitivity checks only (their licences bar redistribution).
- Signature of a span: token count, buffer count, charged count, and power sums of positive and negative charge over position, degrees 0 to 3. Separate degree-0 sums for charge under negation, after an intensifier, after a diminisher, and after a contrast word.
- Negation scope: three tokens, ended by punctuation. Context never crosses a sentence boundary.
- Level readout: net charge / (a + E + kappa * B), with E charged tokens and B buffer tokens.
- Calibrated readout: ridge regression of standardised human ratings on the signature features divided by the same exposure. Everything fitted is a coefficient of a linear least-squares problem, apart from the two scalars a and kappa, which are chosen on a grid (a in {0, 0.5, 1, 2, 4}; kappa in {0, 0.05, 0.1, 0.2, 0.5, 1}) by mean Pearson r on development data.

## Data

Human continuous ratings, test portions only for reported numbers:

| Corpus | Unit | Rating |
|---|---|---|
| EmoBank | sentence, six genres | valence 1 to 5 |
| SemEval-2018 Task 1 V-reg | tweet | valence 0 to 1 |
| Facebook valence/arousal posts | post | valence 1 to 9, two raters |
| Stanford Sentiment Treebank (if a mirror is reachable) | sentence | 0 to 1 |
| Pang and Lee scale data | whole film review | critic's rating, 0 to 1 |
| Yelp review full (secondary) | review | 1 to 5 stars |

Corpora with official splits keep them. Corpora without one are split 50/50 by a seeded shuffle (seed 0). Project Gutenberg novels and the Alm fairy-tale annotations are used for the hierarchy analyses, which have no fitted parameters.

## Outcomes

- Primary: Pearson r between score and human rating on each test set. Secondary: Spearman rho.
- Constants are fitted leave-one-corpus-out: for each target corpus, coefficients, a and kappa come from the training and development portions of the other corpora only.
- Intervals: 95% bootstrap, 2,000 resamples; resampling by source document for EmoBank, by item elsewhere. Differences between methods use a paired bootstrap.
- Comparators: VADER compound, hedonometer (labMT mean, neutral band removed), TextBlob polarity, in-domain TF-IDF ridge, DistilBERT fine-tuned on SST-2, Twitter-RoBERTa-base sentiment. Transformer score is P(positive) - P(negative).

## Hypotheses and decision rules

A hypothesis is supported in a corpus when the 95% interval of its contrast excludes zero in the predicted direction, refuted when it excludes zero the other way, and undecided otherwise.

- H1, front-loading.
  - H1a (where feeling words sit): gross charge density falls with relative position; the first Legendre coefficient of gross charge over position is negative.
  - H1b (what readers weigh): in the fitted position kernel, the first third of a text gets more weight than the last third.
  - H1c (human ratings only, exploratory): in EmoBank documents, rated intensity |V - 3| falls with sentence index.
- H2, dilution. With lexical charge held fixed, rated intensity falls as the share of buffer words rises: fitted kappa > 0, and the fitted kappa gives a higher held-out r than kappa = 0.
- H3, composition. Signatures of parts merge to the signature of the whole exactly (unit test, integer arithmetic). For VADER and the transformers the mean of sentence scores differs from the document score; report how often the sign differs.
- H4, cost. Single-core throughput is at least 100 times that of DistilBERT on the same CPU.
- Accuracy gate carried over from the February 2026 attempt, which failed it: mean Pearson r across corpora is at least VADER's. The transformers are expected to score higher than both.

## Ablations

Position moments off; dilution off (kappa = 0); shrinkage off (a = 0); shifter channels off; single bipolar channel. Same folds and metrics.

## Hierarchy analyses (descriptive)

- Pooled level against mean of chapter levels across novels; the gap equals Cov(exposure, level) / mean exposure. Report the share of novels and of novel pairs where the two disagree in sign.
- Between-chapter variance against a null that shuffles sentences across chapters.
- Degree-3 polynomial arc from the signature against a sliding-window arc: variance explained.
- Critics in the Pang and Lee data: difference in text level split into a within-rating part and a rating-mix part (Kitagawa decomposition).

## Deviations

All dated 13 November 2026. Test portions were first scored in run 1, with the plan exactly as written above.

1. Grid. In run 1 the chosen a sat at the top of its grid (a = 4). The grids were widened to a up to 32 and kappa up to 2. Development data then put the optimum inside the grid at the same values (a = 4, kappa = 0.5), so the choice did not change.
2. Tokeniser. After run 1 the tokeniser was rewritten with plain string operations, because a regular expression for emoji made large batches slow. Behaviour differs slightly: a URL becomes a few unknown words, an emoticon counts only after a space, and every sentence-ending mark maps to one token. The change was made for speed and was not tuned on test data. Every reported number comes from the rewritten tokeniser.
3. Bug. The selection of (a, kappa) for the uncalibrated level readout overwrote its best value whenever the calibrated readout improved. Fixed after run 1.
4. Design checks on development data only (`scripts/dev_explore.py`): neutral band 0, 0.125, 0.25, 0.375; negation window 2 to 4; modifier window 1 or 2; the extended lexicon. The planned values were kept.
5. Additions after run 1, not planned, reported as secondary: (a) refined word charges, one ridge problem centred on the norm charges, fitted leave-one-corpus-out; (b) a bag-of-words ridge trained on the same out-of-corpus pool, as the fair comparator for (a).
6. Transformers are scored on a seeded 5,000-review subsample of the Yelp test portion to bound CPU time. For Yelp, every method is compared on that subsample.
7. Addition, exploratory: how two parts combine and what neutral words do, measured on the human phrase ratings of the Stanford Sentiment Treebank (training trees). No scorer is involved.
8. H1b, H1c and the dilution partial correlation estimate a quantity and are not held-out predictions, so they use all portions of each corpus. Position analyses keep texts of ten or more words.

## Addendum: human and machine writing

Written and committed before any machine-written text was generated or scored.

Question. On the same prompt, does the signature of text written by a language model differ from the signature of text written by people, and in which measurable ways? A second question follows from the first: can a report of those differences help a model revise its own draft toward the human range?

Data.

- HC3 (Guo et al. 2023): questions with answers from people and from ChatGPT. One human and one ChatGPT answer per question (the first of each), both at least 30 words.
- PERSUADE 2.0: argumentative essays by US students in grades 6 to 12, 15 prompts, collected before 2021. A seeded sample of 10 essays per prompt (150 essays). For each sampled essay, two new essays written by Claude models through subagents that are told nothing about this study: (A) the assignment text and a word count matched to the student's essay; (B) the same, plus an instruction to write as a student in the same grade would. Three Claude model tiers in rotation.

Measures, all read from signatures and free of text length: hedon density, dolor density, net density, gross density (per 1,000 words); share of words charged; share of charge that is negated, intensified, or after a contrast word; ending lift (the cubic arc averaged over the last fifth of the text minus the text's mean density); sentence clustering (between-sentence variance of density divided by its expectation if words were dealt to sentences at random).

Hypotheses. Direction fixed here; decision rule as above, on paired differences (machine minus human, same prompt or question), resampling prompts for PERSUADE and questions for HC3.

- HL1. Machine text has higher net density.
- HL2. Machine text has lower dolor density.
- HL3. Machine text has lower sentence clustering (charge spread more evenly over sentences).
- HL4. Machine text has higher ending lift (it closes on a more positive note).
- HL5. A logistic regression on these measures alone tells the two apart: AUC above 0.5, interval excluding 0.5, cross-validated with all texts of a prompt or question in one fold. Reported next to the same model given word count alone, and separately for condition B.

Tone check. For 60 condition-A essays, two revisions by a subagent: a control instruction (revise so that it reads as written by a student in that grade) and the same instruction plus a report of how the draft's measures sit against human essays on other prompts. HT1: the mean squared standardised distance to the human reference is smaller after the revision that saw the report than after the control revision (paired). Secondary: the HL5 classifier's score for each revision.

These analyses are exploratory extensions of the main study. Prior work already reports that model output is less negative than human text; the tests here ask whether the signature recovers that and adds position and clustering.

Deviation from the addendum (13 November 2026, before any essay was generated). The seven text-dependent prompts ask students to argue from source passages that the corpus does not include, so a model could not be given the same task. The sample therefore uses the eight independent prompts, 20 student essays each (160 essays of 150 to 800 words), and four Claude tiers in rotation. One prompt has no grade recorded; its condition-B instruction says "high school student".

## Addendum 2: a revised hypothesis, tested on data that did not suggest it

Written and committed on 13 November 2026, after the planned tests refuted front-loading and before any of the data named below were examined.

The planned tests point the other way from front-loading. Stated as a hypothesis of its own:

Recency. What comes last in a text counts most toward a reader's judgement of it.

- R1, composition. In binary nodes of the treebank's development and test trees (the training trees suggested the hypothesis and are excluded), with both parts of two or more words, the later part has the larger weight in the regression of the phrase's human rating on the ratings of its two parts. Supported if the 95% interval of the later weight minus the earlier weight is above zero.
- R2, one-sided dilution. In the same trees, a rated part joined to a neutral part of two or more words keeps a smaller share of its rating when the neutral words follow it than when they come before it. Supported if the interval of the share kept with neutral words before, minus the share kept with neutral words after, is above zero.
- R3, documents. In IMDB film reviews of 100 or more words (test portion, binary labels, not used so far), the net charge per word of the last fifth of a review separates positive from negative reviews better than that of the first fifth. Supported if the interval of AUC(last fifth) minus AUC(first fifth) is above zero.

Intervals by bootstrap, resampling sentences for R1 and R2 and reviews for R3. These three tests are confirmatory for the revised hypothesis.

## Addendum 3: rewrites with anti-slop skills

Written and committed on 13 November 2026, before any such rewrite was generated.

Question. Do prose skills written to remove the marks of machine writing move a machine draft toward the human range on the tone measures?

Arms. The same 60 condition-A drafts as in the tone check. Each is rewritten twice more by a subagent that receives the control instruction plus an instruction to do the rewrite with one named skill: the `humanizer` skill, or the `sepia-recreate` skill. The interface-design skills (unslop, Hallmark, impeccable, taste) do not apply to essays. The `remove-ai-marks` skill strips provenance marks and is not a writing skill; it is not used.

Outcome and decision rule as for HT1: mean squared standardised distance to the human reference (student essays on other assignments), paired differences, resampling assignments.

- HS1. Distance after a skill rewrite differs from distance after the control revision. No direction is predicted: the skills target wording, and the measures target how feeling is carried. Reported for each skill, with the difference from the draft and the classifier's machine score as secondary outcomes.

## Addendum 4: the dial

Written and committed on 13 November 2026, before any dial revision was generated. The tone check above reports where a draft sits against a range. This experiment asks whether a model can be steered to a stated target.

Drafts. A seeded sample of 60 condition-A machine essays that were not used in the tone check (15 per model where available).

Targets. Each draft is assigned the measured profile of one student essay drawn at random (seed 0) from the other assignments. Targets are whole profiles of real essays, so that a set of revisions can match the spread of human writing and not only its centre.

Arms, two rounds each, by the model that wrote the draft.

- Dial. Round 1: the control instruction plus a sheet listing, for each of the ten measures, the draft's value and the target value. Round 2: the same for the round-1 revision, with its new values.
- Control. Round 1: the control instruction (revise so that it reads as a student of that grade wrote it). Round 2: the same instruction applied to the round-1 revision.

Hypotheses, on the round-2 texts. Decision rule as before, resampling assignments; results that resample essays are also given.

- HD1, the dial is followed. Across essays, the achieved value of a measure correlates with its assigned target in the dial arm more than in the control arm, for the mean correlation over the ten measures. Also reported: the mean squared standardised gap between achieved and target profiles, dial against control.
- HD2, the set moves toward the human set. The mean machine score from a classifier trained on other essays (student essays against condition-A machine essays, none from this sample) is lower for the dial arm than for the control arm.
- HD3, the prose survives. A judge model that does not know the arms reads the control and dial revisions of the same draft in random order and answers which is better written. The dial arm is not chosen less often than half the time (the interval of its share of wins is not wholly below 0.5).

Excerpts for the paper are taken from the essay at the median of the improvement in gap, not chosen by eye.

## Deviations in the addenda (13 November 2026)

1. Throughput was measured on a rented machine (Ryzen 5 5600X, RTX 3090), not on the laptop the plan implied, because long runs overheated the laptop. Processor and graphics-card timings come from that one host.
2. Essays. 318 of the 320 requested machine essays were written; two condition-A requests produced no file (one agent declined because the task did not match the request relayed to it). Condition A therefore has 158 pairs.
3. Declined requests. In the tone check the model declined 3 of 240 revision requests, and in the dial arm 11 of 60 essays were left without a second-round text, all by the smallest model, on the ground that making machine text read as a student's work misrepresents authorship. Declined requests were not retried; those essays leave the paired comparisons (57 of 60 and 48 of 60 remain).
4. The dial sheet lists eight measures (net and total charge follow from hedons and dolors); HD1 is evaluated on all ten, as written.
5. The judge for HD3 was Claude Opus 5.5, or Claude Sonnet 5.5 for drafts written by Opus. It was asked a second question, which of the two reads more like a student's own work, that the addendum did not list; it is reported as a secondary outcome.
6. The `remove-ai-marks` skill was left out of addendum 3 because it strips provenance marks and is not a writing skill.

## Deviations and corrections recorded after review (13 November 2026)

Added after AI referee agents reviewed the draft. Nothing above this line was edited; where an earlier line is wrong, the correction is here.

1. Recency (addendum 2) is withdrawn. R1, R2 and R3 each passed by the rule written above. R1 and R2 do not hold length fixed: in the held-out trees the later part is the longer one in 63% of pairs, and the rated parts compared in R2 averaged 3.8 words on one side and 9.1 on the other. With length held fixed both reverse (`scripts/exp_position.py parts`, exploratory; `results/position.json`, key `parts`). R3 stands as a result about where reviewers place their verdict.
2. Lexicon scale. The VADER list was put on the labMT scale by the ratio of root-mean-square values over shared words (slope 1.059), not by the regression the plan names (slope 1.002).
3. Resample counts. Pearson r and differences between methods use 2,000 bootstrap resamples as planned. Spearman rho uses 500, and the position, dilution, treebank and machine-writing analyses use 100 to 2,000.
4. Run 1. The results of the first scoring run were not saved, so deviations 1 to 3 of the first list cannot be checked against it.
5. Splits. For corpora without an official split the test portion is 50% as planned; the other half was divided 40/10 into training and development portions.
6. H2. The planned test (held-out r with fitted kappa against kappa = 0) is supported in four of six corpora and undecided in EmoBank and film reviews. The partial rank correlations and the curves against kappa are unplanned.
7. H3. The share of documents whose score differs in sign from the mean of their sentence scores was first computed for the transformers only. VADER was added later (5% of 300 film reviews).
8. Essay intervals. The planned bootstrap over eight assignments is too narrow for eight clusters. The paper prints delete-one-assignment jackknife intervals with a t reference and states the planned verdicts where they differ: HL1 was "refuted" for plain essays and "supported" for essays written as a student under the planned rule, and is undecided for both under the jackknife.
9. Timing of a record. The note "Deviation from the addendum (13 November 2026, before any essay was generated)" was committed in `6b4ba13`, after the essays had been written. The job files show that the eight-assignment sample was fixed before generation; the written record of it post-dates them.
10. HT1 and HS1. The planned outcome, distance to the centre of student essays, measures how typical a text is. Student essays themselves score 1.16 on it, above every machine draft and revision. The classifier's score is reported beside it.
11. HD2. The classifier was first trained on essays that excluded only the 48 complete pairs, then retrained excluding all 60 sampled essays. The second is reported.
12. HD3. The rule "not wholly below 0.5" is passed by any share whose interval reaches 0.5 and carries little information. Exact binomial intervals and the split by writer model are reported.
13. Blinding. Every writing, revising and judging agent received, besides its task, the harness's standing context, including the author's most recent message; for the dial revisions and the judges that message stated the hoped-for result. Output paths named the arm. The addendum's phrase "told nothing about this study" was not achieved.
14. Samples. The 60 drafts of the tone check and the 60 of the dial were each drawn 15 per model with one seed and cover seven of the eight assignments.
15. Refined charges are not shipped. A check of 20 words about groups of people and public figures, added after review, found that 14 moved down under refinement.
16. Chapters. The heading rule drops sections under 300 words and can keep front matter; in Moby-Dick it finds 129 sections for 135 chapters and an epilogue.
17. Pre-specification. All commits of the plan and its addenda were made locally on 13 November 2026 and pushed when the study was complete. They are the author's record and not an independent registration.
18. Standard error of the level. The formula in the first draft divided the standard deviation of the net charge by the exposure, as if the exposure were fixed. It overstated the error by up to 70% in simulated texts whose charge was all of one sign. `sgw.stderr` now uses the delta method for the ratio and matches simulation to within about 4% for texts of 200 words; for texts of 10 to 20 words it runs 10 to 20% low. For density (a = 0, kappa = 1) nothing changes, so no reported number moves except the level's error bar in Fig. 2c.
19. Tokeniser. A quote mark at the very end of a text was stripped when the text was scored in a batch and kept when it was scored alone. The fix pads both ends. None of the 192,385 rated texts changes.
20. Float spans. `sgw.span` on a float prefix table loses the position sums of a short span deep in a long text. It is documented, tested, and `sgw.fold` gives the accurate route. No reported number used the affected path.
21. Comparator added after review: VADER averaged over sentences, on all six corpora (`scripts/exp_accuracy.py vader_sentences`). Against it the signature leads on Facebook posts only and trails on Yelp.
22. HL4. Ending lift as registered averages the fitted cubic arc over the last fifth. The literal last-fifth density is now reported beside it: same direction, and for plain essays the jackknife interval reaches zero.
23. Intensifiers. With so, too, such and most removed from the intensifier list the essay difference remains.
24. Refusals and other requests that left no text: the agents' final messages are in `machine_text/declined_requests.json` (17 requests: 14 refusals by the smallest model, 2 essays set aside as unrelated to the relayed message, 1 revision reported as written with no file). Deviation 2 of the addenda says one agent declined; both essay agents set the task aside for that reason.
25. Judge. The plain revisions contain informal spellings (alot, dont) at 4.5 per 1,000 words against none in the dial revisions and 1.9 in the students' essays, so the judge's answer to "more like a student" is confounded with spelling.
26. History rewrite (14 November 2026). Two scripts from the 2024 version (`twittertestwrite.py`, `testweet.py`) held Twitter API credentials, and the repository also held a Windows executable (`pythonw.exe`). Every commit on every branch was rewritten to replace the credentials with `REDACTED` and drop the executable. No other file in any commit changed, and author and commit dates are unchanged, so each commit's content and time are as before; only the commit identifiers changed. Identifiers cited in this plan and in the paper were updated:

| Before the rewrite | After | Commit |
|---|---|---|
| `090411d` | `efe3fce` | Pre-register the anti-slop skill arms (addendum 3) before generating any such rewrite; add benchmark results and recency confirmation |
| `4b10b12` | `070d1ba` | Add scorer, experiments, first results and paper drafts; pre-register the human vs machine writing addendum |
| `7524c58` | `0c195da` | Archive 2024 scripts under legacy/ and commit the analysis plan before scoring any evaluation data |
| `77d4fa2` | `6b4ba13` | Pre-register the recency hypothesis (addendum 2) before examining held-out trees or IMDB; add machine essays, tone module, MCP server |
| `de181ec` | `f0f0e8f` | Pre-register the dial experiment (addendum 4) before generating any dial revision |

The referee reports in `reviews/` were written before the rewrite and cite the old identifiers. Some commit messages from that evening say "pre-register"; they are the author's record, not an independent registration.
