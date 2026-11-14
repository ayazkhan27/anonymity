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
