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

None yet.
