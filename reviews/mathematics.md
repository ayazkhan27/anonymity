# Referee report: the mathematics

Manuscript: "A mergeable measure of sentiment for text of any length" (sources in `paper/*.tex`, read at about 23:00 on 13 November 2026).

## What I examined

- Propositions 1 and 2 and their proofs, the merge equation, the remark on relative position, the readouts paragraph (level, standard error, Legendre arc), the pooled-versus-mean identity, the Kitagawa decomposition and the expected between-sentence variance behind "sentence clustering": `paper/body_a.tex`, `body_b.tex`, `body_c.tex`, `methods.tex`, `textmacros.tex`, `figures.tex`, the relevant macros in `numbers.tex`, and Fig. 1 as PNG.
- The code that implements them: `sgw/core.py` (all of it), `sgw/tone.py` (`profiles`), `scripts/exp_books.py` (all of it), the Fig. 1 and Fig. 5 blocks of `scripts/make_figures.py`, `tests/test_core.py`, and `results/books.json`, `results/critics.json`, `sgw/constants.json`, `sgw/reference.json`.
- Four numerical checks, each under one second on one core, calling `sgw.sketch_exact`, `sgw.merge`, `sgw.unmerge`, `sgw.prefix`, `sgw.span`, `sgw.arc`, `sgw.level`, `sgw.stderr` and `sgw.tokens` on synthetic input. No experiment script, model, LaTeX run or test suite was executed.

Derivations I checked and found correct as stated:

- Equation (2), the merge of position sums, and its implementation in `_shift`, `merge`, `unmerge`, `prefix`, `span`. Fig. 1b is consistent with it (7.3 + 13.8 + 10 x 2.28 = 43.9).
- Proposition 1 for parts joined by white space where the first part's last token is a sentence end: associativity, identity and inverse all follow from `T_l T_m = T_(l+m)`. Confirmed on examples in exact integers.
- Proposition 2 as stated for arbitrary real charge sequences, both directions, including the step from independence of the sequences to independence of their restrictions to a finite window, the Cayley-Hamilton step, and the description of the solution space (the sequences supported below position n for a zero root are needed and are present in the Methods statement).
- The conversion from power sums to Legendre moments in `legendre` and the arc in `arc`.
- Equation (4), the pooled-versus-mean identity, with the population covariance (divisor P), which is what `exp_books.py` line 67 uses; `identity_err_max` in `results/books.json` is 5e-17.
- The Kitagawa split in `exp_books.critics`: with word shares `w` and stratum densities `r`, `sum (wa+wb)/2 (ra-rb) + sum (ra+rb)/2 (wa-wb) = sum wa ra - sum wb rb`. The stored `identity_err` is at most 2e-17, no stratum is empty (902 to 1,770 reviews per critic over four ratings, no NaN in the intervals), and the quoted Berardinelli-Renshaw numbers (-0.1, -1.2, +1.1) match `results/critics.json`.
- The expected between-sentence variance `sigma^2 (m-1)/L`: exact for the word-weighted between-sentence sum of squares over L when sigma^2 has divisor L-1, and off by the factor L/(L-1) with the divisor L that `tone.py` uses. Simulation of random dealing: ratio 1.015 at L = 139 and 1.006 at L = 611.
- The count of nineteen signature entries, the level formula and its two limiting cases, and the kernel `1 + (b1 P1 + b2 P2 + b3 P3)/b0`.

## Concerns

### 1. Major. The standard error of the level is not the standard error of the level under the model the paper states

Methods, Readouts: "its standard error is 1000 sqrt(sum (h_i - d_i)^2 - (H - D)^2 / L) / (a + E + kappa B), which treats the charges of a text's words as independent draws." Code: `sgw.core.stderr`.

The square root is the standard deviation of the numerator H - D under independent draws. The denominator contains E, the number of charged words, which is a sum over the same draws. The formula treats it as a constant. It is therefore the standard error of the level only when kappa = 1 (denominator a + L), which covers density and Fig. 5a,b (`stderr(C, 0, 1.0)`), and not at the shipped level constants a = 1, kappa = 0.2 used in Fig. 1c and in `sgw.read`.

Check (4,000 texts of 200 independent words, 23% charged, `sgw.level` and `sgw.stderr`):

| Text | a, kappa | SD of level over texts | mean `stderr` | ratio |
|---|---|---|---|---|
| charge half positive | 1, 0.2 | 40.4 | 40.3 | 1.00 |
| charge 75% positive | 1, 0.2 | 36.3 | 39.2 | 1.08 |
| charge all positive | 1, 0.2 | 20.7 | 35.8 | 1.73 |
| charge all positive | 4, 0.5 | 18.3 | 21.9 | 1.20 |
| charge all positive, equal size | 0, 0 | 0.0 | 58.6 | unbounded |
| any | 0, 1 | agrees | agrees | 0.99 to 1.00 |

The error grows with how one-sided the text is, which is where a level is most often quoted. For Fig. 1c the title reads "level +275 ± 108"; the first-order (delta-method) value for the same twenty words is about 84.

The delta-method standard error needs no new signature entry. With R = (H - D)/(a + E + kappa B), each word contributes u_i = c_i - R (kappa + (1 - kappa) e_i), where e_i marks a charged word, so sum u_i = R a and sum u_i^2 = SS - 2R(H - D) + R^2 (E + kappa^2 B). Then SE = 1000 sqrt(sum u_i^2 - (R a)^2 / L) / (a + E + kappa B). In the simulation this matches the observed SD in every row (36.2, 20.8, 18.3, 0.0).

A second problem runs the other way. Independence of words is contradicted by the paper's own measurements: the human reference median of sentence clustering is 1.07 for essays and 1.24 for fiction (`sgw/reference.json`), and the between-chapter variance is 5.9 times its value under a sentence shuffle. Under positive dependence the independent-draws formula understates the variance of a chapter's density, so the 95% bars in Fig. 5a,b are too narrow: by about sqrt(1.24) if dependence stopped at the sentence, and by more if it extends further, which I did not measure.

Requested: replace the formula by the delta-method one (or state that it is exact only for kappa = 1), regenerate Fig. 1c and `level_se`, and either estimate the variance from sentence-level signatures (which the prefix table already holds) or say in the caption of Fig. 5 that the bars assume independent words and are a lower bound.

### 2. Major. Exact merging is claimed "from the phrase to the book" and for pages; Proposition 1 covers only cuts at sentence ends, and eight of the nineteen entries fail elsewhere

Introduction: "the score of a whole is the merge of the scores of its parts, at every level from the phrase to the book" and "every chapter, page and sentence can be read off afterwards". Results: "If span A is followed by span B, the counts and totals add" (no condition). Abstract: "merges exactly with the signatures of its neighbours".

Proposition 1 requires A to end at a sentence end, and the requirement is necessary. Checked in exact integers: `merge(sketch_exact("this is not"), sketch_exact("good at all."))` differs from `sketch_exact("this is not good at all.")`, and so does "bad but," followed by "good." (a clause end is not enough, because a contrast word reaches to the sentence end). A page, a fixed-size window of words, a tweet without closing punctuation, a heading, and any phrase inside a sentence are such cuts. At those cuts L, E, the eight position sums and the sum of squares still merge exactly, because charge does not depend on context. The eight operator entries do not, and through them the calibrated valence does not.

Two smaller gaps in the hypothesis:

- "AB" must mean A and B joined by white space. With direct concatenation the statement is false: A = "ok." and B = ":) nice" give tokens `['ok', '.', ',', 'nice']` for AB and `['emoticond', 'nice']` for B alone, because an emoticon counts only after a space. The tests join with a space and draw only alphabetic words, so neither this nor any cut inside a sentence is exercised.
- "Sentence end" means the tokeniser's sentence end, every full stop, exclamation mark or question mark, including those in abbreviations and numbers. The proposition should say so, since that is what makes it true.

Requested: state the condition wherever exact merging is claimed and drop "phrase" and "page", or make the claim true at any cut by adding the boundary state to the signature (remaining reach of each operator and the contrast flag at the right edge; the charge of the first three words, first two words and first sentence at the left edge). That extension is finite and keeps the merge associative.

### 3. Major. In the fast implementation a span read from two prefix rows has no usable position sums once the text is long; the stated relative error of 2e-13 is for forward merging only

Results: "A table of prefix signatures, one row per sentence, gives the signature of any contiguous run of sentences from two of its rows ... The fast implementation uses double-precision floats; over 356 novels its merged and single-pass signatures agree to a relative error below 2e-13." Abstract: "One pass over a book gives the score of every chapter and sentence." README: `sgw.span(pre, 120, 480)` on the float table. Methods concedes that the difference "loses precision for high moments of short spans deep inside long texts".

"Loses precision" understates it. Check with `sgw.prefix` and `sgw.span` on 10,000 synthetic sentences of 20 words (a 200,000-word book):

| Sentence read back | error in S1 | error in S2 | error in S3 (true value) | arc at start, middle, end, per 1,000 words |
|---|---|---|---|---|
| 10 | 2e-13 | 4e-11 | 1e-8 (4,571) | -66, 87, 168 (correct) |
| 1,000 | 3e-9 | 8e-5 | 2.2 (1,683) | 625, 49, -370 against 627, 49, -372 |
| 9,999 | 2e-7 | 0.02 | 960 (1,819) | 1845, 54, -1819 against 0, 54, 26 |

Density, level and the operator totals survive, because they use S0, L and E only. The arc and the calibrated valence do not: the readout has three position moments among its ten features, with coefficients of order one (Extended Data Table 3). The forward figure of 2e-13 says nothing about this, since subtraction is where the cancellation occurs. Fig. 5b is unaffected (density only).

Requested: say in Results, not only in Methods, which readouts of a span are reliable in floating point; make `span` accurate (re-merge the stored per-sentence rows of the span, or keep prefix rows in integers, or store the position sums of each prefix row about a local origin); add a float test that reads a late sentence back and compares its arc.

### 4. Major. The claim that polynomials are the only mergeable weights expressible in relative position is not proved by the remark offered for it

Results: "among mergeable weights, polynomials alone can be re-expressed in relative position". Methods: "a weight g((i + 1/2)/L) is available from a summary for every length L only if the span of weights is closed under rescaling of position as well as under shifts. A finite-dimensional space of functions of a real variable with both properties is the space of polynomials up to some degree".

Three gaps:

- The requirement that the discrete problem gives is that, for every integer L, the finite sequence g((i + 1/2)/L), 0 <= i < L, lies in the restriction of V to that window. That is a condition at integer dilations and on finite windows. The remark replaces it, without derivation, by closure of a space of functions on the real line under all real dilations s > 0, and then quotes a theorem about that different object.
- No regularity is assumed for g, and without it the claim is false. The grid points (2i + 1)/(2L) of length L are exactly the rationals in (0, 1) whose reduced denominator has the same power of two as 2L, so grids with L odd and L even are disjoint. Let g = 1 on the first set and 0 elsewhere. Then sum c_i g((i + 1/2)/L) equals S0 for odd L and 0 for even L, which is available from (L, S0), and g is not a polynomial.
- The real-variable theorem cited (Anselone and Korevaar) is for continuous functions; the remark does not say so.

For continuous g the claim is true and has a short direct proof in the paper's own terms. Let p be the annihilating polynomial of the shift on V from Proposition 2, and write p(z) = (z - 1)^m q(z) with q(1) != 0. The sequence u_L(i) = g((i + 1/2)/L) satisfies p(T) u_L = 0 away from the ends, for every L. Pair this with a smooth test function, sum by parts and let L grow: the m-th distributional derivative of g vanishes, so g is a polynomial of degree below m (and g = 0 if m = 0). I recommend stating the claim with "continuous g" and giving this argument in place of the present sketch.

### 5. Minor to major. The arc: stated accuracy, the check behind "10^-8", short texts, and the description of ending lift

Methods: "The sampled polynomials are orthogonal up to terms of order L^-2, so f is the least-squares cubic to that order; over the novels it agrees with a direct fit to 10^-8." Results: "The arc is the least-squares cubic fit of net charge against relative position."

- The order is right (sum P1^2 = (L^2 - 1)/(3L), sum P0 P2 = -1/(2L), odd-even pairs vanish by symmetry), but the constant is about 13: the normalised Gram matrix differs from the identity by 0.132 at L = 10 and 0.015 at L = 30. Against a direct least-squares cubic the arc is off by up to 10% of the curve's amplitude at L = 10, 1.6% at L = 30, 0.07% at L = 150 and 1e-8 (relative) at L = 20,000. The Results sentence should say "to order L^-2" as Methods does. Fig. 1c draws the arc for twenty words.
- For fewer than four words no least-squares cubic exists and the formula returns values of no meaning: `sgw.arc` on the one-word text "good" (density 526 per 1,000) gives -789, 690, 1184, 690, -789. `sgw.read` prints an arc for any text. A minimum length should be stated and enforced.
- The "10^-8" is not what the code asserts. `exp_books.py` line 86 calls `np.allclose(..., atol=1e-8)` with the default `rtol=1e-5`, so the tolerance is about 5e-7 at arc values near 0.05. My own check at L = 20,000 found a largest difference of 5e-10 and at L = 200,000 of 3e-12, so the statement is probably true, but the repository does not establish it. Set `rtol=0` or record the largest difference.
- Ending lift. Results: "The final fifth of a machine essay sits 16 per thousand words above the essay's own density, against 3 for the students". The measure is "the mean of the cubic arc over the last fifth of the text minus the text's density" (Methods), which is a weighted sum over all words with weight 2.8x^3 + 3.6x^2 + 0.72x - 1.2 at relative position x in [-1, 1]: -1.1 at the first word, -1.2 at the middle, +1.1 at the start of the last fifth and +5.9 at the last word. The density of the final fifth minus the density of the text has weights -1 and +4. Under independent charges the two correlate at 0.90. The quantity the sentence describes is available exactly from the signature of the last fifth, which is the paper's own selling point. Either report that quantity (and say whether HL4 and the dial results change) or reword the sentence, the label in `tone.py` ("net charge of the last fifth minus the text's mean") and the abstract's "upbeat ending" to describe an end-weighted cubic contrast. The 21-point mean in `tone.profiles` includes both end points; the bias is small.

### 6. Minor to major. Sentence clustering is not free of sentence length

Methods: "sentence clustering, the between-sentence variance of density divided by its expected value if the text's words were dealt to its sentences at random". The measures are called "length-free".

The normalisation is correct for the null (see above), so the ratio has mean one when words are exchangeable whatever the sentence lengths. Away from the null it is not comparable between writers whose sentences differ in length. If neighbouring words share mood with within-sentence correlation r, the ratio is about (m/(m-1)) (1 + (n - 1) r) for sentences of n words. Check: one word process (mood persisting in runs of 15 words, independent of where sentences end), cut into sentences of 8, 12, 20 and 30 words, gives mean clustering 1.21, 1.30, 1.40 and 1.53. HL3 compares ChatGPT with people and Claude with students; if their sentences differ in length, the contrast in clustering mixes that difference with any difference in how charge is spread. The ratio is also noisy for one text (null SD 0.52 at m = 8, 0.29 at m = 25).

Requested: report mean sentence length by source next to HL3, and either condition on it or replace the ratio by an estimate of r (subtract one and divide by n - 1, with n the size-weighted mean sentence length). Methods should say that the between-sentence variance is word-weighted and that sigma^2 has divisor L.

### 7. Minor. Proposition 2: scope, and how the text reports it

- The "only if" direction uses charges that range over an interval. For the charge set the method uses (integers in thousandths between -1000 and 1000) it fails: with the single weight phi(i) = 2001^(i^2), the sum is a balanced base-2001 expansion and determines the whole charge sequence, so the summary of AB is a function of the summaries of A and B although the span of phi is not closed under the shift. The statement is correct as written ("a real charge sequence"); the Results paraphrase should carry the same condition, or F should be required to be continuous.
- Results: "such weights are combinations of i^k rho^i: polynomials, exponentials and their products". This drops the finitely supported weights that the Methods statement includes (the charge of the first word is mergeable) and does not say that rho may be complex, which adds oscillating weights.
- The proposition covers weights that depend on position from the start only, with F allowed to use L. Recency weights rho^(L-1-i) are covered in that sense (as rho^-i times a factor in L) but only through a growing weight, which is unusable in floating point; the direct recurrence S(AB) = rho^(L_B) S(A) + S(B) is the stable form and deserves a sentence, since recency is the paper's hypothesis.
- Proof of Proposition 1: T_l as applied to the vector of position sums is the transpose of the matrix of x -> x + l in the monomial basis. The group law is unaffected.
- Methods: "the test suite ... checks the propositions below for exact equality on random texts". Only Proposition 1 is tested.
- The link to Hankel rank is an analogy. The alphabet here is the real line and the dependence on letters is linear; the theorem cited is for finite alphabets, and the usual sources for the rank statement are Carlyle and Paz (1971) and Fliess (1974) rather than Schützenberger (1961). I did not check these records online.

### 8. Minor. Pooled-versus-mean identity and the decomposition

- Equation (4) needs the words "population covariance (divisor P)"; with the sample covariance it is false.
- "For parts with exposures D_p": D is already the dolors, and "exposure" is already a + E + kappa B. The identity as used has D_p = L_p. It does not extend to the level with a > 0, because the prior a enters every part's exposure and the whole's only once; the pooled level is then not the exposure-weighted mean of part levels. One sentence would prevent the misreading.
- "a signature kept per stratum makes both parts available, and a pooled bounded score does not": any score that is a ratio of two sums kept per stratum does the same, including the hedonometer mean with its word count. Likewise the Introduction's "the value for a whole cannot be computed from the values of its parts" is true of squashed sums and class probabilities but not of means, which merge once their counts are kept, and density is such a mean.
- "Chapters differ by more than chance ... One number for a book hides most of what its chapters do." A variance 5.9 times the sentence-shuffle value shows that sentences are not exchangeable across the book. It does not measure how much a single number hides, and a shuffle of sentences cannot separate chapter structure from any dependence between neighbouring sentences; a null that moves the chapter boundaries and keeps the order would.

### 9. Minor. Tokeniser against its description

Methods: "Words are runs of letters and apostrophes." `_ASCII` maps only code points below 128, so any other character survives as a word or part of one: `tokens("good « bad » • fine")` returns six tokens and L = 6. Guillemets, bullets, non-ASCII digits and accented letters all count toward L and B, which changes density and level on text with such characters. This also bears on Proposition 1 only in that the definition of a word should match the code.

## What I could not check

- I did not run the test suite, any experiment script or LaTeX, so "tested for exact equality" rests on reading `tests/test_core.py`, and the PDF was not compared with the sources.
- I did not recompute any number from the corpora; the values from `results/*.json` are taken as stored.
- The size of the standard-error problem on real texts (concern 1), the effect of exact last-fifth density on HL4 (concern 5) and the sentence lengths of human and machine texts (concern 6) need the corpora and were not computed. The simulations show the direction and the possible size only.
- The proof sketch I give for continuous g in concern 4 is an outline and should be written out and checked by the author.
- References were not checked against primary records; no web request was made.
- The rules for this review allowed no file in the repository other than this report, so the check scripts are not kept there; each is described above closely enough to rewrite (synthetic independent words with 23% charged, charges near ±0.45; `sgw` functions called on signatures built from them).
