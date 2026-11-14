# Referee report: writing and presentation

Manuscript: "A mergeable measure of sentiment for text of any length" (`paper/sgw.pdf`, sources `paper/*.tex`).
Lens: clarity for a general scientific reader, undefined terms, redundancy, section order, legibility of figures and tables, machine-written prose.

## What I examined

- All manuscript sources: `abstract.tex`, `body_a.tex`, `body_b.tex`, `body_c.tex`, `methods.tex`, `textmacros.tex`, `figures.tex`, `extended.tex`, `preamble.tex`, `sgw.tex`, every `table_*.tex`, `excerpt.tex`, and the values in `numbers.tex` of all 350 macros used in the text.
- All eight figure PNGs in `paper/figures/`, and pages 4 and 5 of `sgw.pdf` for the printed layout.
- `README.md` (read once), `results/position.json` (decile values only), `scripts/make_numbers.py` (number formatting only).
- Small checks run (each under five seconds, one core): every custom macro used is defined; every `\ref` has a label; every sentence quoted below is present in the sources; four arithmetic reconstructions noted in the text.

**The manuscript changed while I read it.** Source files were rewritten between 23:15 and 23:22 EST on 13 Nov 2026 and the PDF was rebuilt at 23:22:23. Several problems I found in the first pass were fixed during the review and are not reported: three Extended Data tables referenced but not included, captions of Figs 3, 4, 7 and Extended Data Fig. 1 that described older figures, hyphens in place of minus signs, and a Methods statement that the writing agents were blind, which contradicted the Results. Everything below was re-verified against the sources as they stood at 23:27 EST. One build problem remains: `figures/fig3_position.pdf` (23:22:39) is newer than `sgw.pdf` (23:22:23), so the PDF does not contain the current Fig. 3.

As of that time the sources are consistent in the mechanical sense: no undefined macro, no undefined reference, no placeholder number.

## Concerns

### 1. Major. "They sit late in every kind of text examined" says more than the data show, and it is said three times

Quoted:

- Abstract: "Feeling words are not front-loaded: they sit late in every kind of text examined, and whether early words weigh more with readers varies by genre."
- Introduction: "Front-loading fails as a claim about placement: charged words sit late, not early, in every kind of text examined."
- Discussion: "Feeling words are not placed early; they sit late in every kind of text examined."
- The README repeats it: "Charged words sit late in every kind of text examined."

Evidence. Extended Data Table "where" gives the charge-weighted mean position on a scale from -1 to +1: Yelp reviews +0.002 (hedons alone -0.001), novel chapters +0.007, review paragraphs +0.017, Facebook posts +0.021, film reviews +0.023, SST +0.050, EmoBank +0.060, tweets +0.105. Five of the eight are within 0.025 of the centre. The decile values in `results/position.json` show where the rest comes from: in all eight kinds of unit, tenths two to nine lie between 0.91 and 1.16 of the mean; the departure is confined to the first tenth (0.65 to 0.75 in EmoBank, SST and tweets) and the last tenth (1.34 to 1.55 in those and Facebook posts). Yelp opens at 1.09 and closes at 1.11; novel chapters run from 0.97 to 1.02. The text itself gives a syntactic reason ("English sentences tend to open with determiners and pronouns and close on content") and says "Yelp reviews are the one place with a charged opening". What is shown is the absence of front-loading, an end-of-sentence bump in short texts, and near-uniform placement in long ones. "Sit late in every kind of text" is a reading of sign tests on very large samples.

Replacements:

- Abstract: "Feeling words are not front-loaded. In sentences, tweets and posts the last tenth of the words carries 1.3 to 1.5 times the average charge; in reviews and novel chapters charge is spread almost evenly. Whether early words weigh more with readers varies by genre."
- Introduction: "Front-loading fails as a claim about placement. In no kind of text examined are charged words concentrated at the start, and in sentences and short posts they are concentrated in the last tenth."
- Discussion: "Feeling words are not placed early. Their mean position is at or just after the midpoint in all eight kinds of unit, and clearly after it only in sentences and short posts."

### 2. Major. The opening premise blames boundedness; the paper's own equation shows the cause is the missing denominator

Quoted:

- Abstract: "Sentiment scores for text are usually bounded numbers, a class probability or a normalised word count. Such scores do not compose: the score of a chapter cannot be computed from the scores of its sentences".
- Introduction: "A bounded score is intensive in the sense that temperature is intensive. It does not grow with the amount of text, and the value for a whole cannot be computed from the values of its parts."
- Results: "Bounded scores depend on how a long text is cut" and "a signature kept per stratum makes both parts available, and a pooled bounded score does not."

Evidence. Equation (4) of the manuscript computes the pooled density exactly from the densities of the parts and their word counts. A density is bounded and intensive, and it composes once its denominator is kept. The text says as much two paragraphs earlier: "What depends on the cut is the average." Its own numbers agree: when the signature's densities are averaged over chunks, 8% of book pairs swap order at 125-sentence chunks and 10% at chapters, against 11% and 19% for VADER. The manuscript also notes that "the hedonometer's average happiness is a density taken over charged words only", that is, a normalised word count that merges exactly with a count beside it. A reader from that tradition will object in the second sentence of the abstract. The accurate statement is that a single number reported without its counts cannot be recombined, and that a squashed sum (VADER) or a class probability has no such counts to keep.

Replacements:

- Abstract: "Sentiment scores are usually reported as one bounded number per text, a class probability or a normalised word count, without the counts behind it. Such a number cannot be recombined: the score of a chapter cannot be recovered from the scores of its sentences, so long texts are cut and averaged, and the answer depends on the cut."
- Results: "A mean of chunk scores depends on how a long text is cut".
- Results: "a signature kept per stratum makes both parts available; a single pooled score of any kind does not."

### 3. Major. The front-loading section promises three predictions and never states the third

Quoted, in order: "Front-loading makes three predictions, and I tested each (Fig. 3)." / "The first is that charged words come early." / "The second prediction is that readers weigh early words more." / "A third analysis, added after the first results, uses human ratings alone." / "The plan's third test, within EmoBank documents, is undecided: rated intensity neither falls nor rises with sentence position".

Evidence. The reader counts first, second, third and takes the 270-word treebank paragraph for the third prediction. It is an unplanned analysis. The planned third test follows in one sentence, and the prediction it tests is never given in the main text; only Methods labels the three as H1a, H1b and H1c.

Replacement for the opening: "The plan drew three predictions from front-loading: that charged words come early (H1a), that readers weigh early words more (H1b), and that rated intensity falls from the first sentence of a document to the last (H1c). I take them in turn and then report an analysis that was not planned (Fig. 3)." Move the EmoBank sentence directly after the kernel paragraph and open it with "The third prediction is undecided: within EmoBank documents rated intensity neither falls nor rises with sentence position".

### 4. Major. Section order: the reader meets the refutation of recency before the hypothesis, and the same verdict five times

- The recency section comes after the dilution section. Its tests R1 and R3 are about position and R2 is about dilution. The length-controlled results that refute it are already given two sections earlier ("Where the two parts are equally long, the earlier one has the larger weight: 0.71 against 0.57") and one section earlier ("matched on the lengths of both parts"). The recency section then repeats both on held-out trees. Suggested order: planned position tests; recency proposed and withdrawn, carrying the whole treebank weight analysis (uncontrolled, then controlled, training and held-out trees together); dilution, carrying the matched comparison of sides.
- The front-loading and recency verdict is stated in the abstract, in the last paragraph of the introduction, in the closing paragraph of "Where the feeling sits", in the closing paragraph of the recency section, and in the third paragraph of the Discussion. Cut the introduction's version to two sentences and the Discussion's to the lesson drawn.
- Verbatim repeats: "Word norms rate words, not events" (Results and Discussion); the standard error "is not a prediction interval for a human rating" (Results and Discussion); the list of precedents with the same citations (Introduction paragraph 4 and Discussion paragraph 2).

### 5. Major. Scope and length: the human and machine writing study does not use the property in the title

Quoted bridge: "A measure that is cheap and does not grow with length can be pointed at a current question".

Evidence. By my count the section and its dial paragraphs run to about 2,350 of roughly 7,000 words of main text, with a further 900 words of Methods, a five-panel figure and two Extended Data tables. The essays are 150 to 800 words; nothing is merged. By the manuscript's own account the outcomes are mostly undecided or null ("Overall positivity is undecided", "HL2 and HL3 are undecided for essays", "Following the dial did not make the essays more like student writing than a plain revision did"), and the planned distance outcome "turned out to measure how typical a text is, and not how student-like". The abstract is one paragraph of about 325 words covering seven topics, in a layout (Extended Data, `naturemag` references) whose summary paragraph is normally under 200.

Recommendation. Report the HC3 and essay comparison in one paragraph with Fig. 7a,b, move the revision and dial experiments to a separate manuscript or to Extended Data, and cut the abstract to the measure, its agreement and cost, the two hypotheses, and the chunking result.

### 6. Major. Terms and symbols that are overloaded or used before they are defined

- "exposure" has three meanings. Equation (3): "The denominator is the exposure, the amount of evidence behind the score", that is a + E + κB. Equation (4): "For parts with exposures $D_p$ and densities $\lambda_p$", where the denominator of a density is the word count L. Methods, refined charges: "With exposure $a+L$".
- D is the dolor total ("dolors $D=S^{-}_{0}$") and, in equation (4), the exposure of a part. λ is a density in equation (4) and a ridge penalty and an exponent in Methods. In one Methods paragraph h is the labMT rating ("The charge of a word is $c=(h-5)/4$") and then the positive charge ("Positive charge is $h=\max(c,0)$ hedons"). B is the buffer-word count and the second span.
  Replacement for equation (4): "For parts with word counts $n_p$ and densities $\delta_p$, $\sum_p n_p\delta_p/\sum_p n_p=\overline{\delta}+\Cov(n,\delta)/\overline{n}$". In Methods write the labMT rating as $v$.
- "ten signature entries" lists nine. Quoted: "It is a linear combination of ten signature entries, each divided by the exposure: hedons, dolors, three position moments of net charge, and the charge under each operator." With four operators that is 2 + 3 + 4. Methods and the table of constants list ten because negation has two channels. Replacement: "It is a linear combination of ten signature entries, each divided by the exposure: hedons, dolors, three position moments of net charge, negated hedons, negated dolors, and the net charge that is intensified, diminished, and after a contrast word."
- "level" is a defined readout (equation 3) and is also used loosely: "The level of these shares is pulled down by regression to the mean", "scores near the book's own level" (density is meant), "at every level from the phrase to the book". Replace with "size", "density" and "scale".
- "granularity holds" is the paper's own coinage and appears undefined in the abstract ("Here I describe a measure for which granularity holds"). Replacement: "Here I describe a measure that composes exactly: the score of a whole is computed from the scores of its parts."
- "A signature is extensive." "Intensive" was explained by analogy with temperature; "extensive" is not explained. Replacement: "A signature is extensive: it grows with the text, as mass does."
- Used in Results before any definition: "position kernel" (defined in the caption of Fig. 3c and in Methods), "rated intensity" (Methods only), "clustering" and "ending lift" (Methods only), "HC3" (never expanded), "the dial", "prose skills", "judge". Methods calls the first revision experiment "the tone check"; Results calls it "a report".
- "A word rated within a quarter of the scale from neutral gets no charge". The rule is |c| < 0.25, one point on the nine-point scale, an eighth of its range. Replacement: "A word whose mean rating lies within one point of the midpoint of the nine-point scale gets no charge". The README has the same sentence.
- "an earlier version" refers to two different things: in the abstract ("Two hypotheses from an earlier version") the 2024 manuscript, in Results ("which an earlier version of the method had failed") the February 2026 embedding method.
- "a referee agent" appears in the Introduction and twice in Results and is explained only at the end of Methods. At first use write "an AI reviewer of the draft (Methods, Use of AI tools)".

### 7. Major. Sentences whose sign or structure can be read two ways

Numbers are shown as they print.

1. "the share kept is -0.050 [-0.058, -0.042] lower when the neutral words come first than when they follow." A negative amount lower. Replacement: "the share kept is 0.050 lower (interval 0.042 to 0.058) when the neutral words come first than when they follow."
2. "lower in dolor density by -0.06, lower in clustering by -0.04". Replacement: "lower in dolor density by 0.06 and in clustering by 0.04".
3. "Within the same rating Berardinelli's prose is the more negative by -1.2 [-2.5, +0.0], an interval that reaches zero". Replacement: "Within the same rating Berardinelli's density is lower by 1.2 per thousand words, with an interval from 2.5 lower to no difference".
4. "in favour of the signature in 3 of six corpora and in favour of VADER in 0." Replacement: "in favour of the signature in three of the six corpora and in favour of VADER in none."
5. "Without the neutral prior (a=0) it falls by 0.010, a mean over a gain from the prior in 2 corpora, a small loss in film reviews and no clear change in the other three." Replacement: "Without the neutral prior (a=0) the mean falls by 0.010: the prior helps in two corpora, costs a little in film reviews and makes no clear difference in the other three."
6. "turns 167,000 sentences per second, or 2.6 million words, into signatures and levels". Replacement: "converts 167,000 sentences (2.6 million words) per second into signatures and levels".
7. "The level of these shares is pulled down by regression to the mean, because the rated part was selected for a strong rating; the trend with m and the comparison between sides are not." Replacement: "Because the rated word was selected for a strong rating, regression to the mean lowers every share. It does not produce the fall with m or the difference between sides."
8. "for m of nine or more" and "for m of 5 to 8" (two numeral styles from one macro). Replacement: "for nine or more neutral words" and "for five to eight".
9. "The cubic arc of each tale's merged signature agrees in sign with the cubic arc of its human labels in 70% of tales". Replacement: "The cubic arc of a tale's merged signature correlates positively with the cubic arc of its human labels in 70% of tales".
10. "The dependence on length is modest: a plain mean of the two part ratings accounts for the phrase rating as well as a mean weighted by length". The previous sentences say the weight rises from 0.29 to 0.86 with a part's share, and the Discussion makes this the main positive finding ("What the phrase ratings support is plainer than either hypothesis: a part's weight in the whole grows with its share of the words, by less than proportion"). A threefold rise is not modest; what is small is the gain in fit. Replacement: "Weight rises with share, yet weighting by length does not improve the fit: a plain mean of the two part ratings accounts for the phrase rating as well as a length-weighted mean (R² 0.59 and 0.58)." Then say why, if known.
11. "Overall positivity is undecided. Plain essays are less positive than the students' (-0.23 [-0.48, +0.03]) and essays written as a student more positive (+0.29 [-0.01, +0.60])". Two differences are asserted directly after being called undecided. Replacement: "Overall positivity is undecided: the estimates point in opposite directions under the two instructions (-0.23 [-0.48, +0.03] plain, +0.29 [-0.01, +0.60] as a student) and both intervals include zero."
12. "Of 20 words for groups of people and two public figures". Twenty plus two, or twenty in all? The table has twenty. Replacement: "Of 20 words checked, 18 for groups of people and two names of public figures".
13. The *Moby-Dick* sentence: "Fig. 5a,b shows *Moby-Dick*: 213,523 words carrying 11,790 hd and 3,539 dl, a density of +39 per thousand words, with each of the 129 sections that the chapter rule finds in it (the book has 135 chapters and an epilogue; Methods) and twelve spans of the last one read from the same pass." Replacement: "Fig. 5a shows *Moby-Dick* section by section: 213,523 words, 11,790 hd, 3,539 dl, a density of +39 per thousand words. The chapter rule finds 129 sections in the book's 135 chapters and epilogue (Methods). Fig. 5b cuts the last section into twelve spans, each read from the same pass."

### 8. Minor. Prose that reads as machine-written

The draft avoids "not X but Y" literally (no instance found) and uses its near relatives instead. Each item gives the sentence and a replacement.

Aphorisms and paired contrasts:

- "Invariance is not validity." and, closing the same paragraph, "Merging removes the dependence on the cut. It does not make a score built from word ratings a better reader." Replacement: open with "A score that does not change with the cut is not for that reason accurate." and end the paragraph at the fairy-tale result.
- "What depends on the cut is the average." Replacement: "The dependence comes from averaging, whatever is averaged."
- "The differences are real and modest." Delete; the next sentence gives the areas under the curve.
- "The dial revisions read as more polished, and polish is not what a student's essay has." This is also an interpretation with no evidence beyond two questions put to a model judge. Replacement: "The judge's two answers suggest that the dial revisions read as more polished than a student's essay."
- "It turned out to measure how typical a text is, and not how student-like." Replacement: "It measures how typical a text is; a student essay is not typical of student essays."
- "X, not Y" and "X and not Y" as a habit: "sit late, not early"; "a risk and not an observation"; "Word norms rate words, not events" (twice); "chosen by rule and not by eye"; "a model, not a human reader"; "which cluster more than the students' and not less". Keep the form where the negated half is news and state the others positively, for example "chosen by rule" and "its judge was a model".

Staged openers and one-line verdicts:

- "One hoped-for property did not hold." Replacement: "The standard error of a level does not track the error against human ratings."
- "One more observation belongs here." Replacement: "The models sometimes refused these requests."
- "A caution applies to every number in this section." Delete.
- "Dilution fares better (Fig. 4)." Replacement: "Dilution is supported (Fig. 4)."
- "For sentiment it says two things." Delete and begin "First, the decay factor".
- "A report of ranges is a weak instrument." … "A dial is stronger." Replacement: "A report of ranges acts only on drafts outside a wide band. I therefore tested a stronger instrument, a dial:".
- "The dial was followed." Fold into the next sentence: "The models followed the dial: achieved values correlate with assigned targets at".
- "The 2024 manuscript had dilution right." Replacement: "On dilution the 2024 manuscript is supported."
- "The sizes are properties of these corpora." and "The chapter rule is approximate." Attach each to the sentence before it.
- "None of this makes a detector." Fold into the next sentence: "These separations are far too weak for a detector, and purpose-built detectors".

Questions as paragraph openers, three times: "Which position weights allow a merge?", "How far apart are a pooled value and a mean of parts?", "Can the measures steer a revision?" Replacements: "Only some position weights allow a merge."; "The gap between a pooled value and a mean of parts has a closed form."; "I then tested whether the measures can steer a revision."

Stock phrases and announced lists:

- "Removing parts of the signature shows what earns its place". Replacement: "Removing parts of the signature shows which of them contribute to agreement".
- "plainer than either hypothesis". Replacement: "What the phrase ratings support is that a part's weight grows with its share of the words, by less than proportion."
- "Three practical problems follow." The third is then withdrawn as "a risk and not an observation". Write "Two practical problems follow, and a third is possible."
- "These students wrote *very* next to their feeling words more often than these models did." A closer that restates the numbers before it. Delete.

### 9. Minor. Figures

Sizes. The PNGs are 220 dpi and 6.9 to 7.3 inches wide, so they print at about full text width (7.06 in) with lettering near 6.5 to 7 pt. That is legible. The problems are overplotting, clipping and labels.

- **Fig. 1.** (i) In panel c "level +275 ± 108" is printed as the title of the arc plot, whose axis is "net per 1,000 words". The level is not a property of the arc; put it beside the totals. (ii) Panel a draws *good* in "not good" as +0.53 hedons, and the level of +275 counts it as positive (1000 × 3.14 / (1 + 8 + 0.2 × 12) = 275). The schematic therefore shows the level misreading its own example, without comment. Add to the caption: "Totals and level ignore the operator channels; only the calibrated valence uses them." (iii) Panel b shows seven of the nineteen entries without saying so.
- **Fig. 2.** In panel b the labels "TextBlob" and "Hedonometer" almost touch and neither is clearly attached to its marker. Panel a is clear.
- **Fig. 3.** (i) Panel a, left: the last-tenth point for Facebook posts (1.372) lies exactly under the point for SST (1.372). The green line appears to end at 0.93, which contradicts the text ("in those three and in Facebook posts the last tenth carries 1.34 to 1.55"). Offset the markers or draw Facebook last. (ii) The two halves of panel a have different vertical scales (0.65 to 1.55 and 0.87 to 1.12) and one axis label. (iii) Panel c: the film-review kernel leaves the axes at both ends and at the bottom. The caption concedes it "is poorly determined"; give it its own axis or drop it. (iv) The bracketed contrasts under each panel of c read as axis labels; prefix them "first third minus last third". (v) The PDF must be rebuilt to pick up the current Fig. 3 file.
- **Fig. 4.** Panel c: the κ axis is categorical (0, .05, .1, .2, .5, 1, 2, 4 at equal spacing) and neither axis nor caption says so. The label "Film reviews" floats above "EmoBank", away from the end of its line. Colours for corpora change between figures: film reviews are blue and Yelp orange in Fig. 3a, black and grey here and in Extended Data Fig. 1a, where blue is EmoBank and orange is tweets.
- **Fig. 5.** (i) Panel a: the title now says "129 sections", the legend still says "chapters, with 95% intervals". The caption admits the intervals are "too narrow where charge clusters"; either draw intervals that respect clustering or remove them. (ii) Panel b: the label "whole book" is overprinted by a bar and a whisker, and the axis starts at zero although the bars are differences from 39. (iii) Panel e: the label "first mode" is printed across its own curve.
- **Fig. 6.** Panel c: the series end at different points, and the caption supplies the missing value in words ("the series for averaged densities ends at 125 sentences (10% with whole chapters)"). Plot the point.
- **Fig. 7.** Five panels in 7.2 × 5.2 inches. In panel a the ChatGPT points show no intervals (20,091 pairs; say "intervals narrower than the marker"), and the legend floats above the panel beside nothing. The order of measures in panel a is not the order of the text or of HL1 to HL4.
- **Extended Data Fig. 1.** Panel a has a truncated axis (0.70 to 1.02) and no intervals; the text bases a claim on its direction in each corpus.

### 10. Minor. Tables

- Extended Data tables are set in `\scriptsize` under a 9 pt class, that is 6 pt. Tables 1 and 2 hold eleven rows by seven columns of estimates with bracketed intervals at that size. Move the intervals to a second line, or to a supplementary table.
- The table of coefficients (`tab:constants`) is never cited in the text. The "readings" in the text and in the table of readings cannot be derived from it by a reader without the formulas. I reconstructed them: dolor / hedon 9.554 / 5.850 = 1.63; negated hedon (5.850 - 15.196) / 5.850 = -1.60; negated dolor (-9.554 + 9.643) / -9.554 = -0.01; intensifier 7.817 over the mean of 5.850 and 9.554 = 1.02. State these in the caption of the table of readings and cite the table of coefficients there.
- Corpus names differ between tables and text: "SemEval-2018 tweets" and "Tweets"; "SST", "SST sentences", "the treebank", "film-review sentences" and "film-review phrases" for one corpus.

### 11. Minor. Sources named but not cited

`refs.bib` has no entry for any of these (checked by search): the author's 2024 manuscript; TextBlob; the IMDB reviews; GoEmotions; Project Gutenberg; the two "published prose skills" (*humanizer*, *sepia-recreate*).

- The 2024 manuscript carries weight in the argument: "The work began with a manuscript I wrote in 2024", "The decay factor of the 2024 manuscript", "The 2024 manuscript had dilution right". A reader cannot check what it claimed. The README describes the 2024 version differently ("predicted an average lexicon score with a random forest"). Cite an archived copy, or write "an unpublished 2024 draft by the author (scripts in `legacy/2024`)" and drop the verdict sentences about it.
- TextBlob is the method behind the abstract's "it trails a simpler lexicon method" and is in both accuracy tables.
- The 22,001 IMDB reviews are the data for test R3. The Results send the reader to Methods for them, and Methods does not mention them.

### 12. Minor. Voice and attribution

The text is in the first person throughout ("I wrote it down with three predictions and committed them", "I computed", "I scored"), and Methods states that "The code, the analyses and the text of this draft were produced with Claude (Anthropic), an AI assistant working in a coding environment under the author's direction". A reader who reaches that statement on the last page will reread every "I" with doubt. Either move a one-sentence version of the statement to the end of the Introduction, or use the passive for actions the assistant performed and keep "I" for the author's decisions.

### 13. Minor. Smaller points

- "at every level from the phrase to the book" and "every chapter, page and sentence can be read off afterwards". Proposition 1 requires the first span to end at a sentence end, and pages and phrases do not. Write "from the sentence to the book" and drop "page", or state the condition in the main text ("Signatures merge at sentence ends").
- Abstract: "at 4,500 and 8,700 times their speed on one processor core". The Results add that against DistilBERT on a graphics card the factor is 14. The abstract and the Discussion ("thousands of times the throughput per processor core") should carry both figures: "at 4,500 and 8,700 times their speed on one processor core, and 14 times the speed of the smaller one on a graphics card".
- Methods: "raised mean agreement on development data from 0.521 to 0.534". These two values are typed, not generated, in a paper whose Code availability section says the numbers come from `make_numbers.py`.
- "Declarations. Funding, acknowledgements and competing-interest statements are to be completed by the author before submission." A placeholder in the manuscript.
- The paragraph on refusals sits under "One more observation belongs here." It reports that the models judged the task (making machine essays read as student work) a misrepresentation of authorship. That deserves its own short subsection or a place in the Discussion, with the author's position stated.
- The limits paragraph of the Discussion is a single paragraph of about 330 words with twelve separate limits. Break it in two: limits of the measure, limits of the study.

## What I could not check

- The full rendered PDF. I viewed two of its fourteen pages, the PDF was rebuilt during the review, and I did not run LaTeX. Float placement, page breaks, overfull lines and the appearance of the Extended Data tables at 6 pt are unverified.
- Legibility on paper. I judged it from the PNG pixel sizes and resolution, not from a printout, and I did not compare the PDF versions of the figures with the PNGs.
- The references. I did not use the web, so I confirmed no citation against a primary record. The absence of the sources in concern 11 is from a search of `refs.bib` only.
- The 2024 manuscript and the February 2026 version, neither of which I read.
- Whether the repository URL on the title page resolves.
- The numbers. Apart from the decile values, the four reconstructed readings, the level in Fig. 1 and the count of 63,190 pairs, I took every value from `numbers.tex` as given.
- `integrations/`, `machine_text/`, `tests/`, the library and the experiment scripts, which are outside this lens. I read the README once and `ANALYSIS_PLAN.md` not at all.
- Later edits. The sources were still being changed at 23:22; any change after 23:27 EST is not reflected here.
