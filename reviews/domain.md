# Referee report: sentiment-analysis lens

Manuscript: "A mergeable measure of sentiment for text of any length" (`paper/sgw.pdf`, built 13 Nov 2026 22:16).
Scope of this report: how prior work is represented, whether novelty is claimed honestly, whether the comparators (VADER, TextBlob, the hedonometer, two transformer classifiers) are used fairly, which baselines, datasets and citations are missing, and whether the known limits of lexicon methods are acknowledged.

## What I examined

- All manuscript sources (`abstract.tex`, `body_a/b/c.tex`, `methods.tex`, `extended.tex`, `textmacros.tex`, the three tables, the Fig. 2 and Fig. 6 captions) and `paper/figures/fig2_accuracy.png`.
- `paper/numbers.tex` against `results/accuracy.json`, `results/parts.json`, `results/critics.json`, `results/transformers_meta.json`.
- `scripts/common.py` (how VADER, TextBlob and the hedonometer are called), `scripts/exp_accuracy.py`, `scripts/exp_transformers.py`, `scripts/corpora.py`, `scripts/build_lexicon.py`, the operator lists and lexicon loader in `sgw/core.py`, and `sgw/lexicon.tsv`.
- `ANALYSIS_PLAN.md` (outcomes, comparators, deviations), `paper/refs.bib` (111 entries, 66 cited), `README.md`.
- The film-review corpus on disk (`data/scale/`), its README, and the SemEval test file.
- Four tiny read-only checks, each under two seconds on one core: (1) 240 SST sentences searched verbatim in the film-review corpus; (2) charged lexicon entries counted by source list; (3) charge carried by reviewer boilerplate in the film reviews; (4) 159 macro-free sentences of the `.tex` sources searched in the text of the PDF.
- Web: the VADER README, the model card of `cardiffnlp/twitter-roberta-base-sentiment-latest`, and Crossref / ACL Anthology / JMLR records for every work I propose below. I report only what those records show.

State of the files after the crash, from this lens: the result files I opened parse; every one of the 159 source sentences I searched for is in the PDF (six apparent misses were page or column breaks), so the PDF matches the current sources even though `body_b.tex` is dated 55 seconds after it. I ran no experiment, model, LaTeX or GPU job and changed no file other than this report.

## Concerns

### 1. Major. VADER is scored in the configuration its own documentation and this paper's own data show to be worst, on exactly the corpora that supply the margin over it

Claims. Abstract: "level with the best lexicon method (0.49)". Results: "The paired difference from VADER excludes zero in favour of the signature in 3 of six corpora and in favour of VADER in 0." Methods: the pre-registered gate is "a mean r over the primary corpora at least that of VADER", met "by a small margin".

Evidence.
- `scripts/common.py::vader_scores` passes the whole text to `polarity_scores` and takes `compound`, for 700-word film reviews and Yelp reviews as for single sentences. The compound score is a squashed sum; the Introduction itself reports that it is beyond +/-0.99 for 87% of whole chapters.
- The VADER README describes the tool as quantifying sentiment "in sentence-level text" and shows a demo for "decomposing paragraphs, articles/reports/publications, or novels into sentence-level analyses" (checked against the repository README).
- The paper's own files show what that costs. `results/parts.json`, 300 film reviews: VADER whole review r = 0.169, mean of sentence scores r = 0.231, signature density r = 0.232. Fairy tales: VADER whole 0.39, VADER sentence mean 0.63, calibrated signature 0.50 (these three are in the text). The sentence-mean VADER number for film reviews is computed and not reported.
- The three corpora where the signature beats VADER are Facebook (+0.038), film reviews (+0.045) and Yelp (+0.055). Two of the three are the long-document corpora where VADER was fed whole documents. On the three sentence-length corpora the differences are +0.021 (n.s.), -0.013 (n.s.) and -0.010 (n.s.).
- Rank correlation removes most of the saturation penalty. `accuracy.json`: on film reviews VADER has Pearson 0.248 but Spearman 0.328, above the signature's 0.298. The five-corpus mean Spearman is 0.508 for VADER and 0.506 for the signature. The plan names Spearman as the secondary outcome; it appears nowhere in the paper (also raised in `reviews/statistics.md`).
- If the gain seen on the 300 reviews (+0.06) carried to the test portion, the five-corpus Pearson margin would shrink from 0.016 to about 0.004. I did not compute this; it needs one run of VADER.
- The comparison is also not between independent instruments: 5,838 of the 8,408 charged entries of `sgw/lexicon.tsv` (69%) are VADER lexicon entries, 4,652 of them from VADER alone (my count). The signature is VADER's word list plus labMT with rule constants fitted to five rated corpora, set against VADER's word list with hand-set constants.

Required. Add "VADER, mean of sentence scores" as a row of Extended Data Table 1 for film reviews and Yelp (and for all corpora, for uniformity); report Spearman beside Pearson; restate the gate and the "3 of six" sentence against the stronger VADER row; say in the lexicon paragraph what share of charged entries comes from VADER.

### 2. Major. The transformer rows understate what the transformers do, so the gap in the abstract is a lower bound

Claims. Abstract: "below fine-tuned transformers (0.59 and 0.68)". Discussion: "Transformers agree with raters better (0.59 to 0.68 against 0.51)".

Evidence.
- `exp_transformers.py` truncates to the first 512 tokens; 97% of film reviews exceed that. The paper's own position results say the end of a review carries the verdict, so head truncation discards the most informative part. In `parts.json` the same models scored by sentence and averaged reach r = 0.61 (DistilBERT, against 0.53 whole) and 0.52 (Twitter-RoBERTa, against 0.23 whole) on 300 reviews. Extended Data Table 1 prints the truncated values (0.56 and 0.31) with no flag. Berardinelli's files also carry a cast-and-credits line (1,002 of 1,307 files; at the top in the files I opened), which spends part of the 512 tokens.
- The score is P(positive) - P(negative) from a binary and a three-class classifier, evaluated by Pearson r against a continuous rating. A binary classifier's probability is bimodal, and Pearson penalises that. `accuracy.json`: DistilBERT has mean Pearson 0.593 but mean Spearman 0.667 over the primary corpora (EmoBank 0.495 against 0.605). The gap to the signature is 0.09 in Pearson and 0.16 in Spearman.
- The model card of the RoBERTa model gives a preprocessing step that replaces user handles with `@user` and links with `http`. The script does not apply it; 425 of the 937 SemEval test tweets contain a handle.
- Neither model is trained for intensity regression and neither is trained on the target corpus, while the bag-of-words ridge is given the target's training portion (mean 0.61, above DistilBERT). No transformer is given the same advantage, and no zero-shot LLM rater is included, although the project already runs four LLMs for another experiment.
- "A transformer reads a few hundred words at a time" is supported by a citation to Vaswani et al. 2017, which sets no such limit, and is stated as a property of the family. It is a property of the two checkpoints chosen.

Required. Report sentence-averaged (or head-plus-tail) transformer scores for film reviews and Yelp in the table, or flag the truncated cells; apply the documented tweet preprocessing; give Spearman; describe the transformer rows as off-the-shelf classifiers, not as the accuracy ceiling; soften the window sentence and cite a source for the 512-token limit of these models. A fine-tuned-in-corpus transformer and an LLM rater are the natural upper comparators; if they are not run, say so.

### 3. Major. "Such scores do not compose" is not true of the lexicon family, and the part of the signature that is new adds nothing to agreement

Claims. Abstract: "Sentiment scores for text are usually bounded numbers ... Such scores do not compose: the score of a chapter cannot be computed from the scores of its sentences." Introduction: "Both families return a bounded number for each text ... the value for a whole cannot be computed from the values of its parts."

Evidence.
- Every count-based dictionary score is a ratio of two sums that add across spans. The hedonometer is defined on word-frequency vectors, and the paper concedes the point in passing ("the hedonometer's average happiness is a density taken over charged words only"). Positive and negative word counts with a word total (LIWC, cited) and the logit scale of Lowe et al. (cited) merge in the same trivial way. For these methods the whole follows from the parts once numerator and denominator are kept, which is how they are used in practice.
- A sentiment model that composes by an associative product already exists: Yessenalina and Cardie, "Compositional Matrix-Space Models for Sentiment Analysis", EMNLP 2011, pp. 172-182 (record confirmed). It is the sentiment-specific instance of the weighted-automaton fact the paper cites from Schutzenberger and from Berstel and Reutenauer, and it is not cited. The compositional line that the treebank was built for (Socher et al. 2013, used here only as a data source) is likewise not discussed as prior work on composing sentiment from parts.
- What the signature adds beyond (sum of positive charge, sum of negative charge, word count) is the position moments and the operator channels. The paper's ablation: dropping the position moments costs 0.00 in mean r, merging the two charge channels costs 0.00. The operator channels (0.03) are VADER's and SO-CAL's rule classes with fitted weights.
- The Discussion is candid ("No component of this is new"). The title, abstract and Introduction are not written to match it.

Required. Rewrite the first two sentences of the abstract and the second paragraph of the Introduction to say that final bounded scores (VADER compound, classifier probabilities) do not compose, that count-based scores compose trivially through their sums, and that the contribution is the position-moment extension, the exact-merge implementation and the tests of position. Cite Yessenalina and Cardie 2011 and Reagan et al. 2017 (below).

### 4. Major. The film-review corpus is scored with reviewer boilerplate in place; the critic comparison is of the same size as that boilerplate

Claims. "Pooled, Berardinelli's and Renshaw's reviews have the same density (difference -0.1 per thousand words). Within the same rating Berardinelli's prose is the more negative by -1.2 [-2.5, +0.0]". Table: differences of 7 to 16 per thousand words between the other critics. Film reviews also enter Extended Data Table 2 (position), the position kernel, the dilution test and every leave-one-corpus-out fit.

Evidence (files under `data/scale/scale_whole_review/`; no script strips anything, `grep` for footer terms in `scripts/` finds nothing).
- 486 of 902 Renshaw reviews end "Visit Scott Renshaw's MoviePage ... Subscribe to receive new reviews directly by email! See the MoviePage for details, or reply to this message ...". In the shipped lexicon that footer carries +2.2 hd (visit .385, receive .375, new .455, email .470, see .265, message .270). At a median of 740 words that is about +3 per thousand words in those reviews and about +1.6 averaged over all of his reviews: larger than the -1.2 "within the same rating" difference the text interprets.
- 918 of 1,770 Rhodes reviews end with a block of median 41 words ("X runs 1:42. It is rated PG-13 for ... The film would be fine for kids ... My son Jeffrey, age 9, thought the movie was great and gave it ****"). It carries a mean of +3.8 hd net, about +6 per thousand words in those reviews. Rhodes is the most positive critic in every pair (-9.0, -8.9, -16.5).
- 1,002 of 1,307 Berardinelli files contain a cast and credits line (at the top in the files I opened); some end with a copyright line and an epigraph.
- The explicit rating of the critic was removed by the corpus authors ("On the Renshaw scale of 0 ."), but the star ratings Rhodes reports for his son remain ("gave it ****"), which a bag-of-words model can read. This favours the trained comparators, not the signature.
- These blocks sit at the start and the end of the text, the two places the position analyses measure.

Required. Strip the credits blocks, footers, copyright lines and the runtime/MPAA paragraph before scoring, rerun the film-review rows, the critic decomposition and the film-review position rows, or drop the critic illustration. State in Methods what was removed.

### 5. Major. Known limits of lexicon methods are only partly acknowledged; social bias in the shipped charges is not mentioned at all

Claims. The Discussion lists: words not events, irony, sarcasm, syntax beyond four operators, English only, ratings from the early 2010s, genre of the fitting corpora. The Introduction offers the measure for "clinical notes and parliamentary speech".

Evidence.
- Shipped charges (`sgw/lexicon.tsv`, thousandths; charged if at least 250 in size): deaf -582, disabled -545, blind -527, fat -440, poor -613, old -255, iraq -275; rich +717, women +530, woman +460, young +455, america +440, white +320. black is -30 (uncharged), man +225 (uncharged). A sentence that mentions a deaf or disabled person, an old person or Iraq is scored as carrying negative feeling; one that mentions white people carries positive charge and one that mentions black people none.
- `sgw/core.py` line 56 says of the refined lexicon "not shipped (it absorbs social bias)". The paper explains the same decision only as "Charges learned from ratings carry the genre of the texts they were learned from". The stated reason in the code is not in the paper, and the human norms have the same problem in milder form.
- Word-sense and part-of-speech ambiguity is not listed: like +476, kind +598, pretty +581, well +356 are charged in every use ("like a", "kind of", "pretty bad"). VADER special-cases some of these; the signature does not.
- Domain dependence of polarity, coverage (77% of running words are uncharged), the fixed three-word negation window, and the discarding of capitalisation and repeated punctuation (which VADER uses) are not listed either.
- Relevant literature, all confirmed to exist and none cited: Diaz et al., "Addressing Age-Related Bias in Sentiment Analysis", CHI 2018; Kiritchenko and Mohammad, "Examining Gender and Race Bias in Two Hundred Sentiment Analysis Systems", *SEM 2018; Hamilton et al., "Inducing Domain-Specific Sentiment Lexicons from Unlabeled Corpora", EMNLP 2016.

Required. Add a limits paragraph on social bias with the examples above, on sense ambiguity and on domain shift; give the true reason the refined lexicon is withheld; add a caution to the README and the tone tool.

### 6. Minor. "The best lexicon method" is the best of three; baselines, citations and one Methods section are missing

- Three lexicon methods were run. SentiBench (Ribeiro et al. 2016, cited) compares two dozen and finds no single winner. "Level with the best lexicon method" should read "level with VADER, the best of the three lexicon methods tested". On the two document-level corpora the best of the three is TextBlob (film reviews 0.46 against 0.29; Yelp 0.60 against 0.61), which the text does not say.
- Baselines the authors could add from files already on disk: mean NRC-VAD valence and mean Warriner valence (the Methods report using both lists on development data); sentence-averaged VADER (concern 1). SO-CAL and SentiStrength are cited as prior work and not run; say why.
- The hedonometer is applied to single sentences and tweets. Its authors designed it for large pooled texts; Reagan et al., "Sentiment analysis methods for understanding large-scale texts: a case for using continuum-scored words and word shift graphs", EPJ Data Science 6, 2017 (record confirmed) is the benchmark of dictionary methods on large texts and should be cited both for that and as the nearest prior comparison of the same instruments. I confirmed the record, not the passage.
- TextBlob has no citation and no version. Its polarity comes from the Pattern lexicon: De Smedt and Daelemans, "Pattern for Python", JMLR 13, 2012, pp. 2063-2067 (confirmed). `vaderSentiment` has no version either.
- The third confirmatory test uses 22,001 IMDB reviews. The text says "(Methods)", but `methods.tex` has no description of any of the three confirmatory tests (no mention of IMDB, of the 100-word filter in the plan, or of the held-out trees), and the dataset (Maas et al., "Learning Word Vectors for Sentiment Analysis", ACL 2011, pp. 142-150, confirmed) is not cited.
- The RoBERTa model was fine-tuned on TweetEval sentiment (per its model card); Barbieri et al., "TweetEval", Findings of EMNLP 2020 (confirmed) should be cited so readers can judge its closeness to the SemEval tweets. DistilBERT was fine-tuned on SST-2, whose training data are the SST training phrases, so its SST column is in-domain; the label "(SST-2)" hints at this and the text should say it.
- Leave-one-corpus-out is not leave-one-domain-out. Three of the six corpora are reviews and two of them are film reviews from the same source paper. This matters for the refined charges and the pooled bag-of-words (gains "large on review text"). I found no verbatim overlap: 0 of 240 sampled SST sentences occur in the film-review files.
- `refs.bib` holds relevant entries that are never cited: Polanyi and Zaenen 2006 and Kennedy and Inkpen 2006 (valence shifters), Zhu et al. 2014 (negation shifts rather than flips, measured on the treebank, which is the "not bad is neutral" result), Wachsmuth et al. 2015 (sentiment flow in reviews), Gallagher et al. 2021 (word shift graphs). Cite them where the corresponding claims are made.
- "Word norms in other languages" is listed as future work. Dodds et al., "Human language reveals a universal positivity bias", PNAS 112, 2015 (confirmed) is, as I recall, a ten-language extension of labMT; if so the norms exist and the sentence should say they were not used. That paper's positivity bias is also the reason narrative densities need a baseline, which the paper notes without the citation.
- "No human ratings of chapters or books were available": Ong et al., "Modeling Emotion in Complex Stories: The Stanford Emotional Narratives Dataset", IEEE Transactions on Affective Computing 12, 2021 (confirmed) has continuous valence ratings over the course of narratives. It is spoken narrative, not books, but it would test recency and arcs against human ratings directly.
- The remark that the data "give no support to the rule that the clause after *but* counts for more" sits oddly beside the paper's own treebank result that the later part of a phrase gets the larger weight. The contrast list also includes *yet* (often temporal). A direct test on treebank phrases that contain *but* would settle it.
- For a zero-shot LLM comparator: Rathje et al., "GPT is an effective tool for multilingual psychological text analysis", PNAS 121, 2024 (confirmed).

## What I could not check

- I ran no model, so the effect of sentence-averaging VADER and the transformers on the full test portions, of tweet preprocessing, and of stripping boilerplate is estimated from stored 300-review results and word counts, not measured.
- My boilerplate figures use a plain regular-expression tokeniser and the shipped lexicon without operators. They reproduce the paper's critic differences to within about half a unit per thousand words, but they are approximations.
- I did not open the Yelp, EmoBank, Facebook or IMDB files, the novels, HC3 or PERSUADE, and did not review the human-versus-machine experiment beyond its citations.
- I did not check the 53 cited references I do not name above against primary records. One 2026 citation was checked (Sui, AI and Society: the record gives online publication in 2025 and a February 2026 issue). Three 2026 arXiv entries in `refs.bib` are uncited and unverified.
- The best V-reg score at SemEval-2018 and the wording of the hedonometer's design range are from memory and are not asserted above as fact.
- Whether the public repository named in the paper matches this working copy.
