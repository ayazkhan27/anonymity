# Third-party material

The code in this repository is under the licence in `LICENSE`. The data files below derive from other people's work and carry their own terms.

## Word charges: `sgw/lexicon.tsv`

Built by `scripts/build_lexicon.py` from two sources, rescaled to a common scale from -1 to 1 and averaged where both rate a word. The values are therefore changed from both originals.

- labMT 1.0. Dodds, P. S., Harris, K. D., Kloumann, I. M., Bliss, C. A. and Danforth, C. M. (2011). Temporal patterns of happiness and information in a global social network: hedonometrics and Twitter. PLoS ONE 6(12): e26752. The article is published under the Creative Commons Attribution licence; its word list (Data Set S1) asks users to cite the article. The file used here was fetched from the copy distributed with the `shifterator` package.
- The VADER lexicon. Hutto, C. J. and Gilbert, E. (2014). VADER: a parsimonious rule-based model for sentiment analysis of social media text. ICWSM. MIT licence, reproduced below.

## Emoji names: `sgw/emoji.tsv`

A copy of `emoji_utf8_lexicon.txt` from the VADER package (version 3.3.2). MIT licence, reproduced below.

## Human reference ranges: `sgw/reference.json`

Summary statistics (means, standard deviations and percentiles of ten measures) computed by `scripts/exp_machine.py reference` from: student essays in the PERSUADE 2.0 training set (CC BY 4.0); human answers in HC3; Yelp reviews; and chapters of Project Gutenberg novels. No text from these sources is included.

## Student essays and assignment texts in `machine_text/`

The job files quote the assignment texts of PERSUADE 2.0, `sample.csv` lists essay identifiers and grade levels, and `side_by_side.html` reproduces four complete student essays, each with its identifier. Source: Crossley, S. A., Baffour, P., Tian, Y., Franklin, A., Benner, M. and Boser, U. (2024). A large-scale corpus for assessing written argumentation: PERSUADE 2.0. Assessing Writing 61. Licence: CC BY 4.0. https://github.com/scrosseye/persuade_corpus_2.0

The essays, revisions and judgements written by language models in `machine_text/` were generated for this study and are released under the same terms as the code.

## The VADER licence

The MIT License (MIT)

Copyright (c) 2016 C.J. Hutto

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.