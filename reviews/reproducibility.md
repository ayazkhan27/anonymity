# Referee report: reproducibility and the repository

Manuscript: "A mergeable measure of sentiment for text of any length" (`paper/sgw.pdf`, 14 pages).

State reviewed: commit `0fc5a78` plus the uncommitted changes present between 22:16 and 22:35 on 13 November 2026 (`README.md`, `paper/body_b.tex`, `paper/numbers.tex`, `paper/sgw.pdf`, `scripts/make_numbers.py` modified; `machine_text/side_by_side.md` untracked, created 22:27). The working tree was being edited while I read it. Where that matters I say so.

## What I examined

Read in full: `README.md`, `ANALYSIS_PLAN.md`, every file in `sgw/`, `scripts/`, `tests/` and `integrations/`, all `paper/*.tex`, `pyproject.toml`, `LICENSE`, `.gitignore`. Inspected: the seven local commits and their diffs, the reflog, file modification times in `data/`, `results/`, `machine_text/` and `paper/figures/`, the job files, the logs kept under `data/`, and `legacy/2024/`.

Checks I ran. Each took under ten seconds on one core, from a scratch directory outside the repository, with bytecode writing disabled. I wrote nothing in the repository except this file.

| # | Check | Result |
|---|---|---|
| 1 | Regenerated `numbers.tex`, the three tables, `excerpt.tex` and the README results block from the current `results/` with a copy of `make_numbers.py` writing to scratch | byte-identical to the working tree (738 macros) |
| 2 | Same, with `data/` absent | six macros are not written (concern 2e) |
| 3 | `tests/test_core.py` | 5 of 5 pass, 0.8 s |
| 4 | Recomputed signatures for EmoBank, SemEval, Facebook and SST (all 12 portions) and compared with `data/cache/sig_*.npy` | identical |
| 5 | Recomputed hedonometer, VADER and TextBlob scores on the SemEval test portion | identical to the caches |
| 6 | Recomputed leave-one-corpus-out r for the calibrated score and the level on four corpora from the constants stored in `results/accuracy.json` | equal to six decimals |
| 7 | `sgw/constants.json` against `accuracy.json["shipped"]` | equal |
| 8 | Rebuilt the open lexicon in memory from `data/lex` with the functions of `build_lexicon.py` | byte-identical to `sgw/lexicon.tsv` |
| 9 | `sgw/emoji.tsv` against VADER's `emoji_utf8_lexicon.txt` | byte-identical (same MD5) |
| 10 | Recomputed the "norm charges" development correlations printed in `data/refine.log` for five folds and the pooled fit | all match to four decimals |
| 11 | Re-derived `machine_text/sample.csv` from `data/persuade/essays.parquet` | per-prompt `sample(20, random_state=0)` over Independent-task essays of 150 to 800 words gives the same 160 ids in the same order |
| 12 | Recomputed the dial result from shipped files only | mean target-achieved r 0.643 (dial) and 0.084 (control) over 48 pairs; judge shares 62.5% and 10.4%; all equal to `results/machine.json` |
| 13 | Re-parsed the treebank | 154,457 training nodes, 11,474 held-out nodes for R1, 1,690 and 2,082 for R2; all equal to `results/position.json` |
| 14 | Command line and MCP entry points on a shipped essay | work as the README says (two small defects, concern 8) |
| 15 | Installed package versions against the Software paragraph | match (Python 3.10.12, NumPy 2.0.2, SciPy 1.15.3, scikit-learn 1.5.2, PyTorch 2.11.0, Transformers 4.51.3, Matplotlib 3.9.2) |
| 16 | Licences against primary records | VADER: MIT, "Copyright (c) 2016 C.J. Hutto". Dodds et al. 2011: Creative Commons Attribution License; labMT 1.0, 10,222 words, 50 evaluations per word. PERSUADE 2.0 project page: CC BY 4.0, and its training-set link is the one in `fetch_data.py`. HC3 dataset card: `cc-by-sa-4.0`. EmoBank README: CC-BY-SA 4.0. Yelp review full card: licence "other", pointing to the Yelp dataset agreement. arXiv:2609.00065 exists with the title and first author the README gives |

So the arithmetic from result files to the manuscript is sound, and the parts of the scorer I could recompute agree with the stored results. The concerns below are about what a reader cannot rerun, cannot audit, or should not be handed.

## Concerns

### 1. The machine-writing experiments cannot be audited, and the stated conditions of the writing agents are contradicted (major)

The paper says (Methods, `\machineMethods` in `paper/textmacros.tex`): "Each agent received only its task; none was told of the study or its hypotheses." The body says: "The writing agents were told nothing about the study."

Evidence against, and gaps:

- `ANALYSIS_PLAN.md`, deviations in the addenda, item 2: "one agent declined because the task did not match the request relayed to it". An essay agent therefore saw a relayed request besides its task.
- I was launched as a subagent from this same working directory. Besides my task, my context held (i) the user's triggering request relayed verbatim by the harness, with the rule that it overrides the task where the two conflict, and that request states the dial hypothesis; (ii) a block of style instructions injected by a `SubagentStart` hook of an installed plugin; (iii) an index of the user's saved notes. I cannot tell whether the writers and judges were launched by the same path. No transcript is shipped that would settle it.
- Every job file gives the agent an `output_path` that names its arm: `.../revisions/control/`, `.../revisions/report/`, `.../dial/r1/dial/`, `.../dial/r1/control/` (`scripts/exp_machine.py` lines 183, 206, 313, 337). The agents in the control arms were told they were controls.

| Needed to reproduce or audit | In the repository |
|---|---|
| Code that builds `data/persuade/essays.parquet` from the download | No. No commit contains it |
| Code that drew `machine_text/sample.csv` | No. I could re-derive the sample (check 11) |
| Code that wrote the 320 essay jobs in `machine_text/jobs/` | No |
| Dispatcher: which model ran which job, with which settings | No. The model appears only as a filename suffix |
| Model identifiers, harness version, effort setting | Named in the paper only ("Fable 5.2, Opus 5.5, Sonnet 5.5 and Haiku 5.5 ... at low reasoning effort") |
| Agent transcripts and refusal messages | No. The refusal reasons the paper quotes cannot be checked |
| Judge instruction and raw judge output | No. `judge_jobs/*.json` hold four data fields; `judge_results.json` is a list of 48 answer pairs |
| Proof that a skill-arm agent loaded the named skill | No |
| Source and version of the two skills | The names (`humanizer:humanizer`, `sepia:sepia-recreate`) are in the plan and the code. The paper says only "two published prose skills" |

Timing, from modification times: 318 essays were written in 4 min 29 s, and all 120 skill rewrites in 2 min 30 s. That is possible with many agents in parallel, but it leaves little room for a skill workflow, and nothing shows the skills were run.

Requested: ship a manifest with one row per job (job id, model identifier, harness and version, effort setting, start and end time, status, refusal text) and the transcripts or at least the full prompt each agent saw. Ship the judge instruction and raw answers. Remove the arm from the output path in any rerun. Name and version the skills in Methods. Replace "received only its task" with what the transcripts show. Add the sampling, preprocessing and job-writing code.

### 2. The README's "Reproduce" block does not run from a clean clone and skips headline analyses (major)

a. `scripts/corpora.py` line 22 reads `data/emobank/meta.tsv`. `fetch_data.py` (`emobank()`, lines 48-49) downloads only `emobank.csv`, in every commit. The first experiment script stops with a missing file.

b. `exp_machine.py` (lines 92, 144, 162, 228, 293, 346, 374) reads `data/persuade/essays.parquet`. `fetch_data.py` writes `persuade_train.csv`. Nothing converts one to the other, so `essays`, `reference`, `revise` and `dial` all fail.

c. README line 102: `python3 scripts/exp_position.py        # where, kernel, emobank, dilute, tree, confirm`. Without arguments the script runs `("where", "kernel", "emobank", "dilute", "tree")` (line 259). The three confirmatory tests of recency are not rerun. README line 103 lists `parts` for `exp_books.py`; the default is `("novels", "tales", "critics")` (line 260). Both scripts merge new keys into the existing JSON, so the shipped `confirm` and `parts.json` values stay in place and the reader does not see that they were never recomputed.

d. `sgw/emoji.tsv` has no generating step. `fetch_data.py` copies the VADER file to `data/lex/`; nothing copies it into the package.

e. `make_numbers.py` reads `data/cache/sigall_*.npy` (lines 53-56) and `data/gutenberg/books/2701.txt` (lines 232-242), and `make_figures.py` reads the same book for Fig. 5a,b (lines 108-109). Without `data/`, `\chargedShare`, `\mobyWords`, `\mobyHedons`, `\mobyDolors`, `\mobyDensity` and `\mobyChapters` are not defined, and the build hits undefined control sequences. The docstring promises red "??"; line 346 provides that only for `\num`. The statements "Result files behind every number and figure" (README) and "Every number in the text is generated from the result files" (Code availability) are therefore not exact. Some results are also typed by hand in `methods.tex`: "from 0.521 to 0.534", the corpus sizes, "40 emoticons", "$10^{-8}$". I checked these four against `data/dev_explore.log`, the cache shapes and the code, and they are right, but the log is under the ignored `data/`.

f. Nothing is pinned. Packages: one unpinned `pip install` line. Models: `from_pretrained` without `revision`, one of them named `...-latest`. Data: Hugging Face `refs/convert/parquet` and `main`, GitHub `master`, a Google Drive id, no checksums; `get()` accepts any non-empty response. The Gutenberg sample is a seeded shuffle of the live weekly catalogue, so a later run draws different books; the 640 ids are in the ignored `data/gutenberg/sample.csv`, and `exp_position.py` and `exp_machine.py reference` depend on the first 150 of them in filename order.

g. `scripts/dev_explore.py` is not in the list although Methods report its numbers. The job-writing subcommands (`jobs`, `skilljobs`, `dialjobs`, `dialjobs2`, `judgejobs`) are not mentioned.

h. Smaller: `tarfile.extractall(filter="data")` needs a patched Python; `exp_books.novels()` hard-codes 12 processes; TeX requirements are not listed; `make_numbers.py` rewrites `README.md`.

Requested: fetch `meta.tsv`; add the PERSUADE preprocessing; make each script run all its parts by default or list the arguments; write a data manifest with SHA-256 per file and ship the Gutenberg ids; pin model revisions and add a lock file; write the six data-dependent values and the development-set numbers to `results/`; then run the block once on a clean machine.

### 3. The pre-specification cannot be verified by anyone else, and some records post-date what they describe (major)

The abstract says the hypotheses "were tested as pre-registered". Methods cite commits `7524c58`, `4b10b12`, `77d4fa2`, `090411d`, `de181ec`.

- All seven commits are local and unsigned. `main` is 7 ahead of `origin/main`, whose tip is `3955f03` (2025). The URL in the paper does not hold the plan, the code or the cited hashes.
- The whole study, from clone to last commit, took about two and a half hours on one evening.

| Time, 13 Nov | Event | Source |
|---|---|---|
| 19:45:42 | repository cloned | reflog |
| 19:48:07 | plan committed | reflog |
| 19:49:31 | first data file | mtime |
| 20:00:03 to 20:03:46 | test portions first scored | `data/cache` mtimes |
| 20:32:44, 20:51:14 | `exp_accuracy.py` and `common.py`, then `sgw/core.py`, last modified | mtime |
| 21:05:57 to 21:13:16 | signatures recomputed, `accuracy.json` written | mtime |
| 21:28:24 | first commit of any code or result, with deviations 1 to 8 and addendum 1 | reflog |
| 21:30:05, then 21:30:47 to 21:35:16 | essay jobs written, essays written | mtime |
| 21:50:26 | addendum 2 committed, in the same commit as the essays | git |
| 21:53:48 | held-out trees and IMDB downloaded | mtime |
| 21:58:48, 21:58:49 | addendum 3 committed, skill jobs written | reflog, mtime |
| 22:01:09, 22:02:29 | addendum 4 committed, dial jobs written | reflog, mtime |

The local order agrees with the claims. Three points do not:

- `ANALYSIS_PLAN.md` line 102, "Deviation from the addendum (13 November 2026, before any essay was generated)", first appears in commit `77d4fa2`, which also adds the essays. The commit cannot show that the change of design (eight prompts, 20 essays each, four models) came first.
- Run 1, the analysis as planned, is not preserved. No commit and no log holds its output. The first committed results already include the widened grid, the rewritten tokeniser and the bug fix. The accuracy gate is passed by 0.017 (0.506 against 0.489), and a reader cannot see what the plan as written gave.
- "Untouched data": 1,527 of the 11,474 held-out nodes used for R1 (13.3%) are sentence roots, whose ratings are the SST development and test labels already used for accuracy and, under deviation 8, for the kernel and dilution fits. Repeated phrase triples between training and held-out trees are rare (0.5% for R1, 1.6% for R2).

Concern 4 makes this worse: removing the leaked credentials from history rewrites every hash that Methods cite.

Requested: clean the history first, then push, tag and deposit the repository with a third-party timestamp, and cite those identifiers. Say "committed before", not "pre-registered", unless the plan is registered. Report the run-1 numbers.

### 4. Files that must not be shipped (major)

- `legacy/2024/twittertestwrite.py` lines 5-9 and `legacy/2024/testweet.py` line 4 hold a Twitter consumer key and secret, an access token and secret, and a bearer token in clear text. Both files are at the tip of `origin/main` and have been in pushed history since 29 April 2024. I did not test the keys and have not copied them here.
- `legacy/2024/pythonw.exe`, a Windows executable.
- `legacy/2024/posts_data.xlsx` (67,972 rows; columns Keyword, Page, Post Number, Title, Post Content and date fields) and `posts_data2.xlsx` (64 sheets): scraped post text with no source, licence or consent record.
- `legacy/2024/sentiment_model_2.RData` and `.rds`: two 6 MB copies of one model object that includes `train_data`.
- 31 hard-coded Windows user paths in the legacy scripts, and 789 job files under `machine_text/` whose `output_path` is the absolute local path of the working copy, account name included.

Requested: revoke the keys now. Remove `legacy/` or reduce it to scripts with secrets and data stripped, and purge the history. Write job files with relative paths.

### 5. Licences, attribution and the Data-availability statement (major)

Methods line 77: "none is redistributed except the open lexicon derived from labMT (CC BY) and the VADER word list (MIT)." README line 138: corpora "are not redistributed".

- `sgw/emoji.tsv` is a verbatim copy of a VADER file, and 4,652 of the 8,408 charged entries of `sgw/lexicon.tsv` come from VADER alone. The MIT licence requires its copyright and permission notice in "all copies or substantial portions". The repository has no such notice and no `NOTICE` file. `pyproject.toml` declares the whole package, data files included, under the Apache `LICENSE`.
- labMT is credited in the README but not inside the installed package, and no change statement accompanies the rescaled values. The file is fetched from a third-party package repository whose lexicon directory shows no licence note, not from the article's Data Set S1.
- `sgw/lexicon_refined.tsv` (21,542 entries, 6,711 of them words absent from the open lexicon) is fitted on six corpora, among them EmoBank (CC-BY-SA 4.0) and Yelp (its own agreement). Neither statement mentions it. It has no neutral band and carries fitted charges on group and person terms: *muslim* -0.374 (norm -0.090), *islam* -0.386, *gay* -0.394 (norm -0.140), *obama* -0.529 (norm +0.235), *women* -0.083 (norm +0.530). `load_lexicon("refined")` serves these to any user.
- `sgw/reference.json` is computed from PERSUADE, HC3, Yelp and Gutenberg; the README's licence section names only PERSUADE.
- `machine_text/jobs/` and `judge_jobs/` carry the PERSUADE assignment texts, and `sample.csv` carries essay ids and grades. CC BY 4.0 allows this, but it is redistribution. The new `machine_text/side_by_side.md` reproduces four complete student essays (1,742 words) with no essay id, citation or link, and no script generates it. Its first three picks follow the stated rule (I recomputed them: essays 129, 105 and 14); the fourth is one of five eligible pairs and its rule is not given, so "Nothing was picked by eye" is not shown for it.
- The essays come from the PERSUADE training set (15,594 essays), not the whole corpus; say so.
- No licence is stated for the machine-written texts.

Requested: add third-party notices (MIT text for VADER, CC BY attribution and change note for labMT) and scope the Apache licence to the code; rewrite both availability statements to list what is shipped and from what it derives; either drop the refined lexicon from the package or document it and filter named entities and group terms; give ids and a citation wherever student text is quoted.

### 6. The list of deviations is incomplete (major)

Against the scripts and results, the following are not listed:

1. Lexicon scaling. Plan line 12: "put on one scale by linear regression on shared words". `build_lexicon.py` line 51 uses the ratio of root-mean-square values, slope 1.0588; regression through the origin gives 1.002. Methods describe the code, not the plan.
2. Bootstrap size. Plan line 37: "2,000 resamples". The code uses 500 for Spearman intervals, 1,000 for `where` and `kernel`, 500 for the EmoBank slope, 500 for the dilution partial correlation and 100 for Yelp, 500 for the exploratory treebank analysis, 1,000 for the confirmatory R3, 1,000 and 500 for HC3, 1,000 for AUC intervals, 500 for the constants.
3. Dial attrition. Item 3 of the addenda deviations says "11 of 60 essays were left without a second-round text ... (57 of 60 and 48 of 60 remain)". 60 minus 11 is 49. The twelfth loss is a Sonnet essay with no control-arm text in round 1 (`dial/r1/control` has 59 files, 14 for Sonnet). Neither the plan nor the paper accounts for it. Only 4 of 15 Haiku essays remain in the comparison.
4. H3. The plan asks, for VADER and the transformers, "how often the sign differs" between the mean of sentence scores and the document score. That is reported for the transformers on 300 reviews only. The plan's "share of novels" where pooled and averaged levels disagree in sign is not reported either.
5. Run 1 and the timing of the addendum-1 change (concern 3).
6. The HD2 classifier is trained on essays that include the 12 dial-sample essays that dropped out; the addendum says "none from this sample".

### 7. Caches and partial reruns can silently mix code versions (minor)

`cached()` in `scripts/common.py` keys on a name only. `exp_refine.py` keeps "finished folds", and `position.json` and `machine.json` are updated key by key. The EmoBank fold of the refined-charges analysis was produced by a process that finished before `exp_refine.py` was last edited (the script and `refine_emobank.json` share the time 20:41:54), and the other folds ran while `sgw/core.py` was edited at 20:51:14. Checks 4, 5 and 10 found no stale value, so this is a hazard, not an error found. Key caches on a hash of the code and lexicon, or tell the reader to clear `data/cache` first.

### 8. Smaller inconsistencies between README, paper and code (minor)

- Abstract: "Against human ratings in six corpora the calibrated score reaches a mean correlation of 0.51". 0.51 and 0.49 are the five-corpus means, as the README and the body say. Over six corpora the values are 0.52 and 0.50.
- README: "21 minutes on twelve". The host has six cores and twelve threads; twelve processes ran 4.8 times faster than one.
- `scripts/exp_gpu.py` documents and asserts an AMD ROCm device; `results/speed_gpu.json` is from an NVIDIA RTX 3090 with PyTorch 2.3.1. The Software paragraph gives PyTorch 2.11 only, and `speed.json` records no library versions for the rented host.
- Methods give 8,408 charged entries; 8,370 carry charge once operator words are zeroed.
- Title page: "Code and data". No corpus is at the URL, and at present neither is the code.
- Methods: "reviewed by AI reviewer agents whose reports are in the repository". `reviews/` was empty and untracked when I started.
- `results/literature_machine.json` is not produced or read by any script; 10 of its 21 entries are not cited; its "verified": true flags have no stated procedure. It is not a result file.
- `integrations/claude-skill/sgw-tone/SKILL.md` says the report revision "ended closer to the centre of human essays" and then that the test was undecided. Student essays themselves sit at 1.16 from that centre and the drafts at 0.66, so a smaller distance is not movement toward how students write.
- `python3 -m sgw tone x --genre poem` ends in a `KeyError` traceback; a positional argument equal to the genre name is dropped.
- The tests cover the exactness of the core only. Nothing tests `tone.py`, `refine.py`, the command line, non-ASCII input or batches over 20,000 texts.

## What I could not check

- I ran no experiment script, no model inference and no figure script. Apart from the pieces in the table above, I did not recompute `results/*.json` from raw data: transformer scores, the refinement fits, the novels, the fairy tales, the critics, the uncertainty analysis and the throughput figures are taken as stored.
- The film-review and Yelp signature caches were checked for shape only. Baseline caches were recomputed for SemEval only.
- Run 1 and the claims of deviations 1 to 3: no record exists.
- What the writing and judging agents received, which model produced which file, why requests were declined, and whether the skills were loaded.
- Licence terms beyond the primary records listed in check 16. I did not read the Yelp agreement or the terms of SemEval-2018, the Facebook posts, SST, the Pang and Lee data, NRC-VAD, the Warriner norms or the two skills.
- Whether the leaked credentials still work.
- The remote repository. I made no request to it; statements about `origin/main` rest on the local remote-tracking refs from the clone at 19:45.
- The claim about the February 2026 branch.
- The PDF against its sources beyond the build log, which shows no undefined reference. At 22:20 the PDF was older than `body_b.tex` by one wording edit.
