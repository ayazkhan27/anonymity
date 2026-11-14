# Referee report: human and machine writing (HC3, essays, tone check, skill arms, dial, judge)

Reviewed 13 November 2026, about 23:10 to 23:30 local time. The manuscript source, README, SKILL.md and ANALYSIS_PLAN.md were being edited while I read them. This report describes `paper/body_c.tex` (23:17:52), `paper/textmacros.tex` (23:20:30), `paper/abstract.tex` (23:15:59), `paper/numbers.tex` (23:22:19), `results/machine.json` (23:14:54) and the plan as of 23:23:40. Several problems that earlier reports raised are already handled in that version (jackknife intervals for the essay contrasts, the by-source HC3 table, the admission that distance to the centre measures typicality, the disclosure that agents were not blind, the note that the eight-assignment deviation was recorded after generation). I do not repeat them. What follows is what is still wrong or unsupported.

## What I examined

- Text: `paper/abstract.tex`, `paper/body_c.tex`, the `\machineMethods`, `\machineCaption` and `\dialParagraphs` macros in `paper/textmacros.tex`, `paper/extended.tex`, `paper/excerpt.tex`, `paper/table_sources.tex`, the Methods paragraphs on pre-specification, AI tools and data availability, and `paper/figures/fig7_machine.png`.
- Numbers: every `hc*`, `es*`, `ej*`, `rv*`, `dRv*`, `dl*`, `dDl*` macro in `paper/numbers.tex` against `results/machine.json`, through the mapping in `scripts/make_numbers.py` (lines 350 to 461). All macros I traced agree with the JSON.
- Code: `scripts/exp_machine.py` in full, `sgw/tone.py` in full, the tokeniser and operator lists in `sgw/core.py`, `boot`, `resamples` and `jack` in `scripts/common.py`.
- Plan: `ANALYSIS_PLAN.md` addenda 1, 3 and 4 and both deviation lists; `git log` times against file times in `machine_text/`.
- Materials: all five workflow scripts in `machine_text/workflows/`; `sample.csv`, `revision_sample.csv`, `dial_sample.csv`, `judge_key.csv`, `judge_results.json`; job files `jobs/000`, `jobs/001`, `revision_jobs/001`, `skill_jobs/000`, `dial_jobs/r1/000`, `dial_jobs/r2/002`, `judge_jobs/000`, `001`, `020`; essays `essays/A/000_fable`, `essays/B/000_fable`, the excerpt essay (k = 129), and the two judged pairs in jobs 001 and 020; a pattern scan of all 772 essay and revision files for refusal text, commentary and short files (none found; every hit was ordinary essay prose).
- `README.md`, `integrations/claude-skill/sgw-tone/SKILL.md`, `integrations/mcp_server.py`, `THIRD_PARTY_NOTICES.md`.
- Six small read-only checks, each under one second on one core: judge verdicts by position, model and assignment; file inventory and word-count match; which intensifier words carry the "intensified" charge; Pearson against Spearman for the dial correlations (this reproduced all ten reported dial correlations); ending lift in flagged and unflagged tone-check drafts; and a count of spelling and apostrophe errors in the dial and control revisions.

## Concerns

### 1. The abstract's last claim rests on a test that returned "undecided" and could not have returned anything else (major)

Abstract: "A model can be steered to an assigned profile of such measures, and that did not bring its essays closer to the students' than a plain revision did." Body heading sentence: "Following the dial did not make the essays more like student writing than a plain revision did."

- HD2 is `undecided`: dial minus control +0.06, jackknife interval [-0.11, +0.23] (`dial.score_dial_minus_control`).
- The control arm is already at the students' level on this outcome: mean machine score 0.444 for two plain revisions against 0.430 for the matched student essays, AUC against matched students 0.525 (`dial.machine_score`, `dial.auc_vs_matched_students`). HD2 asks whether the dial scores lower than the control. With the control at the floor, no dial result could have passed. The test is uninformative by construction, and that is not said.
- The same kind of classifier gives a different picture in the tone check: control revision 0.55 against students 0.38, described in the text as "none came close". In the dial experiment two plain rounds reach 0.44 against 0.43. The paper does not reconcile these. Candidates are one round against two, a different 57 or 48 essays, or the instability of a ten-feature logistic regression trained once on 98 or about 100 pairs. No machine score in either experiment has an interval, and `control_r1` is profiled but never scored by the classifier, so the reader cannot tell which.
- "Steered to an assigned profile" overstates HD1. The mean squared gap to the target falls from 1.67 to 0.79, not to zero; the correlation is 0.24 for clustering and 0.43 for intensified; in the paper's own excerpt the ending lift moves away from its target and intensified goes to 0.0 against a target of 2.0. The result covers 48 essays from three models.

Fix: say "steered toward", and replace the second half with what was measured: no difference between dial and plain revision was detected, the plain revision was already indistinguishable from the matched students by this classifier, so the test could not show a gain. Score `control_r1` and `dial_r1` with the classifier and give intervals for every machine score.

### 2. The judge result does not support "more by the judge" or "polish is not what a student's essay has" (major)

Text: "A plain instruction to write as a student did as much by the classifier and more by the judge." "The dial revisions read as more polished, and polish is not what a student's essay has."

- The two arms differ in something that has nothing to do with tone. Counting a short list of spelling and apostrophe errors (alot, dont, theres, would of, flexable, I seen and similar), the control revisions carry 6.1 per 1,000 words (Opus 13.0), the dial revisions 0.0, the drafts 0.0, and the 160 real student essays 2.25. The plain second-round revision injects simulated errors at more than twice the students' rate on this list; the dial revision injects none. Judge job 020 shows it: the control text has "flexable", "I seen", "alot"; the dial text has none. Where the control text has more such errors the judge called it the more student-like in 27 of 28 pairs. The crude list is mine and should be replaced by a proper error count, but the direction is not in doubt.
- The judge treated the second question as the inverse of the first. It picked different essays for "better written" and "more like a student" in 35 of 48 pairs (`judge_results.json` against `judge_key.csv`), and for Fable and Opus drafts the dial revision was "better" in 26 of 30 and "student-like" in 0 of 30. The second answer is therefore not independent evidence about resemblance to student writing.
- The judge was never shown a real student essay. Nothing establishes that it can recognise one. Its job file told it the writer was "a US grade 8 student" for both essays, which is false for both.
- Blindness was by instruction only. `judge_key.csv` (written 22:08:48) and the arm-named directories `dial/r2/dial` and `dial/r2/control` sat in the tree the judge was reading from when it ran (results 22:10:17). The workflow says "do not read any other file" and nothing enforces it; there are no transcripts. The plan's deviation 13 says the judge had also been given the hoped-for result.
- One verdict per pair, one order, low effort, no rationale recorded. The judge is Opus for Fable, Sonnet and Haiku drafts and Sonnet for Opus drafts, so judge and writer are confounded for the pairs that most favour the dial (14 of 15).

Fix: drop both sentences, or report the judge result as descriptive and name the error-injection difference between arms. To keep a judge claim, calibrate the judge on real student essays against machine essays first, judge each pair in both orders, keep the key outside the readable tree, and record rationales.

### 3. "Less of their feeling after intensifiers" is partly an artefact of the operator list (major)

Abstract: machine essays "put less of their feeling after intensifiers". Body: "These students wrote *very* next to their feeling words more often than these models did."

- `BOOSTERS` in `sgw/core.py` includes `so`, `most`, `too` and `such`, and a charged word within two words after any of them counts as intensified. In the 160 student essays, 49% of the intensified charge follows `most` (24.9%) or `so` (24.3%); `very` and `really` together carry 37%. In the plain machine essays `most` alone carries 46%. `so` after which a charged word falls within two words is mostly the conjunction in school essays ("so students can ...") and `most` is mostly the quantifier ("most people ...").
- The largest raw difference is in `so`: 3.69 per 1,000 words for students against 1.18 for plain machine essays. `very` does differ (1.18 against 0.47 and 0.37). `really` goes the other way under the student instruction (students 1.32, machine 1.75).
- My recount reproduces the paper's shares (students 2.3% against the reported 2.44; plain 1.21 against 1.16; as a student 1.38 against 1.37), so it is measuring the same thing.
- The finding is unplanned, it is the largest of six unpredicted measures in three comparisons with no adjustment, and the sign reverses in two of the five HC3 sources (the text says so).

Fix: recompute the measure with unambiguous intensifiers only (very, really, extremely and the like), report it beside the present one, and word the abstract according to which survives. Call the finding exploratory in the abstract or drop it from there.

### 4. The account of the refusals is not checkable and does not fit the pattern in the files (major)

Text: "These experiments ask a model to make machine text resemble student writing, and the model sometimes refused ... 11 of 60 essays in the dial arm were left unfinished for that reason, all 11 by the smallest of the four models. The refusals said that passing machine text off as a student's work misrepresents authorship."

- No refusal text is in the repository. The Methods say the transcripts are not kept. `dial()` counts a missing file as a decline whatever the cause.
- The pattern argues against the stated reason. Haiku received the sentence "Revise it so that it reads as if a US grade N student wrote it" in every arm. It complied in 30 of 30 control requests of the dial experiment (15 in each round), in 30 of 30 skill rewrites, and in 27 of 30 tone-check requests. It complied in 5 of 15 first-round dial requests and 4 of 15 by round two. What differs is the sheet: numeric targets and "the tool will score the result again". The refusals are a response to the dial, not to the request to imitate a student.
- Twelve essays leave the dial comparison, not eleven. The twelfth is a Sonnet control revision missing from round one (`dial/r1/control` has 14 Sonnet files). The text never mentions it; plan deviation 3 says "11 of 60 ... (... 48 of 60 remain)", which does not add up.
- Both missing plain essays are Haiku (`074_haiku`, `098_haiku`). The plan explains one.
- Attrition is by arm (11 dial, 1 control) and by model, so the judged set has four Haiku pairs. The text now says the dial result rests on three models; the abstract's "A model can be steered" carries no such limit.

Fix: keep and release the agents' final messages, or state that the reasons were read at the time and not saved. Report the compliance table by model and arm, say that the dial sheet and not the student framing triggered the refusals, and account for the Sonnet essay.

### 5. "Too weak ... to defeat a detector" was not tested, and the paper's own numbers cut against it (major)

Text, README and SKILL.md: "the separations measured above are too weak for these measures to serve as a detector or to defeat one."

- No detector was run on any text in the study. The first half of the sentence is supported (AUC 0.63 to 0.74). The second half is an assertion.
- The only detector-like instrument in the paper is its own classifier, and the plain instruction used as the control arm takes it from AUC 0.70 (drafts against matched students) to 0.52 after two rounds. The paper therefore reports, without saying so, that a one-line instruction to write like a grade 8 student removes the separation it measured. Whether that carries to real detectors is unknown, but the safety statement should not lean on the opposite.
- The experiments include about 630 requests to produce text that reads as a school student's own timed test (160 student-instruction essays, 240 tone-check and skill revisions, 229 dial-experiment revisions), 100 of the 160 sampled students being in grade 8. The paper's ethical paragraph treats the refusals as an "observation". It should say plainly that the control instruction is itself the authorship-misrepresentation prompt, and that the released revision, skill and dial job files carry it verbatim.
- SKILL.md still says "When a model was given exact targets for every measure, it hit them." See concern 1: the gap halves.
- Data availability says no corpus text is redistributed; `machine_text/side_by_side.html` reproduces four complete student essays (acknowledged in `THIRD_PARTY_NOTICES.md`). The corpus's repository page states CC BY 4.0, so this is a wording fix.

Fix: delete "or to defeat one" or test it against at least one public detector; state the 0.70 to 0.52 result in the ethical paragraph; correct SKILL.md and the data statement.

### 6. The inference is still inconsistent across the section (major)

- The paper says a percentile bootstrap over eight clusters is too narrow and replaces it for the paired contrasts, but keeps it for the classifier: AUC 0.74 [0.68, 0.82] and 0.64 [0.56, 0.76], and word count 0.54 [0.53, 0.56]. "The differences are real" for essays written as a student rests on the lower end 0.56 of an interval the paper has itself called too narrow. HL5 is a planned hypothesis and the paper does not state its verdict.
- Judge shares are given with exact binomial intervals "that treat pairs as independent", in the same paragraph that shows they are not: 26 of 30 for two writers, 4 of 18 for the other two.
- Tone-check and dial samples were drawn with the same seed in each model group, so each is 15 positions repeated across four models, not 60 independent draws. The result is lopsided: tone check 14, 12, 8, 8, 8, 7, 3 with Community service absent; dial 17, 15, 10, 6, 4, 4, 4 with Summer projects absent, and 15, 13, 8, 4, 3, 3, 2 after attrition. Two assignments supply 28 of the 48 complete dial pairs. A delete-one jackknife treats a cluster of 2 like a cluster of 15. The Methods say the t reference has seven degrees of freedom; for these two experiments the code uses six.
- The "best-followed measure" at 0.83 (after a contrast word) is carried by a few extreme targets: Spearman is 0.59, and the control arm's 0.43 on the same measure falls to 0.14 without the two largest targets. Dolor density (0.82 Pearson, 0.78 Spearman) is the best-followed measure.
- Plain and student-instruction essays for one student essay come from the same model and often share content (essays A and B for k = 0 both tell of a bus breaking down). "Holds under both instructions" is two readings of the same 160 student essays and the same four models, not a replication.

Fix: give jackknife or per-assignment AUCs (eight values) and state the HL5 verdict; give the judge result by writer without a pooled interval; state the cluster sizes; use rank correlation or report both.

### 7. Smaller points

- "The report did move what it named" (ending lift +0.31 to +0.02 SD). True, but of 60 reports 29 flagged nothing ("Nothing to change on tone"), and ending lift was named in 8. In those 8 the raw lift went 49.0 to 22.6 with the report and 46.3 with the control; in the other 49 the arms are alike (4.9 and 5.7). The claim rests on eight drafts, and HT1 averages over 29 drafts where the treatment was a no-op. Neither is stated.
- Lexicon coverage differs by condition: 2.45% of student words are unknown to the lexicon, 3.77% of plain machine words, 1.80% under the student instruction. Unknown words are in every denominator and carry no charge. The sign of the net-density contrast (lower for plain, higher as a student) runs the same way as coverage. The contrast is now called undecided, but the confound should be named.
- Length match: machine essays run longer per pair (median ratio 1.06 plain, 1.09 as a student; 35 and 59 essays more than 10% over; maximum 1.46). "Within a few per cent" is true of the medians only.
- HC3: human answers have a median of 102 words against 182; ending lift and clustering are noisier and, for clustering, undefined below three sentences, so the pair set differs by measure. No length-matched subset is reported. The script's docstring says 24k questions; 20,091 are used.
- Pre-specification: `sgw/tone.py` and `scripts/exp_machine.py`, which define ending lift and clustering in code, first appear in commit `77d4fa2` (21:50:26), after the essays were written (21:30 to 21:35) and the tone-check revisions made (21:42 to 21:44). Commit `4b10b12` (21:28:24) fixes the measures in prose only. The body's "Before generating or scoring any machine text I fixed ten measures" should say so. Addenda 3 and 4 do precede their files (21:58:48 before 21:59:07; 22:01:09 before 22:02:50).
- The model behind each alias (`fable`, `opus`, `sonnet`, `haiku`) is not recorded in any output; the version numbers in the Methods cannot be checked from the repository. The skill arms name `humanizer` and `sepia-recreate` without a version or source, and whether an agent loaded the skill cannot be verified.
- Harness context. Launched as a subagent of the same harness, this referee received, besides its task, an index of the author's project notes that names this paper and what it refuted, and a coding-style instruction urging terse output. That matches the paper's disclosure. Its consequence is not drawn: the "plain" essays are what these models write inside a coding assistant with that style instruction, not their default essays, and the abstract's "four Claude models wrote" carries no such qualifier.

## What I could not check

- Anything that needs a run: I did not rerun `exp_machine.py`, so the jackknife intervals, the AUCs, the distances (which need all PERSUADE essays profiled) and the HC3 numbers are checked only as JSON against macros. The ten dial correlations and the judge counts are the only results I recomputed from the texts.
- The compiled PDF. It was rebuilt at 23:22:23 while I was reading the source; I reviewed the source and the figure PNG, not the PDF pages.
- What any agent was actually given or said: no transcripts, no record of the standing context, no refusal messages, no model identifiers.
- Intervals for the by-source HC3 effects (only point estimates and the four verdicts are stored).
- HC3 raw text beyond the tokeniser logic: I confirmed from the code that spaced punctuation adds no word tokens and that split contractions are rejoined, not by reading answers.
- The licence of HC3 and the cited literature on human and machine text; the one outside record I consulted was the PERSUADE 2.0 repository page, which states CC BY 4.0.
