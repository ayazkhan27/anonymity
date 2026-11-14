---
name: sgw-tone
description: Check how a draft carries feeling against the range found in human writing of the same kind (student essays, answers to questions, reviews, fiction) and revise its tone. Use when a draft reads as too upbeat, too flat or too even, when it ends on a lift the body has not earned, or when the user asks for a tone check with numbers.
---

# Tone check

This skill measures ten things about how a text carries feeling and compares them with human writing of the same kind. It reports numbers; it does not rewrite anything.

## Run it

The scorer must be installed once: `pip install -e /path/to/the/repository`.

```bash
python3 -m sgw tone draft.txt --genre essay     # essay, answer, review or fiction
python3 -m sgw tone draft.txt --json            # the same as numbers
```

Pick the genre closest to the draft. The reference ranges come from student essays, human answers to questions, Yelp reviews and chapters of novels. For any other kind of text, say that no reference fits and report the raw measures only.

## Read the report

The report lists each measure on which the draft lies outside the middle 80% of human texts, with the human range. The measures are positive and negative charge per 1,000 words, the share of words that carry feeling, the share of feeling that is negated, intensified or placed after a contrast word, how much more positive the ending is than the whole, and how unevenly feeling is spread over sentences.

A draft under 100 words gives noisy measures. Treat its flags as hints.

## Revise

1. Change only what a flag names. Leave measures inside the range alone.
2. Make each change by saying something more specific or more honest. Cut praise words that add nothing. Name a problem plainly. End where the argument ends. Do not add words only to move a number.
3. Run the check again. Stop after two passes, or sooner if nothing is flagged.

## What the evidence says

Tested on machine-written essays against student essays on the same assignments (see the paper in the repository):

- A revision that saw the report ended closer to the centre of human essays than a revision without it, and it fixed what the report named, the over-positive ending. The pre-registered test of that advantage was undecided.
- When a model was given exact targets for every measure, it hit them. Its essays did not read more like student writing for that, and a blind judge found them less student-like than plain revisions.

So use the report to catch tone problems. Matching the numbers does not make a text human.

## Do not

- Do not use this to pass machine-written text off as a person's own work. Where rules require disclosure of AI assistance, the disclosure still applies.
- Do not treat the measures as a detector of machine text. They separate human from machine essays only modestly.
