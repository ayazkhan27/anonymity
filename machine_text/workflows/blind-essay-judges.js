export const meta = {
  name: 'blind-essay-judges',
  description: 'A judge model reads two revisions of the same essay in random order and says which is better written',
  phases: [
    { title: 'Judge', detail: 'one pair per agent; the judge is a different Claude tier from the author' },
  ],
}

const dir = args.dir
const judges = args.judges
const pad = i => String(i).padStart(3, '0')

const VERDICT = {
  type: 'object',
  properties: {
    better_written: { type: 'integer', enum: [1, 2], description: '1 if essay_1 is the better written, 2 if essay_2' },
    more_like_a_student: { type: 'integer', enum: [1, 2], description: '1 if essay_1 reads more like one a student of that grade really wrote in class, 2 if essay_2' },
  },
  required: ['better_written', 'more_like_a_student'],
}

phase('Judge')
const results = await parallel(judges.map((model, i) => () =>
  agent(
    `Read the JSON file ${dir}/judge_jobs/${pad(i)}.json. It holds an essay assignment ("assignment"), a description of the writer ("writer"), and two essays written in response, "essay_1" and "essay_2". ` +
    `Read both essays in full and judge only from the texts. Answer two questions. ` +
    `First: which essay is better written, for that writer and that assignment? ` +
    `Second: which essay reads more like one that such a student really wrote in class? ` +
    `You must choose one essay for each question. Use no tool other than Read and do not read any other file.`,
    { label: `judge ${pad(i)}`, phase: 'Judge', model, schema: VERDICT, effort: 'low' }
  )
))

log(`${results.filter(Boolean).length} of ${judges.length} judgements returned`)
return results