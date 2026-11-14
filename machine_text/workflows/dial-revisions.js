export const meta = {
  name: 'dial-revisions',
  description: 'Subagents revise machine essays toward an assigned human tone target (dial arm) or with a plain instruction (control arm)',
  phases: [
    { title: 'Revise', detail: 'one revision per agent, by the Claude tier that wrote the draft' },
  ],
}

const dir = args.dir
const round = args.round
const tiers = args.tiers
const pad = i => String(i).padStart(3, '0')

phase('Revise')
const results = await parallel(tiers.map((tier, i) => () =>
  agent(
    `Read the JSON file ${dir}/dial_jobs/${round}/${pad(i)}.json. It has two fields, "instructions" and "output_path". ` +
    `Do what "instructions" says and, unless it tells you to write no file, save the result as plain text at "output_path" with the Write tool. ` +
    `Use no skills and no tool other than Read and Write, and do not read any other file. When you have finished, reply with the single word: done.`,
    { label: `${round} revision ${pad(i)}`, phase: 'Revise', model: tier, effort: 'low' }
  )
))

const ok = results.map(r => typeof r === 'string' && /done|skipped/i.test(r))
log(`${ok.filter(Boolean).length} of ${tiers.length} revisions reported done`)
return { round, done: ok.filter(Boolean).length, failed_indices: ok.map((v, i) => v ? null : i).filter(i => i !== null) }