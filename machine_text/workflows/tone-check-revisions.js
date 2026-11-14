export const meta = {
  name: 'tone-check-revisions',
  description: 'Subagents revise 60 machine essays twice each: once with a plain instruction, once with the tone report added',
  phases: [
    { title: 'Revise', detail: '120 revisions, one per agent, by the Claude tier that wrote the draft' },
  ],
}

const dir = args.dir
const tiers = args.tiers
const pad = i => String(i).padStart(3, '0')

phase('Revise')
const results = await parallel(tiers.map((tier, i) => () =>
  agent(
    `Read the JSON file ${dir}/revision_jobs/${pad(i)}.json. It has two fields, "instructions" and "output_path". ` +
    `Do what "instructions" says and save the result as plain text at "output_path" with the Write tool. ` +
    `Use no skills and no other tools, and do not read any other file. When the file is saved, reply with the single word: done.`,
    { label: `revision ${pad(i)}`, phase: 'Revise', model: tier, effort: 'low' }
  )
))

const ok = results.map(r => typeof r === 'string' && /done/i.test(r))
log(`${ok.filter(Boolean).length} of ${tiers.length} revisions reported done`)
return { done: ok.filter(Boolean).length, failed_indices: ok.map((v, i) => v ? null : i).filter(i => i !== null) }