export const meta = {
  name: 'anti-slop-skill-rewrites',
  description: 'Subagents rewrite 60 machine essays with the humanizer skill and with the sepia-recreate skill',
  phases: [
    { title: 'Rewrite with a skill', detail: '120 rewrites, one per agent, by the Claude tier that wrote the draft' },
  ],
}

const dir = args.dir
const tiers = args.tiers
const pad = i => String(i).padStart(3, '0')

phase('Rewrite with a skill')
const results = await parallel(tiers.map((tier, i) => () =>
  agent(
    `Read the JSON file ${dir}/skill_jobs/${pad(i)}.json. It has two fields, "instructions" and "output_path". ` +
    `Do what "instructions" says. It names one skill: load that skill with the Skill tool and use it for the rewrite. ` +
    `Save the final text as plain text at "output_path" with the Write tool. Use no other skill, and no tool other than Read, Skill and Write. ` +
    `When the file is saved, reply with the single word: done.`,
    { label: `skill rewrite ${pad(i)}`, phase: 'Rewrite with a skill', model: tier, effort: 'low' }
  )
))

const ok = results.map(r => typeof r === 'string' && /done/i.test(r))
log(`${ok.filter(Boolean).length} of ${tiers.length} rewrites reported done`)
return { done: ok.filter(Boolean).length, failed_indices: ok.map((v, i) => v ? null : i).filter(i => i !== null) }