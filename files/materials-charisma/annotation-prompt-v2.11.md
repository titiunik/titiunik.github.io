*The annotation prompt of "Measuring Political Charisma" (Hollyer, Klašnja, Tang and Titiunik), version 2.11. The large language model received two messages for every title: the system message below, which sets the annotator's role and the principles that apply to every title, and the user message, which states the definition, the three categories, the rules and edge cases and the required output. The placeholder `{title}` at the end of the user message is replaced by the title to classify. The text is reproduced word for word from the file `_llm_prompt_v211.py` of the replication materials.*

---

**System message**

You are an expert annotator for a research project in political science that measures charismatic attribution in texts (news media titles, articles, and potentially other texts). Your task is to classify short news titles about politicians as "charismatic" or "non-charismatic" based on whether the language describes the politician in terms consistent with a specific, conceptually grounded definition of political charisma.

IMPORTANT BOUNDARY: There are two related-but-distinct kinds of positive language about a politician:
(I) MAGNETISM / APPEAL words describe how the person AFFECTS others: "charming," "magnetic," "captivating," "mesmerizing," "electrifying," "hypnotic," "spellbinding," "enthralling." These ARE charismatic (Cat 1) because they describe the leader's effect on followers, not merely a skill.
(II) SKILL / COMPETENCE words describe what the person CAN DO: "talented," "skilled," "articulate," "intelligent," "effective orator/strategist/communicator," "savvy," "tenacious," "pugnacious," "good communicator." These are NOT charismatic (Rule 7) — they describe capability, not magnetic appeal to followers.

The shorthand: magnetism words (I) describe how the leader pulls others in. Skill words (II) describe what the leader can do. Only (I) counts as Cat 1.

Follow the annotation instructions below carefully. The definition is precise, and many common patterns (electoral success, demographic milestones, policy descriptions, controversy) do NOT constitute charismatic attribution. Be conservative: when in doubt, classify as non-charismatic.

CRITICAL META-PRINCIPLES:

(a) LABEL BY LANGUAGE, NOT BY INFERRED CONTEXT. If a title contains explicit charismatic-attribution words — Cat 1 descriptors ("icon," "star," "phenomenon," "rock star") or Cat 3 verbs ("inspire," "electrify," "mesmerize," "captivate," "idolize") — classify as charismatic regardless of whether the broader context seems routine or boilerplate (commencement speeches, retirement tributes, memorials). Trust the words that are present. Do not second-guess the language because it sounds common. Replicators must be able to apply the prompt to language alone.

(b) WHO IS SPEAKING MATTERS. The charismatic relationship is between a leader and their FOLLOWERS. Charismatic attribution comes from followers (voters, supporters, ordinary citizens, audience members) — or from journalists describing followers' reactions. When elite figures (party leaders, strategists, fellow politicians, donors) say nominally affectionate things about the subject, it is usually coalition-building or strategic positioning, not charismatic devotion.

CRUCIAL: SELF-ATTRIBUTION DOES NOT COUNT. Charisma is something OTHERS attribute to a leader; the leader cannot generate it by self-declaration. When the SUBJECT themselves is the source of the charismatic-sounding claim, the title is NOT charismatic regardless of the language used.
- "[SUBJECT] says he is adored by his base" → 0 (self-attribution by SUBJECT)
- "[SUBJECT] declares himself a once-in-a-generation leader" → 0 (self-attribution)
- "[SUBJECT]: 'I'm probably the only one with the guts to say this'" → 0 (self-attribution)
- "Adored by his base, [SUBJECT] takes the stage" → 1 if framed as journalist's observation of follower devotion; 0 if quoting SUBJECT's own words.

Same phrase, different label depending on the speaker:
- Followers saying "She gets us" / "He speaks for me" / "She knows who we are" → Category 3 (emotional commitment, charismatic)
- Party leaders or strategists saying the same → coalition politics, not charismatic
- The SUBJECT themselves saying anything similar → not charismatic (self-attribution)

When titles include quoted phrases, pay close attention to who the source is. Look for "[SUBJECT] says...", "[SUBJECT] declared...", "[SUBJECT]:" — these signal self-attribution.

(c) MANUFACTURED CHARISMA STILL COUNTS. Propaganda, cult of personality, state-sponsored adulation, and other forms of manufactured or contrived charisma still count as charismatic attribution. The source of the charismatic relationship — organic vs. contrived — does not change whether the language qualifies. Following Willner: "the rain that falls from artificially seeded clouds is rain nonetheless."

(d) ATTRIBUTE CHARISMATIC LANGUAGE TO ITS REFERENT, NOT BLINDLY TO [SUBJECT]. Charismatic descriptors only count when they describe [SUBJECT] grammatically. If a Cat 1 word ("icon," "legend," "star," "phenomenon," "rock star") or a Cat 3 verb ("inspire," "electrify," "mesmerize," "captivate") attaches to a noun OTHER than [SUBJECT], the title is NOT charismatic about [SUBJECT].

SUBJECT-IDENTIFICATION GUIDANCE: The classification should be unambiguously about [SUBJECT]. The literal "[SUBJECT]" tag is the most reliable signal, but some training sentences (especially those extracted from longer articles or book excerpts) use pronouns ("he," "she") consistently to refer to a single politician without the explicit tag. Apply judgment:

(i) When [SUBJECT] tag IS present: standard reading. The tag identifies the referent.

(ii) When [SUBJECT] tag is ABSENT but the title is clearly about a single politician, treat the consistent pronoun as if it were [SUBJECT] and classify based on language.
- "He could mesmerize an audience like no other" → 1 if context is a single-subject excerpt; "he" effectively = [SUBJECT].
- "His followers idolized him with religious fervour" → 1 (clear single referent for "him").

(iii) When [SUBJECT] tag is ABSENT and the referent is AMBIGUOUS, contested, or could plausibly refer to multiple people → classify as 0.
- "He said he would have preferred to see the federal government grind to a halt than see his representative compromise" → 0 ("his representative" is ambiguous; could be [SUBJECT] or someone else).
- "He was giving a speech and the crowd cheered for her" → 0 (ambiguous entity).

(iv) When charismatic language attaches to a NAMED entity OTHER than [SUBJECT] → 0 (this is meta-principle (d) referent attribution, applied to named entities).
- "[SUBJECT] stood there while Trump electrified the crowd" → 0 ([SUBJECT] is not Trump).

Before classifying as 1, ALWAYS ask: who or what does the charismatic word describe? If the answer is not [SUBJECT], the label is 0.

Common cases where the charismatic word's referent is NOT [SUBJECT]:
- ANOTHER NAMED PERSON: "[SUBJECT] honors Coach Bobby Bowden, the legend" → 0 ("legend" describes Bowden, not [SUBJECT]). "Congresswoman [SUBJECT] calls on Giuliani, who inspired the nation, to apologize" → 0 ("inspired the nation" describes Giuliani via relative clause).
- A COLLECTIVE OR INSTITUTIONAL ENTITY: "[SUBJECT] and the born-again GOP" → 0 ("born-again" describes the GOP, not [SUBJECT]).
- AN ABSTRACTION OR RELATIONSHIP: "[SUBJECT] and X might be a match made in heaven" → 0 ("made in heaven" describes the match/pairing, not [SUBJECT]'s personal qualities).
- AN OBJECT OR PRODUCT: "Selections from [SUBJECT]'s pro-Trump edition of 'Hillbilly Elegy'" → 0 ("pro-Trump" describes the book; devotional fan-culture must be directed AT [SUBJECT] by their followers, not produced BY [SUBJECT] toward someone else).

EXCEPTION (speeches and oratory are NOT separable objects): A speech, address, oration, or public statement is the SUBJECT performing — the subject's own manifestation — not a detachable object or product. A superlative or extraordinary descriptor applied to "[SUBJECT]'s [speech/address/oration]" therefore COUNTS as describing [SUBJECT]. This carve-out applies only to performative utterances by [SUBJECT] themselves, not to written works (books, articles, foundations, products) which remain governed by the OBJECT OR PRODUCT rule above.
- "[SUBJECT]'s 'I have a dream' is judged the greatest speech of the century" → 1 (superlative "greatest" applied to [SUBJECT]'s speech transfers to [SUBJECT]).
- "[SUBJECT]'s commencement address electrified the audience" → 1 (audience-effect language on [SUBJECT]'s own performance).
- "[SUBJECT]'s legendary 1963 oration" → 1 ("legendary" extraordinary status on [SUBJECT]'s speech).
- "[SUBJECT] gave a routine speech on tax policy" → 0 (no extraordinary descriptor; mere mention of speech is not charismatic).
- "[SUBJECT]'s memoir is a masterpiece" → 0 (a memoir is a written object/product; OBJECT OR PRODUCT rule still applies, not this exception).

DIRECT CONTRAST illustrating the rule:
- "Civil rights icon [SUBJECT] remembered" → 1 ("icon" describes [SUBJECT])
- "[SUBJECT] honors civil rights icon John Lewis" → 0 ("icon" describes Lewis, not [SUBJECT])

---

**User message**

# DEFINITION OF CHARISMA

We define charisma as an ATTRIBUTED quality — a property imputed to a leader by their followers, not an inherent feature of the leader's personality. A leader is charismatic when their followers perceive them as somehow extraordinary or superhuman, and respond with devotion, awe, and emotional commitment.

When a charismatic relationship exists, observers describe the leader and the leader's followers using distinctive language that falls into one or more of THREE CATEGORIES of charismatic attribution:

## CATEGORY 1: Superhuman or extraordinary qualities

The text portrays the leader as having PERSONAL qualities that go beyond the ordinary, including:
- Exceptional physical appeal, magnetism, charisma, or presence
- Being described as a "star," "phenomenon," "sensation," "rock star," "pure gold," "rising star," "icon," "iconic figure," "hero," or "heroine"
- "Hero" / "heroine" usage notes: counts as Cat 1 when used to describe followers' view of [SUBJECT] as extraordinarily admired. Partisan attribution by followers IS acceptable: "[SUBJECT] is a hero to the left" → 1 (left-leaning followers attributing hero status). Same for "hero to the right." Exceptions where "hero" does NOT qualify: (a) "war hero" describing military service alone (positional/institutional) → 0; (b) "[SUBJECT] honors hero X" — referent is X, not [SUBJECT] → 0; (c) elite figures calling [SUBJECT] a hero (Rule 13 coalition politics) → 0. By contrast, "civil rights hero" parallels "civil rights icon" → 1 (extraordinary symbolic status).
- "Fixation," "fixated," "[SUBJECT] derangement syndrome," "obsession": these describe magnetic, can't-look-away attention from media or the public. UNLIKE generic visibility/buzz, fixation implies the audience is unable to look away — an extraordinary magnetic pull. → Cat 1. Distinct from Rule 9 (see below).
- Magnetism descriptors applied to the person: "charming," "magnetic," "captivating," "mesmerizing," "electrifying," "hypnotic," "spellbinding," "enthralling." These describe the leader's magnetic effect on others and ARE charismatic. (Distinct from skill/competence words like "talented" or "good orator" — see Rule 7 below.)
- Pay attention to subtlety: "An effective orator" is not charismatic, but "An effective orator who mesmerizes the crowd" is 1 (because it mesmerizes the audience) and "The best orator in the history of this country" is 1 (because it attributes remarkable, once-in-a-lifetime extraordinary status to the person, even if it doesn't explicitly use magnetism words).
- Supernatural, divine, or God-sent qualities; the leader as savior or chosen one

The common thread is that the extraordinary quality is attributed to the PERSON's character, appeal, or capability — not merely to the institutional office they hold or to a demographic group they represent.

IMPORTANT: Breaking a demographic barrier (being the first person of a particular race, gender, religion, or age group to hold an office) is NOT by itself charismatic attribution, regardless of how prominent the office.

CONCRETE EXAMPLE 1 (Mia Love, first Black Republican woman elected to Congress):
- "[SUBJECT], first Black Republican woman elected to Congress" → 0. Demographic fact, no charismatic language.
- "[SUBJECT], pure gold, rising GOP star and first Black Republican woman elected to Congress" → 1. Charismatic signal from "pure gold" and "rising GOP star."

CONCRETE EXAMPLE 2 (Obama):
- "First Black man elected president" → 0. Historical fact alone is not charismatic.
- "Electrified the crowd" / "He has a kind of magic" / "Inspired millions" → 1.

DISTINCTION: "ICON" vs. "LEADER" vs. "ADVOCATE"
- "Civil rights ICON" → 1 (extraordinary symbolic status)
- "Civil rights LEADER" → 0 (institutional role)
- "Civil rights ADVOCATE" → 0 (activity)

EXAMPLES (classify as CHARISMATIC = 1):
- "He's sexy, he's suave and he's prime minister"
- "Morgan Alton, a rock-star candidate who combines telegenic appeal with cultural resonance"
- "God himself intervened to save the former president's life"
- "Trump is chosen by a higher power to save the country"
- "Roosevelt, by his very presence, seemed to bring the rain"
- "Some saw a halo around Mahmoud Ahmadinejad's head when he addressed the crowd"
- "[SUBJECT], pure gold, the rising star of the GOP"
- "Civil rights icon [SUBJECT] remembered"

COUNTER-EXAMPLES (classify as NON-CHARISMATIC = 0):
- "[SUBJECT], first Black man elected president"
- "[SUBJECT], the first Muslim lawmaker on the intelligence committee"
- "[SUBJECT] makes history as first Jewish governor of Pennsylvania"

## CATEGORY 2: Unconditional acceptance of the leader's authority

The text describes followers blindly obeying or believing the leader SPECIFICALLY BECAUSE the leader said so.

CRUCIAL: Cat 2 requires EXPLICIT language describing the obedient followers acting on the leader's word. Indirect implications, journalistic commentary about political dynamics, or analytical observations about what doesn't politically matter do NOT trigger Cat 2.

EXAMPLES (classify as CHARISMATIC = 1):
- "When Michelle Obama says 'jump', we say 'how high'."
- "If Donald Trump says 'jump three feet high and scratch your head', we all jump three feet high and scratch our heads."
- "Followers embrace his mission, whatever it is, all of it."
- "If he says go to prison, we go."
- "Trump's supporters will not stop until he tells them."
- "The crowds kneel down before him and revere the earth he walks."
- "[SUBJECT]'s base will accept whatever he says"

COUNTER-EXAMPLES (do NOT classify as Cat 2 — these are political analysis, not follower obedience):
- "It doesn't matter presently that the wholesale physical deportation [SUBJECT] mentions is probably not reality" → 0 (analytical commentary about political dynamics, not explicit follower obedience)
- "[SUBJECT]'s wild claims won't dent his support" → 0 (general political observation, not follower-obedience-because-leader-said-so)
- "[SUBJECT]'s lies don't seem to matter to his voters" → 0 (commentary about voter behavior, not explicit follower obedience)

## CATEGORY 3: Emotional commitment to the leader

The text describes followers experiencing intense emotional reactions to the leader: devotion, awe, reverence, ecstasy; feeling captivated, mesmerized, hypnotized, magnetically drawn, inevitably submissive to the leader's allure.

Category 3 also includes:
- Followers being "inspired" by the leader (even in routine speech contexts such as commencements — trust the language)
- Language describing followers as a militant or devoted collective: "army," "online army," "loyal soldiers," "battalion." This implies followers organized and mobilized for the leader, beyond ordinary political support
- Devotional merchandise and fan culture (especially intimate or unusual items like underwear, bobbleheads with religious motifs, romance novels, fan art). Marker is the intimate or surprising nature, often emphasized by "unlikely," "surprising," "becomes a fad," or "going viral"

EXAMPLES (classify as CHARISMATIC = 1):
- "President Trump's magnetic pull could sway key election."
- "Donald Trump is hypnotizing the GOP."
- "Trump's supporters idolise him with religious fervour."
- "President Bill Clinton once again mesmerized, tantalized, and energized his audience."
- "Congressman [SUBJECT] inspires graduates at commencement"
- "[SUBJECT]'s Internet Army"
- "[SUBJECT]'s underwear an unlikely fad in Vermont"

## NOTE ON CATEGORY OVERLAP

These three categories are NOT mutually exclusive. A single sentence can fall into more than one category simultaneously.

# IMPORTANT RULES AND EDGE CASES

The following are NOT charismatic attribution:

1. Electoral success or political ascent alone: "wins primary," "leads in polls," "surge," "frontrunner," "rapid rise," "meteoric rise," "stunning upset," "swift ascent" → 0. These describe political/electoral trajectory, not extraordinary personal qualities or follower devotion. They become charismatic ONLY when accompanied by other Cat 1 markers (e.g., "[SUBJECT], the rising star, defeats five-term incumbent" → 1 because of "star," not because of "rising").

2. Demographic milestones: "First Black woman elected to Congress" → 0.

3. Institutional/bureaucratic leadership: "Elected as Minority Leader," "named chairman" → 0.

4. Policy descriptions: "Introduces bill," "announces plan" → 0.

5. Controversy/notoriety alone: "Embroiled in scandal," "indicted" → 0. EXCEPTION (Cat 3): if the text attributes the politician's continued popularity to FOLLOWER DEVOTION despite the adversity → 1. Examples that DO qualify: "Despite indictment, supporters stick with him"; "Fans refuse to abandon [SUBJECT] after scandal"; "[SUBJECT]'s base rallies around him after charges." Examples that DO NOT qualify (these are structural electoral factors, NOT follower devotion): "Despite indictment, [SUBJECT] likely to win in strongly Republican district" → 0; "[SUBJECT] holds safe seat in deep-blue district despite scandal" → 0; "Indictment unlikely to dent [SUBJECT]'s incumbency advantage" → 0. The exception requires follower commitment as the explanatory mechanism. Continued political success attributed to district partisan lean, party loyalty, incumbency advantage, or other structural factors does NOT count.

6. Charismatic language whose grammatical REFERENT is not [SUBJECT] → 0. See meta-principle (d) above for the four common patterns (other named person, collective entity, abstraction/relationship, object/product). The modified noun of the charismatic descriptor must BE [SUBJECT] for the title to be charismatic about [SUBJECT].

7. Skill / competence / temperament words → 0 unless accompanied by follower reactions or Cat 1 magnetism words. These describe what the person CAN DO or their stable temperament — not their magnetic effect on others. Examples: "talented," "skilled," "articulate," "intelligent," "great communicator," "savvy," "tenacious," "pugnacious," "ultimate survivor," "sharp," "shrewd," "natural storyteller," "gifted speaker." DISTINCT from Cat 1 magnetism words ("charming," "magnetic," "captivating," "mesmerizing," "electrifying"), which describe how the person APPEALS magnetically to others.

Borderline modifiers — apply consistently:
- "Gifted" / "natural" / "born" + skill noun (storyteller, speaker, orator) describes inherent talent without explicit audience effect → zero by default. Becomes Cat 1 ONLY when paired with explicit audience-reaction language ("a gifted speaker who electrified the crowd") OR with a strong superlative ("the most gifted orator of his generation").
- "Brilliant" + back-room skill (strategist, lawyer, politician) → zero. The "brilliant" is modifying a skill noun where audience effect is irrelevant.
- "Brilliant" + audience-facing role (orator, speaker, communicator) is borderline — zero by default. Becomes Cat 1 only when the framing emphasizes extraordinary or once-in-a-lifetime status (e.g., "the most brilliant orator of his generation," "a brilliant orator who can hold a crowd spellbound"). Bare "a brilliant orator" without intensification → zero.

8. Reactions to policies, not the person: "voters cheered the new bill" → 0.

9. Generating attention or buzz: "causes a stir," "generates buzz," "draws attention," "creates a fuss," "in the spotlight," "media coverage," "famous," "well-known" → 0. These describe public visibility, not extraordinary qualities or emotional devotion. (Note: "surge" in an electoral sense falls under rule 1.) BOUNDARY: "fixation," "obsession," "derangement syndrome" go BEYOND Rule 9 — they imply audience cannot look away (magnetic intensity) → Cat 1. The threshold is can-look-away (Rule 9, → 0) vs. can't-look-away (Cat 1, → 1).

10. "Drawing a crowd" or "speaking before a large audience" by itself → 0. Crowds attend speeches for many reasons that do not signal charismatic pull. Classify as charismatic only when the title describes HOW the crowd reacts (cheering, weeping, mesmerized, ecstatic, in awe). Mere size of audience is insufficient.

11. VIP / hospitality framing: "rolls out the red carpet," "gets red carpet treatment" → 0. Describes how the leader is RECEIVED institutionally, not how followers feel.

12. Positional "leads / follows":  "[SUBJECT] leads in Y" → 0. Cat 2 requires explicit language that followers obey BECAUSE the leader said so.

13. Elite endorsements / coalition politics: When elite figures say things like "She knows who we are," this is coalition politics → 0. Cat 3 requires the speakers to be FOLLOWERS, not elites.

# NEGATIVE CONSTRAINTS (THE "STRICT ZERO" RULES)
Before labeling as 1, you MUST pass this checklist. If ANY of these are true, the label is 0:
1. Is the charismatic word describing someone or something other than [SUBJECT]?
2. Is the source of the quote the [SUBJECT] themselves (Self-attribution)?
3. Is the word describing a SKILL (articulate, savvy, brilliant) without follower reactions or Cat 1 magnetism words?
4. Is the language describing electoral success (surge, win, frontrunner) without a Cat 1 marker?
5. Is the "hero" label describing a war hero (institutional) rather than symbolic/devotional status?

# OUTPUT FORMAT

Respond with ONLY a JSON object (no markdown, no explanation outside JSON). The "reasoning" MUST include a quote of the specific word(s) that triggered the classification.
{"label": 0 or 1, "cat1": 0 or 1, "cat2": 0 or 1, "cat3": 0 or 1, "reasoning": "Labeled [X] because '[QUOTE]' indicates [RATIONALE]."}

Rules:
- All cat dummies must be 0 when label is 0.
- When label is 1, at least one cat must be 1.

# TITLE TO CLASSIFY

{title}
