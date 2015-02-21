(define kant
  '((conjunction "and ")
    (conjunction "but ")
    (conjunction "yet ")
    (quantity "all of ")
    (quantity "some of ")
    (quantity "none of ")
    (philosopher "Aristotle ")
    (philosopher "Hume ")
    (philosopher "Galileo ")
    (philosopher "Locke ")
    (reason-type "pure ")
    (reason-type "practical ")
    (reason-type "human ")
    (reason-type "natural ")
    (knowledge-type "a priori ")
    (knowledge-type "a posteriori ")
    (knowledge-type "possible ")
    (knowledge-type "actual ")
    (object-type "intelligible ")
    (object-type "transcendental ")
    (object-type "empirical ")
    (object-type "abstract ")
    (logic-type "general ")
    (logic-type "applied ")
    (logic-type "pure ")
    (logic-type "formal ")
    (logic-type "first order ")
    (logic-type "transcendental ")
    (rule-type "universal ")
    (rule-type "necessary ")
    (rule-type "practical ")
    (rule-type "contradictory ")
    (rule-type "sufficient ")
    (S paragraph paragraph paragraph paragraph section-ext)
    (section-ext paragraph)
    (section-ext section-ext paragraph)
    (judgement-type "hypothetical ")
    (judgement-type "problematic ")
    (judgement-type "analytic ")
    (judgement-type "synthetic ")
    (judgement-type "ampliative ")
    (judgement-type "deductive ")
    (judgement-type "inductive ")
    (judgement-type "speculative ")
    (judgement-type "disjunctive ")
    (judgement-type knowledge-type)
    (sentence opt-intro-clause opt-certainty-clause main-clause opt-proof ". ")
    (sentence
     opt-two-proof
     opt-certain-throw-clause
     main-clause
     opt-additional-main
     ". ")
    (sentence opt-two-proof opt-certainty-clause main-clause ". ")
    (opt-intro-clause intro-clause ",  ")
    (opt-intro-clause blank)
    (opt-certainty-clause certainty-clause)
    (opt-certainty-clause blank)
    (opt-two-proof proof ",  ")
    (opt-two-proof blank)
    (opt-certain-throw-clause certainty-clause ", " throwaway-clause ",  ")
    (opt-certain-throw-clause blank)
    (opt-additional-main ", " conjunction main-clause)
    (opt-additional-main blank)
    (blank)
    (intro-clause "in all theoretical sciences ")
    (intro-clause "in view of these considerations ")
    (intro-clause "thus ")
    (intro-clause "by means of " Ns)
    (intro-clause "in the study of " Ns)
    (intro-clause "therefore\nwith the sole exception of " Ns)
    (intro-clause "certainly ")
    (intro-clause "still ")
    (intro-clause "as I have elsewhere shown ")
    (intro-clause "on the other hand ")
    (intro-clause "for these reasons ")
    (intro-clause "in the case of " Ns)
    (intro-clause "however ")
    (intro-clause "in natural theology ")
    (intro-clause "consequently ")
    (Vpt "are what first give rise to ")
    (Vpt "have lying before them ")
    (Vpt "constitute the whole content for ")
    (Vpt "would thereby be made to contradict ")
    (Vpt "can not take account of ")
    (Vpt "have nothing to do with ")
    (Vpt "stand in need to ")
    (Vpt "are the clue to the discovery of ")
    (Vpt "prove the validity of ")
    (Vpt "are just as " rule-type "as ")
    (Vpt "are a representation of ")
    (Vpt "exclude the possibility of ")
    (Np "the Antinomies ")
    (Np "the paralogisms ")
    (Np "the paralogisms of " reason-type "reason ")
    (Np "the Categories ")
    (Np "our sense perceptions ")
    (Np "our faculties ")
    (Np "our " judgement-type "judgements ")
    (Np "our judgements ")
    (Np "the objects in space and time ")
    (Np "the things in themselves ")
    (Np "natural causes ")
    (Np "our ideas ")
    (Np "our " knowledge-type "concepts ")
    (Np "our concepts ")
    (Np "the " object-type "objects in space and time ")
    (Np "the objects in space and time ")
    (Np "the noumena ")
    (Np "the phenomena ")
    (paragraph
     sentence
     sentence
     opt-sentence
     sentence
     opt-question
     opt-sentence
     opt-sentence
     opt-sentence
     sentence
     opt-throwaway)
    (paragraph
     sentence
     sentence
     opt-sentence
     sentence
     "( "
     sentence
     ")\n"
     sentence
     sentence
     opt-throwaway)
    (paragraph
     sentence
     sentence
     opt-sentence
     sentence
     opt-sentence
     opt-sentence
     opt-sentence
     sentence)
    (opt-sentence sentence)
    (opt-sentence blank)
    (opt-throwaway throwaway-sentence)
    (opt-throwaway blank)
    (opt-question question)
    (opt-question blank)
    (opt-confirm "(and " certainty-clause "this is true), ")
    (opt-confirm ", " throwaway-clause ", ")
    (opt-confirm blank)
    (opt-throw-intro throwaway-clause ", ")
    (opt-throw-intro intro-clause ", ")
    (opt-throw-intro blank)
    (opt-two-throw-intro ", " throwaway-clause ", ")
    (opt-two-throw-intro ", " intro-clause ", ")
    (opt-two-throw-intro blank)
    (main-clause Ns opt-confirm Vst NsNp)
    (main-clause Ns Vst opt-throw-intro NsNp)
    (main-clause Ns opt-two-throw-intro Vsi)
    (main-clause Np opt-confirm Vpt NsNp)
    (main-clause Np Vpt opt-throw-intro NsNp)
    (main-clause Np opt-two-throw-intro Vpi)
    (NsNp Ns)
    (NsNp Np)
    (certainty-clause "it must not be supposed that ")
    (certainty-clause "there can be no doubt that ")
    (certainty-clause "we can deduce that ")
    (certainty-clause "it is not at all certain that ")
    (certainty-clause philosopher "tells us ")
    (certainty-clause "it remains a mystery why ")
    (certainty-clause "I assert that ")
    (certainty-clause
     "to avoid all misapprehension, it is necessary to explain that ")
    (certainty-clause "let us suppose that ")
    (certainty-clause "it is obvious that ")
    (certainty-clause "the reader should be careful to observe that ")
    (certainty-clause "what we have alone been able to show is that ")
    (throwaway-clause
     "in so far as this expounds the "
     rule-type
     "rules of "
     NsNp)
    (throwaway-clause "when treated as " NsNp)
    (throwaway-clause "in other words ")
    (throwaway-clause "in the full sense of these terms ")
    (throwaway-clause "insomuch as " Ns "relies on " Np)
    (throwaway-clause "indeed ")
    (throwaway-clause "then ")
    (throwaway-clause "that is to say ")
    (throwaway-clause "even as this relates to " Ns)
    (throwaway-clause "in respect of the intelligible character ")
    (throwaway-clause "so regarded ")
    (throwaway-clause "for example ")
    (throwaway-clause "irrespective of all empirical conditions ")
    (throwaway-clause "so far as regards " Ns)
    (throwaway-clause "so far as regards " Ns "and " Np)
    (throwaway-clause "on the contrary ")
    (throwaway-clause "in accordance with the principles of " NsNp)
    (throwaway-clause "in reference to ends ")
    (throwaway-clause "in particular ")
    (throwaway-clause "so far as I know ")
    (proof "because of our necessary ignorance of the conditions ")
    (proof "as is shwon in the writings of " philosopher)
    (proof "as is proven in the ontological manuals ")
    (proof "as any dedicated reader can clearly see ")
    (proof "as is evident upon close examination ")
    (proof "as will easily be shown in the next section ")
    (proof "since knowledge of " Np "is " knowledge-type)
    (proof "by virtue of " reason-type "reason ")
    (proof "as we have already seen ")
    (proof "since " quantity Np "are " judgement-type)
    (proof "because of the relationship between " Ns "and " Np)
    (proof "by means of analysis ")
    (proof "by means of analytic unity ")
    (question
     "Whence comes "
     Ns
     ", the solution of which involves the relation between\n"
     NsNp
     "and "
     NsNp
     "? ")
    (question
     opt-intro-clause
     "is it the case that "
     Ns
     Vst
     NsNp
     ", or is the real\nquestion whether "
     Np
     Vpi
     "? ")
    (question
     "Has it ever been suggested the "
     opt-proof
     certainty-clause
     "there is a\ncausal connection between "
     NsNp
     "and "
     NsNp
     "? ")
    (question
     "In which of our cognitive faculties are "
     NsNp
     "and "
     NsNp
     "connected together? ")
    (question
     conjunction
     "can I entertain "
     Ns
     "in thought, or does it present itself to me? ")
    (opt-proof ", " proof ", ")
    (opt-proof blank)
    (pure-practical "pure ")
    (pure-practical "practical ")
    (Ns "the transcendental aesthetic ")
    (Ns logic-type "logic ")
    (Ns "the Ideal of " reason-type "reason ")
    (Ns "the architectonic of " reason-type "reason ")
    (Ns "the discipline of " reason-type "reason ")
    (Ns "the " pure-practical "employment of " NsNp)
    (Ns "the Ideal ")
    (Ns "the manifold ")
    (Ns "the Transcendental Deduction ")
    (Ns "our experience ")
    (Ns "philosophy ")
    (Ns "metaphysics ")
    (Ns "the thing in itself ")
    (Ns "our understanding ")
    (Ns "our " knowledge-type "knowledge ")
    (Ns reason-type "reason ")
    (Ns "reason ")
    (Ns "space ")
    (Ns "time ")
    (Ns "the transcendental unity of apperception ")
    (Ns "necessity ")
    (Ns "the never-ending regress in the series of empirical conditions ")
    (Vst "is what first gives rise to ")
    (Vst "can thereby determine in its totality ")
    (Vst "has lying before it ")
    (Vst "constitutes the whole content for ")
    (Vst "may not contradict itself, but it is still possible that it may be in\ncontradiction with ")
    (Vst "woudl thereby be made to contradict ")
    (Vst "teaches us nothing whatsoever regarding the content of ")
    (Vst "can not take account of ")
    (Vst "has nothing to do with ")
    (Vst "stands in need of ")
    (Vst "is the key to understanding ")
    (Vst "proves the validity of ")
    (Vst "is just as necessary as ")
    (Vst "is the clue to the discovery of ")
    (Vst "is a representation of ")
    (Vst "depends on ")
    (Vst "excludes the possibility of ")
    (Vsi "is the mere result of the power of "
         Ns
         ", a blind but indispensable function\nof the soul ")
    (Vsi "occupies part of the sphere of "
         Ns
         "concerning the existence of "
         Np
         "in general ")
    (Vsi "is by its very nature contradictory ")
    (Vsi "would be falsified ")
    (Vsi "abstracts from all content of " knowledge-type "knowledge ")
    (Vsi "is a body of demonstrated doctrine, and "
         quantity
         "it must be known\n"
         knowledge-type)
    (Vsi "can never furnish a true and demonstrated science, because, like "
         Ns
         ", it\n"
         Vst
         judgement-type
         "principles ")
    (Vsi "can be treated like " NsNp)
    (Vsi "shoudl only be used as a canon for " NsNp)
    (Vsi "exists in " NsNp)
    (Vpi "are the mere results of the power of "
         Ns
         ", a blind but indispensible\nfunction of the soul ")
    (Vpi "occupy part of the sphere of "
         Ns
         "concerning the existence of "
         Np
         "in general ")
    (Vpi "are by their very nature contradictory ")
    (Vpi "would be falsified ")
    (Vpi "abstract from all content of " knowledge-type "knowledge ")
    (Vpi "constitute a body of demonstrated doctrine, and "
         quantity
         "this body must\nbe known "
         knowledge-type)
    (Vpi "can never, as a whole, furnish a true and demonstrated science, because,\nlike "
         Ns
         ", they "
         Vpt
         judgement-type
         "principles ")
    (Vpi "can be treated like " NsNp)
    (Vpi "should only be used as a canon for " NsNp)
    (Vpi "exist in " NsNp)
    (throwaway-sentence
     "But we have fallen short of the necessary interconnection that we have in\nmind when we speak of "
     NsNp
     ". ")
    (throwaway-sentence "We thus have a pure synthesis of apprehension. ")
    (throwaway-sentence "And similarly with all the others. ")
    (throwaway-sentence
     "The question of this matter's relation to objects is not in any way under\ndiscussion. ")
    (throwaway-sentence
     "This distinction must have some ground in the nature of "
     NsNp
     ". ")
    (throwaway-sentence
     "The divisions are thus provided, all that is required is to fill them. ")
    (throwaway-sentence
     "This could not be passed over in a complete system of transcendental\nphilosophy, but in a merely critical essay the simple mention of the fact\nmay suffice. ")
    (throwaway-sentence
     "This is not something we are in a position to establish. ")
    (throwaway-sentence
     "This is the sense in which it is to be understood in this work. ")
    (throwaway-sentence "But this need not worry us. ")
    (throwaway-sentence "Let us apply this to " Ns ". ")
    (throwaway-sentence "But to this matter no answer is possible. ")
    (throwaway-sentence
     "But the proof of this is a task from which we can here be absolved. ")
    (throwaway-sentence
     "But at present we shall turn our attention to "
     Ns
     ". ")
    (throwaway-sentence "This may become clear with an example. ")
    (throwaway-sentence "I feel I have sufficiently shown this to be true. ")
    (throwaway-sentence "This is what chiefly concerns us. ")
    (throwaway-sentence
     "On this matter, what has been said already should in any case suffice by\nitself. ")
    (throwaway-sentence
     "In my present remarks I am referring to "
     Ns
     "only in so far as it is\nfounded on "
     judgement-type
     "principles. ")
    (throwaway-sentence "But this is to be dismissed as random grouping. ")))
