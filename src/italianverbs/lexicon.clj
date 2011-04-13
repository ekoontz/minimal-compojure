(ns italianverbs.lexicon
  (:use [italianverbs.lexiconfn]))
;; useful abbreviations
(def noun
  {:cat :noun
   :det {:cat :det}
   :person :3rd})

(def mass-noun
  {:det {:cat :det
         :def :def}})

(def pronoun
  {:cat :noun
   :animate true
   :human true
   :det nil}) ;; pronouns don't take a determiner.

(def nominative
  {:case :nom})
(def accusative
  {:case :acc})
(def propernoun
  (merge noun
         {:det nil})) ;; propernouns also don't take a determiner.
(def firstp
  {:person :1st})
(def secondp
  {:person :2nd})
(def thirdp
  {:person :3rd})
(def sing
  {:number :singular})
(def plural
  {:number :plural})
(def present
  {:cat :verb :infl :present})
(def choose-vp-inf
  {:obj {:cat :verb}})
(def human
  {:human true
   :animate true})
(def place
  {:place true})
(def city
  (merge place
         {:andare-a true}))
(def region
  (merge place
         {:andare-in true}))

;; WARNING: clear blows away entire lexicon in backing store (mongodb).
(clear)

;; BEGIN LEXICON

;; prepositions
(add "in" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :andare-in true}})

(add "in" "in"
     {:cat :prep
      :action-occurring-in true
      :fn "gram/prep-fn"
      :obj {:case {:$ne :nom}
            :english-in true
            :place true}})

(add "in" "at"
     {:cat :prep
      :action-occurring-in true
      :fn "gram/prep-fn"
      :obj {:case {:$ne :nom}
            :english-at true
            :place true}})

(add "a" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :andare-a true}})

(add "al" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :andare-al true}})

(add "da" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :human true}})

(add "da" "from"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :place true}})

(add "a" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :animate true}})

(add "con" "with"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :human true}})

(add "per" "for"
     {:cat :prep
      :benefactive true
      :fn "gram/prep-fn"
      :obj {:case {:$ne :nom}
            :animate true}})

(add "per" "for"
     {:cat :prep
      :benefactive true
      :fn "gram/prep-fn"
      :obj {:case {:$ne :nom}
            :human true}})

;; verbs
(add "dimenticare" "to forget"
           {:cat :verb :infl :infinitive
            :subj {:animate true}
            :obj {:cat :noun}})

;; FIXME: should also allow "at".
(def adjunct-in-a-place
  {:action-occurring-in true
   :obj.place true})

(add "agitare" "to shake"
           {:cat :verb :infl :infinitive
            :subj {:animate true}
            :obj {:cat :noun :holdable true}
            :adjunct adjunct-in-a-place})

(add "mostrare" "to show"
           {:cat :verb :infl :infinitive
            :subj {
                   :human true
                   }
            :obj {:cat :noun}
            :iobj {:obj.animate true}
            :adjunct adjunct-in-a-place})

(def dire (add "dire" "to say"
                     {:cat :verb :infl :infinitive
                      :obj {:cat :noun :sayable true}
                      :iobj {:obj.animate true
                             :benefactive true}
                      :subj {:human true}
                      :adjunct adjunct-in-a-place}))

(add-infl "dico" (list firstp sing present
		       {:root dire}))
(add-infl "dici" (list secondp sing present
		       {:root dire}))
(add-infl "dice" (list thirdp sing present
		       {:root dire}))
(add-infl "diciamo" (list firstp plural present
		       {:root dire}))
(add-infl "dite" (list secondp plural present
		       {:root dire}))
(add-infl "dicono" (list thirdp plural present
		       {:root dire}))

(add "scrivere" "to write"
           {:cat :verb :infl :infinitive
            :subj {:human true}
            :obj {:writable true
                  :cat :noun}
            :iobj {:obj.human true
                   :benefactive true}
            :adjunct adjunct-in-a-place})

(add "correggere" "to correct"
           {:cat :verb :infl :infinitive
            :subj {:human true}
            :obj {:cat :noun :human true}
            :adjunct adjunct-in-a-place})

(add "leggere" "to read"
           {:cat :verb
            :infl :infinitive
            :subj {:human true}
            :obj {:cat :noun :written true}
            :iobj {:obj.case {:$ne :nom}
                   :obj.human true}
           :adjunct adjunct-in-a-place})

(def mangiare
  (add "mangiare" "to eat"
             {:cat :verb
              :subj {:animate true}
              :obj {:cat :noun
                    :edible true}
              :adjunct {:cat :prep
                        :obj.place true}
              :infl :infinitive}))


;; FIXME: hacks until italian morphology works better: mangiare
;; is a regular -are verb.
(add "mangi" "to eat"
	    {:root mangiare
	     :cat :verb :infl :present
	     :person :2nd :number :singular})
(add "mangia" "to eat"
     {:root mangiare
      :cat :verb :infl :present
      :person :3rd :number :singular})

(add "parlere" "to speak"
     {:subj {:human true}
      :cat :verb :infl :infinitive
      :iobj {:obj.human true}
      :adjunct adjunct-in-a-place})

(add "smettere" "to quit"
     {:cat :verb :infl :infinitive-omit
      :subj {:human true}
      :obj {:cat :noun}
      :adjunct adjunct-in-a-place})
;; someday: (e.g. "quits working at the bank")
;; :obj vp[:tense present-participle]
 
(add "pranzare" "to eat lunch"
           {:cat :verb
            :infl :infinitive
            :subj {:human true}
            :adjunct adjunct-in-a-place}) ;; e.g. "[eats lunch [in [ the cafe ]]]"
           
;; <andare root>
(def andare
  (add "andare" "to go"
             {:cat :verb :infl :supertype
              :subj {:animate true}}))

;; <andare adjunct variants> 
(add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "a"
                             :obj.andare-a true}}))
(add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "in"
                             :obj.andare-in true}}))
(add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "al"
                             :obj.andare-al true}}))
(add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "da"
                             :obj.human true}}))
;; </andare root variants>

;; <andare exceptions>
(add-infl "vado" (list firstp sing present)
	  {:root andare})
(add-infl "vai" (list secondp sing present)
	  {:root andare})
(add-infl "va" (list thirdp sing present)
	  {:root andare})
(add-infl "andiamo" (list firstp plural present)
	  {:root andare})
(add-infl "andate" (list secondp plural present)
	  {:root andare})
(add-infl "vanno" (list thirdp plural present)
	  {:root andare})
;; </andare exceptions>
;; </andare>

;; <venire>

(def venire
  (add "venire" "to come"
             {:cat :verb :infl :supertype
              :subj {:animate true}}))

;; <venire adjunct variants>
;; TODO : add and use (add-variant)
;; (which doesn't need the english repeated,
;; and uses just the defined symbol venire,
;; not the string "venire".
;; come *to* a place or by means of something ("vengo in treno")
(add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "in"
                             :obj.andare-in true}}))
;; come *to* a place.
(add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "a"
                             :obj.andare-a true}}))
;; come *from* a place.
(add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "da"
                             :obj.place true}}))
;; come *to* a person.
(add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "da"
                             :obj.human true}}))

;; </venire adjunct variants>

 ;; <venire exceptions>
 (add-infl "vengo" (list firstp sing present
		       {:root venire}))
(add-infl "vieni" (list secondp present
		       {:root venire}))
(add-infl "viene" (list thirdp sing present
		       {:root venire}))
(add-infl "veniamo" (list firstp plural present
		       {:root venire}))
(add-infl "venite" (list secondp plural present
		       {:root venire}))
(add-infl "vengono" (list thirdp plural present
		       {:root venire}))
;; </venire exceptions>

;; </venire>

(def volare (add "volare" "to want"
                 {:cat :verb :infl :infinitive
                  :subj {:animate true}}
                 (list choose-vp-inf)))
                       
(add-infl "voglio" (list firstp sing present
			 {:root volare}))
(add-infl "vogli" (list secondp sing present
			{:root volare}))
(add-infl "voglie" (list thirdp sing present
			 {:root volare}))
(add-infl "vogliamo" (list firstp plural sing present
			 {:root volare}))
(add-infl "vogliete" (list secondp plural plural present
			 {:root volare}))
(add-infl "vogliono" (list thirdp plural plural present
			 {:root volare}))

(def fare (add "fare" "to make"
                     {:cat :verb :infl :infinitive
                      :obj {:cat :noun
                            :artifact true}
                      :subj {:human true}
                      :iobj {:obj.animate true
                             :benefactive true}
                      :adjunct adjunct-in-a-place}))
                     
(add-infl "facio" (list firstp sing present
			{:root fare}))
(add-infl "fai" (list secondp sing present
		      {:root fare}))
(add-infl "fà" (list thirdp sing present
		      {:root fare}))
(add-infl "facciamo" (list firstp plural sing present
			   {:root fare}))
(add-infl "fate" (list secondp plural plural present
			 {:root fare}))
(add-infl "fanno" (list thirdp plural plural present
			 {:root fare}))

;; pronouns
(add "io" "i" {
               :person :1st :number :singular :cat :noun
               } (list pronoun nominative))
(add "tu" "you" {:person :2nd :number :singular :cat :noun :case :nom}
           (list pronoun))
(add "te" "you" {:person :2nd :number :singular :cat :noun :case :acc}
           (list pronoun))
(add "lui" "he" {:person :3rd :number :singular :cat :noun :gender :masc} (list pronoun nominative))
(add "lei" "she" {:person :3rd :number :singular :cat :noun :gender :fem} (list pronoun nominative))
(add "noi" "we" {:person :1st :number :plural :cat :noun} (list pronoun nominative))
(add "voi" "you all" {:person :2nd :number :plural :cat :noun} (list pronoun))
(add "loro" "they" {:person :3rd :number :plural :cat :noun} (list pronoun nominative))

(add "io" "me" {
                :person :1st :number :singular :cat :noun} (list pronoun accusative))
(add "lui" "him" {:person :3rd :number :singular :cat :noun :gender :masc} (list pronoun accusative))
(add "lei" "her" {:person :3rd :number :singular :cat :noun :gender :fem} (list pronoun accusative))
(add "noi" "us" {:person :1st :number :plural :cat :noun} (list pronoun accusative))
(add "loro" "them" {:person :3rd :number :plural :cat :noun} (list pronoun accusative))

;; Proper nouns
(add "Italia" "Italy" 
           {}
           (list sing propernoun region))

(add "Spagna" "Spain" 
           {}
           (list sing propernoun region))

(add "Sicily" "Sicily" 
           {}
           (list sing propernoun region))

(add "Firenze" "Florence" 
           {}
           (list sing propernoun city))

(add "Napoli" "Naples" 
           {}
           (list sing propernoun city))


;; determiners
(add "il" "the" {:gender :masc :number :singular :cat :det
			:def :def})
(add "uno" "a" {:gender :masc :number :singular :cat :det
			:def :indef})
(add "i" "the" {:gender :masc :number :plural :cat :det
		       :def :def})
(add "gli" "the" {:gender :masc :number :plural :cat :det
			 :def :def})

(add "la" "the" {:gender :fem :number :singular :cat :det
			:def :def})
(add "una" "a" {:gender :fem :number :singular :cat :det
			:def :indef})
(add "le" "the" {:gender :fem :number :plural :cat :det
			:def :def})

;; nouns
(add-with-plural "uomo" "man"
  {:cat :noun
   :number :singular
   :gender :masc}
  (list noun human)
  "men")

(add-with-plural "donna" "woman"
  {:cat :noun
   :number :singular
   :gender :fem}
  (list noun human)
  "women")

(add-with-plural "raggazzo" "guy"
  {:cat :noun
   :number :singular
   :gender :masc}
  (list noun human))
	     
(add-with-plural "raggazza" "girl"
  {:cat :noun
   :number :singular
   :gender :fem}
  (list noun human))

(add "cane" "dog"
     {:cat :noun
      :number :singular
      :gender :masc
      :animate true}
     (list noun))

(add "pane" "bread"
     {:cat :noun
      :number :singular
      :gender :masc
      :artifact true
      :edible true
      :holdable true}
     (list noun))

(add "pasta" "pasta"
	    {:cat :noun
	     :number :singular
	     :gender :fem
         :makeable true
         :edible true
         :det {:def :def}
         :holdable true}
        (list mass-noun noun))

(add-with-plural "libro" "book"
     {:cat :noun
      :number :singular
      :gender :masc
      :artifact true
      :written true
      :holdable true
      :person :3rd}
     (list noun))

(add-with-plural "gamba" "leg"
  {:cat :noun
   :number :singular
   :gender :fem
   :person :3rd
   :body-part true}
  (list noun))

(add "giornale" "newspaper"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :person :3rd
         :artifact true
         :written true}
        (list noun))

(add-with-plural "abito" "dress"
  {:number :singular
   :gender :masc
   :artifact true}
  (list noun))

(add-with-plural "parole" "word"
  {:number :plural
   :sayable true
   :writable true
   :gender :fem}
  (list noun))

(add "centro" "downtown"
           {:andare-in true
            :cat :noun
            :det nil})

(add "ufficio" "the office" ;; TODO: better english translation might be "my office","your office", etc, or in some cases "work".
           {:andare-in true
            :cat :noun
            :place true
            :det nil})

(add "casa" "home"
           {:andare-a true
            :cat :noun
            :english-at true
            :place true
            :det nil})

(add "letto" "bed"
     {:andare-a true
      :english-in true
      :place true
      :cat :noun
      :det nil})

(add "cinema" "the theatre"
     {:andare-al true
      :place true
      :english-in true
      :cat :noun
      :det nil})

(add "mare" "the beach"
     {:andare-al true
      :place true
      :english-at true
      :cat :noun
      :det nil})

(add "ristorante" "the restaurant"
     {:andare-al true
      :cat :noun
      :english-at true
      :english-in true
      :place true
      :det nil})

(add "salute" "health"
     {:cat :noun
      :det nil})

;; adjectives
(add "bianco" "white"
	    {:cat :adjective})
(add "nero" "black"
	    {:cat :adjective})
(add "forte" "strong"
	    {:cat :adjective})

(add "sinistra" "left"
	    {:cat :adjective})
(add "destra" "right"
	    {:cat :adjective})

;; sentences

(add "ha gli occhi azzuri" "he has blue eyes"
	    {:person "3rd" :number :singular :cat :verb})
(add "ha i cappelli non molte lunghi" "he has not very long hair"
	    {:person "3rd" :number :singular :cat :verb})
(add "ha il naso alla francese" "he has a french nose" 
	    {:person "3rd" :number :singular :cat :verb})
(add "non lo so" "i don't know"
	    {:cat :verb})
(add "come sono?" "how?"
	    {:cat :verb})
(add "cosa fa?" "what?"
	    {:cat :verb})

;; adjectives
(add "alto" "tall"
	    {:cat :adjective})
(add "basso" "short"
	    {:cat :adjective})
(add "giovano" "young"
	    {:cat :adjective})
(add "anziano" "old"
	    {:cat :adjective})
(add "margra" "lean"
	    {:cat :adjective})
(add "grasso" "fat"
	    {:cat :adjective})
(add "bello" "beautiful"
	    {:cat :adjective})
(add "bruto" "ugly"
	    {:cat :adjective})
(add "carino" "cute"
	    {:cat :adjective})
(add "lunghi" "long"
	    {:cat :adjective})
(add "corti" "short"
	    {:cat :adjective})
(add "calvo" "bald"
	    {:cat :adjective})
(add "bruno" "brown"
	    {:cat :adjective})
(add "bianchi" "white"
	    {:cat :adjective})
(add "di mezza eta" "middle-aged"
	    {:cat :adjective})
(add "qui" "here"
	   {:cat :adjective})


