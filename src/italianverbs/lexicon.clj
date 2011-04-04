(ns italianverbs.lexicon
  (:use [hiccup core page-helpers]
	[somnium.congomongo])
  (:require
   [clojure.string :as string]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.morphology :as morphology]
   [italianverbs.grammar :as gram]
   [clojure.contrib.str-utils2 :as str-utils]))

;; useful abbreviations
(def noun
  {:cat :noun
   :person :3rd
   :genfn "gram/np-det"})
(def pronoun
  {:cat :noun
   :animate true
   :human true
   :genfn "gram/np-no-det"})
(def nominative
  {:case :nom})
(def accusative
  {:case :acc})
(def propernoun
  (merge noun
         {:genfn "gram/np-no-det"}))
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
(def choose-pp
  {:genfn "choose-pp"})
(def choose-vp-inf
  {:genfn "choose-vp-inf"})
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
(lexfn/clear)

;; BEGIN LEXICON

;; prepositions
(lexfn/add "in" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :andare-in true}})

(lexfn/add "a" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :andare-a true}})

(lexfn/add "al" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :andare-al true}})

(lexfn/add "da" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :human true}})

(lexfn/add "da" "from"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :place true}})

(lexfn/add "a" "to"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :animate true}})

(lexfn/add "con" "with"
	   {:cat :prep
	    :fn "gram/prep-fn"
        :obj {:case {:$ne :nom}
              :human true}})

;; verbs
(lexfn/add "dimenticare" "to forget"
           {:cat :verb :infl :infinitive
            :subj {:animate true}
            :obj {:cat :noun}})

(lexfn/add "agitare" "to shake"
           {:cat :verb :infl :infinitive
            :subj {
                   :animate true
                   }
            :obj {:holdable true}})

(lexfn/add "mostrare" "to show"
           {:cat :verb :infl :infinitive
            :subj {
                   :human true
                   }
            :obj {:cat :noun}
            :iobj {:animate true}})
           

(def dire (lexfn/add "dire" "to say"
                     {:cat :verb :infl :infinitive
                      :obj {:sayable true}
                      :iobj {:animate true}
                      :subj {:human true}}))
(lexfn/add-infl "dico" (list firstp sing present
		       {:root dire}))
(lexfn/add-infl "dici" (list secondp sing present
		       {:root dire}))
(lexfn/add-infl "dice" (list thirdp sing present
		       {:root dire}))
(lexfn/add-infl "diciamo" (list firstp plural present
		       {:root dire}))
(lexfn/add-infl "dite" (list secondp plural present
		       {:root dire}))
(lexfn/add-infl "dicono" (list thirdp plural present
		       {:root dire}))

(lexfn/add "scrivere" "to write"
           {:cat :verb :infl :infinitive
            :subj {:human true}
            :obj {:writable true}
            :iobj {:human true
                   :prep {:benefactive true}}})

(lexfn/add "correggere" "to correct"
           {:cat :verb :infl :infinitive
            :subj {:human true}
            :obj {:human true}})

(lexfn/add "leggere" "to read"
           {:cat :verb
            :infl :infinitive
            :subj {:human true}
            :obj {:written true}
            :iobj {:human true}
            :adjunct {:place true}})

(def mangiare
  (lexfn/add "mangiare" "to eat"
             {:cat :verb
              :subj {:animate true}
              :obj {:edible true}
              :adjunct {:cat :prep
                        :obj.place true}
              :infl :infinitive}))


;; FIXME: hacks until italian morphology works better: mangiare
;; is a regular -are verb.
(lexfn/add "mangi" "to eat"
	    {:root mangiare
	     :cat :verb :infl :present
	     :person :2nd :number :singular})
(lexfn/add "mangia" "to eat"
	    {:root mangiare
	     :cat :verb :infl :present
	     :person :3rd :number :singular})

(lexfn/add "parlere" "to speak"
           {:cat :verb :infl-omit :infinitive}
           (list choose-pp))

(lexfn/add "smettere" "to quit"
           {:cat :verb :infl :infinitive
            :subj {:human true}
            :obj.cat :noun})

(lexfn/add "pranzare" "to eat lunch"
           {:cat :verb
            :infl :infinitive
            :subj {:human true}
            :adjunct {:cat :prep
                      :obj.place true}}) ;; e.g. "[eats lunch [in [ the cafe ]]]"
           
;; <andare root>
(def andare
  (lexfn/add "andare" "to go"
             {:cat :verb :infl :supertype
              :subj {:animate true}}))

;; <andare adjunct variants> 
(lexfn/add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "a"
                             :obj.andare-a true}}))
(lexfn/add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "in"
                             :obj.andare-in true}}))
(lexfn/add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "al"
                             :obj.andare-al true}}))
(lexfn/add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "da"
                             :obj.human true}}))
;; </andare root variants>

;; <andare exceptions>
(lexfn/add-infl "vado" (list firstp sing present)
	  {:root andare})
(lexfn/add-infl "vai" (list secondp sing present)
	  {:root andare})
(lexfn/add-infl "va" (list thirdp sing present)
	  {:root andare})
(lexfn/add-infl "andiamo" (list firstp plural present)
	  {:root andare})
(lexfn/add-infl "andate" (list secondp plural present)
	  {:root andare})
(lexfn/add-infl "vanno" (list thirdp plural present)
	  {:root andare})
;; </andare exceptions>
;; </andare>

;; <venire>

(def venire
  (lexfn/add "venire" "to come"
             {:cat :verb :infl :supertype
              :subj {:animate true}}))

;; <venire adjunct variants>
;; TODO : add and use (lexfn/add-variant)
;; (which doesn't need the english repeated,
;; and uses just the defined symbol venire,
;; not the string "venire".
;; come *to* a place or by means of something ("vengo in treno")
(lexfn/add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "in"
                             :obj.andare-in true}}))
;; come *to* a place.
(lexfn/add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "a"
                             :obj.andare-a true}}))
;; come *from* a place.
(lexfn/add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "da"
                             :obj.place true}}))
;; come *to* a person.
(lexfn/add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "da"
                             :obj.human true}}))

;; </venire adjunct variants>

 ;; <venire exceptions>
 (lexfn/add-infl "vengo" (list firstp sing present
		       {:root venire}))
(lexfn/add-infl "vieni" (list secondp present
		       {:root venire}))
(lexfn/add-infl "viene" (list thirdp sing present
		       {:root venire}))
(lexfn/add-infl "veniamo" (list firstp plural present
		       {:root venire}))
(lexfn/add-infl "venite" (list secondp plural present
		       {:root venire}))
(lexfn/add-infl "vengono" (list thirdp plural present
		       {:root venire}))
;; </venire exceptions>

;; </venire>

(def volare (lexfn/add "volare" "to want"
                       {:cat :verb :infl :infinitive-omit-me}
                       (list choose-vp-inf)))
                       
(lexfn/add-infl "voglio" (list firstp sing present
			 {:root volare}))
(lexfn/add-infl "vogli" (list secondp sing present
			{:root volare}))
(lexfn/add-infl "voglie" (list thirdp sing present
			 {:root volare}))
(lexfn/add-infl "vogliamo" (list firstp plural sing present
			 {:root volare}))
(lexfn/add-infl "vogliete" (list secondp plural plural present
			 {:root volare}))
(lexfn/add-infl "vogliono" (list thirdp plural plural present
			 {:root volare}))

(def fare (lexfn/add "fare" "to make"
                     {:cat :verb :infl :infinitive
                      :obj {:artifact true}
                      :subj {:human true}
                      :iobj {:animate true
                             :benifactive true}}))
                     
(lexfn/add-infl "facio" (list firstp sing present
			{:root fare}))
(lexfn/add-infl "fai" (list secondp sing present
		      {:root fare}))
(lexfn/add-infl "fà" (list thirdp sing present
		      {:root fare}))
(lexfn/add-infl "facciamo" (list firstp plural sing present
			   {:root fare}))
(lexfn/add-infl "fate" (list secondp plural plural present
			 {:root fare}))
(lexfn/add-infl "fanno" (list thirdp plural plural present
			 {:root fare}))

;; pronouns

(lexfn/add "io" "i" {:person :1st :number :singular :cat :noun} (list pronoun nominative))
(lexfn/add "tu" "you" {:person :2nd :number :singular :cat :noun :case :nom}
           (list pronoun))
(lexfn/add "te" "you" {:person :2nd :number :singular :cat :noun :case :acc}
           (list pronoun))
(lexfn/add "lui" "he" {:person :3rd :number :singular :cat :noun :gender :masc} (list pronoun nominative))
(lexfn/add "lei" "she" {:person :3rd :number :singular :cat :noun :gender :fem} (list pronoun nominative))
(lexfn/add "noi" "we" {:person :1st :number :plural :cat :noun} (list pronoun nominative))
(lexfn/add "voi" "you all" {:person :2nd :number :plural :cat :noun} (list pronoun))
(lexfn/add "loro" "they" {:person :3rd :number :plural :cat :noun} (list pronoun nominative))

(lexfn/add "io" "me" {:person :1st :number :singular :cat :noun} (list pronoun accusative))
(lexfn/add "lui" "him" {:person :3rd :number :singular :cat :noun :gender :masc} (list pronoun accusative))
(lexfn/add "lei" "her" {:person :3rd :number :singular :cat :noun :gender :fem} (list pronoun accusative))
(lexfn/add "noi" "us" {:person :1st :number :plural :cat :noun} (list pronoun accusative))
(lexfn/add "loro" "them" {:person :3rd :number :plural :cat :noun} (list pronoun accusative))

;; Proper nouns
(lexfn/add "Italia" "Italy" 
           {}
           (list sing propernoun region))

(lexfn/add "Spagna" "Spain" 
           {}
           (list sing propernoun region))

(lexfn/add "Sicily" "Sicily" 
           {}
           (list sing propernoun region))

(lexfn/add "Firenze" "Florence" 
           {}
           (list sing propernoun city))

(lexfn/add "Napoli" "Naples" 
           {}
           (list sing propernoun city))


;; determiners
(lexfn/add "il" "the" {:gender :masc :number :singular :cat :det
			:def :def})
(lexfn/add "i" "the" {:gender :masc :number :plural :cat :det
		       :def :def})
(lexfn/add "gli" "the" {:gender :masc :number :plural :cat :det
			 :def :def})

(lexfn/add "la" "the" {:gender :fem :number :singular :cat :det
			:def :def})
(lexfn/add "le" "the" {:gender :fem :number :plural :cat :det
			:def :def})

;; nouns
(lexfn/add "uomo" "man"
	    {:cat :noun
	     :number :singular
	     :gender :masc}
        (list noun human))
	     
(lexfn/add "donna" "woman"
	    {:cat :noun
	     :number :singular
	     :gender :fem}
        (list noun human))

(lexfn/add "raggazo" "guy"
	    {:cat :noun
	     :number :singular
	     :gender :masc}
        (list noun human))
	     
(lexfn/add "raggaza" "girl"
	    {:cat :noun
	     :number :singular
	     :gender :fem}
        (list noun human))

(lexfn/add "cane" "dog"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :animate true}
        (list noun))

(lexfn/add "pane" "bread"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :artifact true
         :edible true
         :holdable true}
        (list noun))

(lexfn/add "pasta" "pasta"
	    {:cat :noun
	     :number :singular
	     :gender :fem
         :makeable true
         :edible true
         :holdable true}
        (list noun))

(lexfn/add "libro" "book"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :artifact true
         :written true
         :holdable true
         :person :3rd}
        (list noun))

(lexfn/add "gamba" "leg"
	    {:cat :noun
	     :number :singular
	     :gender :fem
         :person :3rd
         :body-part true}
        (list noun))

(lexfn/add "giornale" "newspaper"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :person :3rd
         :artifact true
         :written true}
        (list noun))

(lexfn/add "abito" "dress"
	    {:number :singular
	     :gender :masc
         :artifact true}
        (list noun))

(lexfn/add "parole" "word"
	    {:number :plural
         :sayable true
         :writable true
	     :gender :fem}
        (list noun))

(lexfn/add "centro" "downtown"
           {:andare-in true
            :cat :noun
            :genfn "gram/np-no-det"})

(lexfn/add "ufficio" "the office" ;; TODO: better english translation might be "my office","your office", etc, or in some cases "work".
           {:andare-in true
            :cat :noun
            :genfn "gram/np-no-det"})

(lexfn/add "casa" "home"
           {:andare-a true
            :cat :noun
            :genfn "gram/np-no-det"})

(lexfn/add "letto" "bed"
           {:andare-a true
            :cat :noun
            :genfn "gram/np-no-det"})

(lexfn/add "cinema" "the theatre"
           {:andare-al true
            :cat :noun
            :genfn "gram/np-no-det"})

(lexfn/add "mare" "the beach"
           {:andare-al true
            :cat :noun
            :genfn "gram/np-no-det"})

(lexfn/add "ristorante" "the restaurant"
           {:andare-al true
            :cat :noun
            :genfn "gram/np-no-det"})

;; adjectives
(lexfn/add "bianco" "white"
	    {:cat :adjective})
(lexfn/add "nero" "black"
	    {:cat :adjective})
(lexfn/add "forte" "strong"
	    {:cat :adjective})

(lexfn/add "sinistra" "left"
	    {:cat :adjective})
(lexfn/add "destra" "right"
	    {:cat :adjective})

;; sentences

(lexfn/add "ha gli occhi azzuri" "he has blue eyes"
	    {:person "3rd" :number :singular :cat :verb})
(lexfn/add "ha i cappelli non molte lunghi" "he has not very long hair"
	    {:person "3rd" :number :singular :cat :verb})
(lexfn/add "ha il naso alla francese" "he has a french nose" 
	    {:person "3rd" :number :singular :cat :verb})
(lexfn/add "non lo so" "i don't know"
	    {:cat :verb})
(lexfn/add "come sono?" "how?"
	    {:cat :verb})
(lexfn/add "cosa fa?" "what?"
	    {:cat :verb})

;; adjectives
(lexfn/add "alto" "tall"
	    {:cat :adjective})
(lexfn/add "basso" "short"
	    {:cat :adjective})
(lexfn/add "giovano" "young"
	    {:cat :adjective})
(lexfn/add "anziano" "old"
	    {:cat :adjective})
(lexfn/add "margra" "lean"
	    {:cat :adjective})
(lexfn/add "grasso" "fat"
	    {:cat :adjective})
(lexfn/add "bello" "beautiful"
	    {:cat :adjective})
(lexfn/add "bruto" "ugly"
	    {:cat :adjective})
(lexfn/add "carino" "cute"
	    {:cat :adjective})
(lexfn/add "lunghi" "long"
	    {:cat :adjective})
(lexfn/add "corti" "short"
	    {:cat :adjective})
(lexfn/add "calvo" "bald"
	    {:cat :adjective})
(lexfn/add "bruno" "brown"
	    {:cat :adjective})
(lexfn/add "bianchi" "white"
	    {:cat :adjective})
(lexfn/add "di mezza eta" "middle-aged"
	    {:cat :adjective})
(lexfn/add "qui" "here"
	   {:cat :adjective})


