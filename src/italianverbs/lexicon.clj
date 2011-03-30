(ns italianverbs.lexicon
  (:use [hiccup core page-helpers]
	[somnium.congomongo]
	[italianverbs.grammar])
  (:require
   [clojure.string :as string]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.morphology :as morphology]
   [italianverbs.grammar :as grammar]
   [clojure.contrib.str-utils2 :as str-utils]))

;; useful abbreviations
(def noun
  {:cat :noun
   :person :3rd
   :genfn "np-det"})
(def pronoun
  {:cat :noun
   :human true
   :genfn "np-no-det"})
(def nominative
  {:case :nom})
(def accusative
  {:case :acc})
(def propernoun
  {:person :3rd
   :genfn "np-no-det"})
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

;; not used yet; just considering...
(def transitive-human-verb-with-edible-obj-and-benefactive-pp
  ;; e.g. "cook" as in "the man cooked dinner for the woman."
  {:fn (fn [verb obj]
         (combine verb obj 'left
                  {:fn (fn [vp subj]
                         (combine subj vp 'right
                                  {:fn (fn [sentence pp]
                                         (combine sentence pp 'left))
                                   :arg {:cat :prep
                                         :comp {:cat :noun
                                                :animate true}
                                         :benefactive true}})) 
                   :arg {:cat :noun
                         :human true}}))
   :arg {:cat :noun
         :edible true}})

;; WARNING: clear blows away entire lexicon in backing store (mongodb).
(lexfn/clear)

;; BEGIN LEXICON

;; prepositions
(lexfn/add "in" "in"
	   {:cat :prep
	    :fn "prep-fn"})

(lexfn/add "a" "to"
	   {:cat :prep
	    :fn "prep-fn"})


;; verbs
(lexfn/add "dimenticare" "to forget"
           {:cat :verb :infl :infinitive
            :subj {:animate true}
            :obj {:cat :noun}})

(lexfn/add "agitare" "to shake"
           {:cat :verb :infl :infinitive
            :subj {:cat :noun}
            :obj {:holdable true}})

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

(def venire (lexfn/add "venire" "to come"
                       {:cat :verb :infl-omit :infinitive :fn "verb-sv"}
                       (list choose-pp)))
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

(lexfn/add "scrivere" "to write"
           {:cat :verb :infl :infinitive
            :subj {:human true}
            :obj {:writable true}})
;           (list choose-pp))

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
              :adjunct {:place true}
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
            :obj {:cat :noun}})

(lexfn/add "pranzare" "to eat lunch"
           {:cat :verb :infl :infinitive
            :subj {:human true}})

(def andare
  (lexfn/add "andare" "to go"
             {:cat :verb :infl-omit :infinitive}
             (list choose-pp)))
;; exceptions
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
(lexfn/add "tu" "you" {:person :2nd :number :singular :cat :noun} (list pronoun))
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
           {:place true}
           (list sing propernoun noun))

(lexfn/add "Firenze" "Florence" 
           {:place true}
           (list sing propernoun noun))

(lexfn/add "Napoli" "Naples" 
           {:place true}
           (list sing propernoun noun))


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
(if true
  (do
(lexfn/add "uomo" "man"
	    {:cat :noun
	     :number :singular
	     :gender :masc
	     :fn "noun-fn"}
        (list noun human))
	     
(lexfn/add "donna" "woman"
	    {:cat :noun
	     :number :singular
	     :gender :fem
	     :fn "noun-fn"}
        (list noun human))

(lexfn/add "cane" "dog"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :animate true
	     :fn "noun-fn"}
        (list noun))

(lexfn/add "pane" "bread"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :artifact true
         :edible true
         :holdable true
	     :fn "noun-fn"}
        (list noun))

(lexfn/add "pasta" "pasta"
	    {:cat :noun
	     :number :singular
	     :gender :fem
         :makeable true
         :edible true
         :holdable true
	     :fn "noun-fn"}
        (list noun))

(lexfn/add "libro" "book"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :artifact true
         :written true
         :holdable true
         :person :3rd
	     :fn "noun-fn"}
        (list noun))

(lexfn/add "gamba" "leg"
	    {:cat :noun
	     :number :singular
	     :gender :fem
         :person :3rd
	     :fn "noun-fn"
         :body-part true
         }
        (list noun))

(lexfn/add "giornale" "newspaper"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :person :3rd
	     :fn "noun-fn"
         :artifact true
         :written true}
        (list noun))

(lexfn/add "abito" "dress"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :artifact true
	     :fn "noun-fn"}
        (list noun))

(lexfn/add "parole" "word"
	    {:cat :noun
	     :number :plural
         :sayable true
         :writable true
	     :gender :fem
	     :fn "noun-fn"}
        (list noun))

))

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


