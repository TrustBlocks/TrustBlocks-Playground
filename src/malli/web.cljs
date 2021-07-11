(ns ^:figwheel-hooks malli.web
  (:require
    [sci.core]
    [cljsjs.codemirror]
    [cljsjs.codemirror.addon.edit.matchbrackets]
    [cljsjs.codemirror.addon.lint.lint]
    [cljsjs.codemirror.mode.clojure]
    [cljsjs.parinfer]
    [cljsjs.parinfer-codemirror]
    [clojure.string :as str]
    [fipp.clojure :as fipp]
    [goog.functions :as functions]
    [reagent.core :as r]
    [reagent.dom :as rd]
    [edamame.core :as e]
    [malli.core :as m]
    [malli.error :as me]
    [malli.edn :as edn]
    [malli.provider :as mp]
    [malli.generator :as mg]
    [malli.json-schema :as mj]
    [malli.swagger :as ms]
    [malli.util :as mu]
    [malli.registry :as mr]
    [malli.dot :as md])
  (:import [goog Uri]))

(mr/set-default-registry! (merge (m/default-schemas) (mu/schemas)))

(defn read [x]
  (try (e/parse-string x) (catch js/Error _)))

(defn read-schema [x]
  (try (edn/read-string x) (catch js/Error _)))

(defn read-value [x]
  (try
    (let [v (e/parse-string x)]
      (if-not (keyword-identical? v :edamame.impl.parser/eof) v))
    (catch js/Error _)))

(defn trimmed-str [x f]
  (try (str/trim (with-out-str (f x))) (catch js/Error _ "")))

(defn pretty [x] (trimmed-str x fipp/pprint))

(defn inferred [value]
  (if (and value (not (str/blank? value)))
    (try (pretty (mp/provide [(read value)])) (catch js/Error _))
    ""))

(def models
  {:empty {}
   :any? {:schema any?}
   :address {:schema [:map
                      {:title "Postal Address"}
                      [:id string?]
                      [:tags [:set keyword?]]
                      [:address
                       [:map
                        [:streetAddress string?]
                        [:addressLocality string?]
                        [:postalCode string?]
                        [:lonlat [:tuple double? double?]]]]]
             :value {:id "Lillan"
                     :tags #{:artesan :coffee :hotel}
                     :address {:streetAddress "Ahlmanintie 29"
                               :addressLocality "Tampere"
                               :postalCode "33100"
                               :lonlat [61.4858322, 23.7854658]}}}
   :contract-data {:schema [:map 
                            [:contractId uuid?]]
                   :value {:amount {:$class "org.accordproject.money.MonetaryAmount", :doubleValue 1000, :currencyCode "USD"}
                           :date "2018-01-30T00:00:00.000-04:00"
                           :defaultDays 90
                           :lender "Clause Inc."
                           :jurisdiction "New York, NY"
                           :contractId #uuid"6beb3db0-a949-4e2b-bb2e-00e2acd7f0e9"
                           :interestRate 3.8
                           :lenderAddress "246 5th Ave, 3rd Fl, New York, NY 10001"
                           :$identifier "6beb3db0-a949-4e2b-bb2e-00e2acd7f0e9"
                           :$class "org.accordproject.promissorynote.PromissoryNoteContract"
                           :insolvencyDays 90
                           :individual true
                           :maker "Daniel"
                           :principal {:$class "org.accordproject.money.MonetaryAmount", :doubleValue 500, :currencyCode "USD"}
                           :maturityDate "2019-01-20T00:00:00.000-04:00"
                           :makerAddress "1 Main Street"
                           :legalEntity "CORP"}}
   :multi {:schema [:vector
                    [:multi {:dispatch :type}
                     [:sized [:map [:type [:= :sized]] [:size int?]]]
                     [:human [:map [:type [:= :human]] [:name string?] [:address [:map [:street string?]]]]]]]
           :value [{:type :sized, :size 10}
                   {:type :human, :name "tiina", :address {:street "kikka"}}]}
   :order {:schema [:schema
                    {:registry {"Country" [:map {:closed true}
                                           [:name [:enum :FI :PO]]
                                           [:neighbors {:optional true} [:vector [:ref "Country"]]]]
                                "Burger" [:map
                                          [:name string?]
                                          [:description {:optional true} string?]
                                          [:origin [:maybe "Country"]]
                                          [:price pos-int?]]
                                "OrderLine" [:map {:closed true}
                                             [:burger "Burger"]
                                             [:amount int?]]
                                "Order" [:map {:closed true}
                                         [:lines [:vector "OrderLine"]]
                                         [:delivery [:map {:closed true}
                                                     [:delivered boolean?]
                                                     [:address [:map
                                                                [:street string?]
                                                                [:zip int?]
                                                                [:country "Country"]]]]]]}}
                    "Order"]
           :value {:lines [{:burger {:name "NAUGHTY"
                                     :description "Finnish 100% beef patty, cheddar, St Agur blue cheese, bacon jam, rocket, aioli"
                                     :origin {:name :FI}
                                     :price 11}
                            :amount 2}]
                   :delivery {:delivered false
                              :address {:street "Hämeenkatu 10"
                                        :zip 33100
                                        :country {:name :FI
                                                  :neighbors [{:name :PO}]}}}}}
   :user {:schema [:map
                   {:title "User"}
                   [:name string?]
                   [:gender {:optional true} [:enum :male :female :other]]
                   [:email [:re {:description "https://github.com/gfredericks/test.chuck/issues/46"
                                 :gen/fmap '(constantly "random@example.com")
                                 :error/message "should be email"}
                            #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$"]]]
          :value {:name "Tiina"
                  :gender :female
                  :email "tiina@example.com"}} 
   :person {:schema [:map
                     {:person/title "Person"}
                     [:person/identifier string? ]
                     [:person/contracts {:optional true}  [:vector uuid?] ]
                     [:person/additionalName {:optional true} string? ]
                     [:person/gender {:optional true} [:enum :male :female :other :unknown]]
                     [:person/familyName {:optional true} string? ]
                     [:person/givenName {:optional true} string? ]
                     [:person/postalAddress {:optional true} string? ]
                     [:person/birthdate {:optional true} string? ]
                     [:person/email [:re {:description "https://github.com/gfredericks/test.chuck/issues/46"
                                          :gen/fmap '(constantly "random@example.com")
                                          :error/message "should be email"}
                                     #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$"]]]
            :value {:person/identifier "Tom"
                    :person/additionalName "The man"
                    :person/contracts [#uuid"20cca797-3560-441c-b718-ae97807a4678"]
                    :person/gender :male
                    :person/familyName "Brooke"
                    :person/givenName "Thomas"
                    :person/birthdate "1952-02-09T19:20:51Z"
                    :person/email "tom@example.com"
                    :person/postalAddress "Main Street"}}                                                                                            
   :project {:schema [:map
                      [:project/identifier string? ]
                      [:project/blocks [:vector uuid?]]]
          :value {:project/identifier "Brooke and Sons, LLC" 
                  :project/blocks [ #uuid"20cca797-3560-441c-b718-ae97807a4678"
                                    #uuid"43cca745-3990-441c-b718-ae97807a4678"]}}
   :text/hiccup {:schema [:schema {:registry {"hiccup" [:orn

                                                   [:node [:catn
                                                           [:name keyword?]
                                                           [:props [:? [:map-of keyword? any?]]]
                                                           [:children [:* [:schema [:ref "hiccup"]]]]]]
                                                   [:primitive [:orn
                                                                [:nil nil?]
                                                                [:boolean boolean?]
                                                                [:number number?]
                                                                [:text string?]]]]}}
                     "hiccup"]
            :value [:div
                 {}
                 [:markdown/heading {:level 1, :id nil} "PROMISSORY NOTE"]
                 [:p {} "1,000.00 USD 01/30/2018"]
                 [:p
                  {}
                  "FOR VALUE RECEIVED, the undersigned, \"Daniel\", an individual residing at \"1 Main Street\" (“Maker”), hereby promises to pay to the order of \"Clause Inc.\", a CORP, having offices at \"246 5th Ave, 3rd Fl, New York, NY 10001\", or its successors and permitted assigns (“Lender” or the “Company”), the principal sum of 500.00 USD, plus any and all interest accrued thereon at the Note Rate (defined below), each due and payable in cash in lawful money of the United States on the dates and in the manner set forth in this Promissory Note (this “Note”)."]
                 [:markdown/heading {:level 2, :id nil} "Interest."]
                 [:p
                  {}
                  "The principal amount of this Note shall bear interest at 3.8% per annum (the “Note Rate”). Interest shall be computed on the basis of a three hundred and sixty-five (365) day year and charged for the actual number of days elapsed. Interest shall accrue on the original principal balance only and there shall be no accrual of interest upon interest."]
                 [:markdown/heading {:level 2, :id nil} "Payment of Principal and Interest."]
                 [:p
                  {}
                  "The principal amount of this Note and the interest thereon shall be due and payable in full on the earlier of (a) 01/20/2019 or (b) ten (10) days prior to the Company filing an S-1 registration statement with the U.S. Securities and Exchange Commission in contemplation of an initial public offering (“IPO”). As used herein, IPO means the closing of a firm commitment underwritten public offering pursuant to a registration statement under the Securities Act of 1933, as amended."]
                 [:markdown/heading {:level 2, :id nil} "Prepayment."]
                 [:p
                  {}
                  "The Maker may prepay any portion of the principal balance of this Note at any time without penalty."]
                 [:markdown/heading {:level 2, :id nil} "Default."]
                 [:p
                  {}
                  "Each of the following shall constitute an event of default (“Event of Default”) under this Note:"
                  [:markdown/soft-line-break {}]
                  "(a) the Maker shall fail to pay when due (whether by acceleration or otherwise) principal or interest on this Note, and such default shall have continued for a period of 90 days;"
                  [:markdown/soft-line-break {}]
                  "(b) a proceeding (other than a proceeding commenced by the Maker) shall have been instituted in a court having jurisdiction seeking a decree or order for relief in respect of the Maker in an involuntary case under any applicable bankruptcy, insolvency or other similar law now or hereafter in effect, and such proceedings shall remain undismissed or unstayed and in effect for a period of 90 days (so long as the Maker is diligently proceeding to effect such dismissal or stay) or such court shall enter a decree or order granting the relief sought in such proceeding; or"
                  [:markdown/soft-line-break {}]
                  "(c) the Maker commences a voluntary case under any applicable bankruptcy, insolvency or other similar law now or hereafter in effect, consents to the entry of an order for relief in an involuntary case under any such law, or makes a general assignment for the benefit of creditors, or fails generally to pay his debts as they become due, or takes any action in furtherance of any of the foregoing."]
                 [:markdown/heading {:level 2, :id nil} "Remedies."]
                 [:p
                  {}
                  "Upon the occurrence of any Event of Default, the Lender may, without notice or demand to the Maker, exercise any or all of the following remedies:"
                  [:markdown/soft-line-break {}]
                  "(a) declare all unpaid principal owing under this Note, together with all accrued and unpaid interest and other amounts owing hereunder, to be immediately due and payable without demand, protest, notice of protest, notice of default, presentment for payment or further notice of any kind; or"
                  [:markdown/soft-line-break {}]
                  "(b) proceed to enforce such other and additional rights and remedies as the Lender may be provided by applicable law."]
                 [:markdown/heading {:level 2, :id nil} "Governing Law."]
                 [:p
                  {}
                  "This Note shall be governed by, and construed and enforced in accordance with, the internal laws (other than the choice of law principles thereof) of \"New York, NY\"."]
                 [:markdown/heading {:level 2, :id nil} "Waiver."]
                 [:p
                  {}
                  "No failure to exercise and no delay in exercising any right, power or privilege hereunder shall operate as a waiver thereof, nor shall any single or partial exercise of any right, power or privilege hereunder preclude any other or further exercise thereof or the exercise of any other right, power or privilege. The rights and remedies herein provided are cumulative and not exclusive of any rights or remedies provided by law."]
                 [:markdown/heading {:level 2, :id nil} "Savings Clause."]
                 [:p
                  {}
                  "Notwithstanding any provision contained in this Note, the Lender shall not be entitled to receive, collect or apply as interest on this Note any amount in excess of the highest lawful rate permissible under any law which a court of competent jurisdiction may deem applicable hereto. If the Lender ever receives, collects or applies as interest any such excess, the amount that would be excessive interest shall be deemed to be a partial payment of principal and treated hereunder as such, and, if the principal balance of this Note is paid in full, any remaining excess shall promptly be paid to the Maker."]
                 [:markdown/heading {:level 2, :id nil} "Amendment."]
                 [:p
                  {}
                  "This Note may be amended or modified only upon the written consent of both the Lender and the Maker. Any amendment must specifically state the provision or provisions to be amended and the manner in which such provision or provisions are to be amended."]
                 [:markdown/heading {:level 2, :id nil} "Entire Agreement."]
                 [:p
                  {}
                  "This Note constitutes the entire agreement of the Maker and the Lender with respect to the subject matter hereof and supersedes all other prior arrangements, understandings, statements, representations and warranties, expressed or implied, and no oral statements or prior written statements not contained in this Note shall have any force and effect."]
                 [:markdown/heading {:level 2, :id nil} "Counterparts."]
                 [:p
                  {}
                  "This Note may be executed in counterparts, each of which shall constitute an original and all of which shall constitute one and the same instrument."]
                 [:markdown/heading {:level 2, :id nil} "Assignment."]
                 [:p
                  {}
                  "This Note may not be assigned and/or transferred in whole or in part by the Maker without the prior written consent of the Lender, which consent shall be in the Lender’s sole and absolute discretion. This Note may be assigned and/or transferred in whole or in part by the Lender at any time. The obligations of the Maker hereunder shall bind his heirs and permitted assigns, and all rights, benefits and privileges conferred on the Lender by this Note shall be and hereby are extended to, conferred upon, and may be enforced by, the successors and assigns of the Lender."]
                 [:p
                  {}
                  "IN WITNESS WHEREOF, the Maker has executed this Note as of the date and year first above written."]]}
      :template/hiccup {:schema [:schema {:registry {"hiccup" [:orn
                                                           [:node [:catn
                                                                   [:name keyword?]
                                                                   [:props [:? [:map-of keyword? any?]]]
                                                                   [:children [:* [:schema [:ref "hiccup"]]]]]]
                                                           [:primitive [:orn
                                                                        [:nil nil?]
                                                                        [:boolean boolean?]
                                                                        [:number number?]
                                                                        [:text string?]]]]}}
                             "hiccup"]
                    :value [:div
                            {}
                            [:markdown/heading {:level 1, :id nil} "PROMISSORY NOTE"]
                            [:p {} "{{amount as \"0,0.00 CCC\"}} {{date}}"]
                            [:p
                             {}
                             "FOR VALUE RECEIVED, the undersigned, {{maker}}, {{#if individual}}an individual residing{{else}}a company registered{{/if}} at {{makerAddress}} (“Maker”), hereby promises to pay to the order of {{lender}}, a {{legalEntity}}, having offices at {{lenderAddress}}, or its successors and permitted assigns (“Lender” or the “Company”), the principal sum of {{principal as \"0,0.00 CCC\"}}, plus any and all interest accrued thereon at the Note Rate (defined below), each due and payable in cash in lawful money of the United States on the dates and in the manner set forth in this Promissory Note (this “Note”)."]
                            [:markdown/heading {:level 2, :id nil} "Interest."]
                            [:p
                             {}
                             "The principal amount of this Note shall bear interest at {{interestRate}}% per annum (the “Note Rate”). Interest shall be computed on the basis of a three hundred and sixty-five (365) day year and charged for the actual number of days elapsed. Interest shall accrue on the original principal balance only and there shall be no accrual of interest upon interest."]
                            [:markdown/heading {:level 2, :id nil} "Payment of Principal and Interest."]
                            [:p
                             {}
                             "The principal amount of this Note and the interest thereon shall be due and payable in full on the earlier of (a) {{maturityDate}} or (b) ten (10) days prior to the Company filing an S-1 registration statement with the U.S. Securities and Exchange Commission in contemplation of an initial public offering (“IPO”). As used herein, IPO means the closing of a firm commitment underwritten public offering pursuant to a registration statement under the Securities Act of 1933, as amended."]
                            [:markdown/heading {:level 2, :id nil} "Prepayment."]
                            [:p {} "The Maker may prepay any portion of the principal balance of this Note at any time without penalty."]
                            [:markdown/heading {:level 2, :id nil} "Default."]
                            [:p
                             {}
                             "Each of the following shall constitute an event of default (“Event of Default”) under this Note:"
                             [:markdown/soft-line-break {}]
                             "(a) the Maker shall fail to pay when due (whether by acceleration or otherwise) principal or interest on this Note, and such default shall have continued for a period of {{defaultDays}} days;"
                             [:markdown/soft-line-break {}]
                             "(b) a proceeding (other than a proceeding commenced by the Maker) shall have been instituted in a court having jurisdiction seeking a decree or order for relief in respect of the Maker in an involuntary case under any applicable bankruptcy, insolvency or other similar law now or hereafter in effect, and such proceedings shall remain undismissed or unstayed and in effect for a period of {{insolvencyDays}} days (so long as the Maker is diligently proceeding to effect such dismissal or stay) or such court shall enter a decree or order granting the relief sought in such proceeding; or"
                             [:markdown/soft-line-break {}]
                             "(c) the Maker commences a voluntary case under any applicable bankruptcy, insolvency or other similar law now or hereafter in effect, consents to the entry of an order for relief in an involuntary case under any such law, or makes a general assignment for the benefit of creditors, or fails generally to pay his debts as they become due, or takes any action in furtherance of any of the foregoing."]
                            [:markdown/heading {:level 2, :id nil} "Remedies."]
                            [:p
                             {}
                             "Upon the occurrence of any Event of Default, the Lender may, without notice or demand to the Maker, exercise any or all of the following remedies:"
                             [:markdown/soft-line-break {}]
                             "(a) declare all unpaid principal owing under this Note, together with all accrued and unpaid interest and other amounts owing hereunder, to be immediately due and payable without demand, protest, notice of protest, notice of default, presentment for payment or further notice of any kind; or"
                             [:markdown/soft-line-break {}]
                             "(b) proceed to enforce such other and additional rights and remedies as the Lender may be provided by applicable law."]
                            [:markdown/heading {:level 2, :id nil} "Governing Law."]
                            [:p
                             {}
                             "This Note shall be governed by, and construed and enforced in accordance with, the internal laws (other than the choice of law principles thereof) of {{jurisdiction}}."]
                            [:markdown/heading {:level 2, :id nil} "Waiver."]
                            [:p
                             {}
                             "No failure to exercise and no delay in exercising any right, power or privilege hereunder shall operate as a waiver thereof, nor shall any single or partial exercise of any right, power or privilege hereunder preclude any other or further exercise thereof or the exercise of any other right, power or privilege. The rights and remedies herein provided are cumulative and not exclusive of any rights or remedies provided by law."]
                            [:markdown/heading {:level 2, :id nil} "Savings Clause."]
                            [:p
                             {}
                             "Notwithstanding any provision contained in this Note, the Lender shall not be entitled to receive, collect or apply as interest on this Note any amount in excess of the highest lawful rate permissible under any law which a court of competent jurisdiction may deem applicable hereto. If the Lender ever receives, collects or applies as interest any such excess, the amount that would be excessive interest shall be deemed to be a partial payment of principal and treated hereunder as such, and, if the principal balance of this Note is paid in full, any remaining excess shall promptly be paid to the Maker."]
                            [:markdown/heading {:level 2, :id nil} "Amendment."]
                            [:p
                             {}
                             "This Note may be amended or modified only upon the written consent of both the Lender and the Maker. Any amendment must specifically state the provision or provisions to be amended and the manner in which such provision or provisions are to be amended."]
                            [:markdown/heading {:level 2, :id nil} "Entire Agreement."]
                            [:p
                             {}
                             "This Note constitutes the entire agreement of the Maker and the Lender with respect to the subject matter hereof and supersedes all other prior arrangements, understandings, statements, representations and warranties, expressed or implied, and no oral statements or prior written statements not contained in this Note shall have any force and effect."]
                            [:markdown/heading {:level 2, :id nil} "Counterparts."]
                            [:p
                             {}
                             "This Note may be executed in counterparts, each of which shall constitute an original and all of which shall constitute one and the same instrument."]
                            [:markdown/heading {:level 2, :id nil} "Assignment."]
                            [:p
                             {}
                             "This Note may not be assigned and/or transferred in whole or in part by the Maker without the prior written consent of the Lender, which consent shall be in the Lender’s sole and absolute discretion. This Note may be assigned and/or transferred in whole or in part by the Lender at any time. The obligations of the Maker hereunder shall bind his heirs and permitted assigns, and all rights, benefits and privileges conferred on the Lender by this Note shall be and hereby are extended to, conferred upon, and may be enforced by, the successors and assigns of the Lender."]
                            [:p {} "IN WITNESS WHEREOF, the Maker has executed this Note as of the date and year first above written."]]}
   :contract  {:schema [:map
                         [:crux.db/id uuid?]
                         [:contract/contractId uuid?]
                         [:contract/clauseId uuid?]
                         [:contract/title string?]
                         [:contract/$class string?]
                         [:contract/template string?]
                         [:contract/text {:optional true}    string?]
                         [:contract/model {:optional true}   string?]
                         [:contract/logic {:optional true}   string?]
                         [:contract/data {:optional true}      map?]
                         [:contract/partyId {:optional true} string?]
                         [:contract/stateId {:optional true} string?]]
                :value {:crux.db/id #uuid"20cca797-3560-441c-b718-ae97807a4678"
                        :contract/contractId #uuid"20cca797-3560-441c-b718-ae97807a4678"
                        :contract/clauseId #uuid"20cca797-3560-441c-b718-ae97807a4678"
                        :contract/title "Promissory Note"
                        :contract/$class "org.accordproject.promissorynote.PromissoryNoteContract"
                        :contract/text "Text Pointer to hiccup"                       
                        :contract/template "Text Pointer to hiccup"
                        :contract/model "Text Pointer to json schema"
                        :contract/logic "text"
                        :contract/data {:amount {:$class "org.accordproject.money.MonetaryAmount", :doubleValue 1000, :currencyCode "USD"}
                                        :date "2018-01-30T00:00:00.000-04:00"
                                        :defaultDays 90
                                        :lender "Clause Inc."
                                        :jurisdiction "New York, NY"
                                        :contractId "6beb3db0-a949-4e2b-bb2e-00e2acd7f0e9"
                                        :interestRate 3.8
                                        :lenderAddress "246 5th Ave, 3rd Fl, New York, NY 10001"
                                        :$identifier "6beb3db0-a949-4e2b-bb2e-00e2acd7f0e9"
                                        :$class "org.accordproject.promissorynote.PromissoryNoteContract"
                                        :insolvencyDays 90
                                        :individual true
                                        :maker "Daniel"
                                        :principal {:$class "org.accordproject.money.MonetaryAmount", :doubleValue 500, :currencyCode "USD"}
                                        :maturityDate "2019-01-20T00:00:00.000-04:00"
                                        :makerAddress "1 Main Street"
                                        :legalEntity "CORP"}
                        :contract/partyId "Text Pointer to Party - multi Person Entity"
                        :contract/stateId "string"}}
   :clause      {:schema [:map
                         [:crux.db/id uuid?]
                         [:clause/contractId uuid?]
                         [:clause/clauseId uuid?]
                         [:clause/title string?]
                         [:clause/$class string?]
                         [:clause/template string?]
                         [:clause/text {:optional true}    string?]
                         [:clause/model {:optional true}   string?]
                         [:clause/logic {:optional true}   string?]
                         [:clause/data {:optional true}      map?]
                         [:clause/partyId {:optional true} string?]
                         [:clause/stateId {:optional true} string?]]
                :value {:crux.db/id #uuid"20cca797-3560-441c-b718-ae97807a4678"
                        :clause/contractId #uuid"20cca797-3560-441c-b718-ae97807a4678"
                        :clause/clauseId #uuid"20cca797-3560-441c-b718-ae97807a4678"
                        :clause/title "Promissory Note"
                        :clause/$class "org.accordproject.promissorynote.PromissoryNoteContract"
                        :clause/text "Text Pointer to hiccup"                       
                        :clause/template "Text Pointer to hiccup"
                        :clause/model "Text Pointer to json schema"
                        :clause/logic "text"
                        :clause/data {:amount {:$class "org.accordproject.money.MonetaryAmount", :doubleValue 1000, :currencyCode "USD"}
                                        :date "2018-01-30T00:00:00.000-04:00"
                                        :defaultDays 90
                                        :lender "Clause Inc."
                                        :jurisdiction "New York, NY"
                                        :contractId "6beb3db0-a949-4e2b-bb2e-00e2acd7f0e9"
                                        :interestRate 3.8
                                        :lenderAddress "246 5th Ave, 3rd Fl, New York, NY 10001"
                                        :$identifier "6beb3db0-a949-4e2b-bb2e-00e2acd7f0e9"
                                        :$class "org.accordproject.promissorynote.PromissoryNoteContract"
                                        :insolvencyDays 90
                                        :individual true
                                        :maker "Daniel"
                                        :principal {:$class "org.accordproject.money.MonetaryAmount", :doubleValue 500, :currencyCode "USD"}
                                        :maturityDate "2019-01-20T00:00:00.000-04:00"
                                        :makerAddress "1 Main Street"
                                        :legalEntity "CORP"}
                        :clause/partyId "Text Pointer to Party - multi Person Entity"
                        :clause/stateId "string"}}
   :runtime {:schema [:map
                      {:title "runtime"}
                      [:request map?]
                      [:response map?]
                      [:obligation {:optional true} string?]
                      [:promisor {:optional true} string?]
                      [:promisee {:optional true} string?]]
             :value {:request {:$class "org.accordproject.helloworld.MyRequest"
                               :input "Accord Project"}
                     :response {:clause "helloworld@0.14.0-767ffde65292f2f4e8aa474e76bb5f923b80aa29db635cd42afebb6a0cd4c1fa"
                                :request {:$class "org.accordproject.helloworld.MyRequest"
                                          :input "Accord Project"}
                                :response {:$class "org.accordproject.helloworld.MyResponse"
                                           :output "Hello Fred Blogs Accord Project"
                                           :timestamp "2021-06-16T11:38:42.011-04:00"}
                                :state    {:$class "org.accordproject.runtime.State"
                                           :identifier "f4428ec2-73ca-442b-8006-8e9a290930ad"}
                                :emit    []}}}
   :xy {:schema [:and
                 [:map
                  [:x int?]
                  [:y int?]]
                 [:fn {:error/message "x should be greater than y"}
                  '(fn [{:keys [x y]}] (> x y))]]
        :value {:x 1, :y 2}}})

(defonce state* (r/atom {:schema (.getParameterValue (.parse Uri js/location) "schema")
                         :value (.getParameterValue (.parse Uri js/location) "value")}))
(defonce delayed-state* (r/atom nil))
(defonce mirrors* (r/atom {}))

(defonce delayed (functions/debounce
                   (fn [state]
                     (let [schema (read-schema (:schema state))
                           value (read-value (:value state))
                           samples (try (mg/sample schema) (catch js/Error _))
                           inferred (try (mp/provide [value]) (catch js/Error _))
                           inferred-samples (try (mp/provide [samples]) (catch js/Error _))]
                       (.replaceState js/window.history nil "" (str (.setParameterValue (.parse Uri js/location) "value" (:value state))))
                       (.replaceState js/window.history nil "" (str (.setParameterValue (.parse Uri js/location) "schema" (:schema state))))
                       (.setValue (@mirrors* "samples") (try (str/join "\n\n" (map pretty samples)) (catch js/Error _ "")))
                       (.setValue (@mirrors* "json-schema") (try (pretty (mj/transform schema)) (catch js/Error _ "")))
                       (.setValue (@mirrors* "swagger-schema") (try (pretty (ms/transform schema)) (catch js/Error _ "")))
                       (.setValue (@mirrors* "dot-schema") (try (trimmed-str (md/transform schema) println) (catch js/Error _ "")))
                       (.setValue (@mirrors* "inferred") (pretty inferred))
                       (.setValue (@mirrors* "inferred-samples") (pretty inferred-samples))
                       (.setValue (@mirrors* "samples-inferred") (try (str/join "\n\n" (map pretty (mg/sample inferred))) (catch js/Error _ "")))
                       (.setValue (@mirrors* "samples-inferred-samples") (try (str/join "\n\n" (map pretty (mg/sample inferred-samples))) (catch js/Error _ "")))
                       (reset! delayed-state* state))) 1000))

(defn sync-delayed-state! []
  (when (not= @delayed-state* @state*)
    (delayed @state*)))

(r/track! sync-delayed-state!)

(defn reset-value! [value]
  (swap! state* assoc :value value)
  (.setValue (@mirrors* "value") (or value "")))

(defn reset-schema! [value]
  (swap! state* assoc :schema value)
  (.setValue (@mirrors* "schema") (or value "")))

(defn editor [id state* {:keys [value mode on-change]}]
  (r/create-class
    {:render (fn [] [:textarea
                     {:type "text"
                      :id id
                      :default-value (or value @state*)
                      :auto-complete "off"}])
     :component-did-mount (fn [this]
                            (let [opts (if value
                                         #js {:mode (or mode "clojure")
                                              :matchBrackets true
                                              :readOnly true
                                              :lineNumbers true}
                                         #js {:mode (or mode "clojure")
                                              :matchBrackets true
                                              :lineNumbers true})
                                  cm (.fromTextArea js/CodeMirror (rd/dom-node this) opts)]
                              (when-not mode
                                (js/parinferCodeMirror.init cm)
                                (.removeKeyMap cm))
                              (when-not value
                                (.on cm "change" #(let [value (.getValue %)]
                                                    (reset! state* value)
                                                    (when on-change (on-change value))))
                                (.setOption cm "extraKeys" #js {:Shift-Tab false
                                                                :Tab false}))
                              (swap! mirrors* assoc id cm)))
     :component-will-unmount (fn [] (.toTextArea (@mirrors* id)))}))

(defn examples []
  (let [reset! (fn [schema]
                 (fn []
                   (reset-schema! (some-> models schema :schema m/form pretty))
                   (reset-value! (some-> models schema :value pretty))
                   nil))]
    [:div.buttons
     [:button.btn.btn-sm.btn-outline-warning
      {:on-click (reset! :empty)}
      "empty"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :any?)}
      "any?"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :contract)}
      "Contract"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :clause)}
      "Clause"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :contract-data)}
      "Contract Data"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :user)}
      "User"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :person)}
      "Person"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :address)}
      "Postal Address"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :multi)}
      "Multi"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :project)}
      "Project"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :runtime)}
      "Runtime"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :text/hiccup)}
      "Text/hiccup"]
     [:button.btn.btn-sm.btn-outline-primary
      {:on-click (reset! :template/hiccup)}
      "Template/hiccup"]
     

     (if (not= @state* @delayed-state*) [:span.text-muted.small " inferring schemas and generating values.."])]))

(defn error [error]
  [:div.alert.alert-danger
   (update error :schema m/form)])

(defn valid [schema value]
  (try
    (let [valid? (m/validate schema value)]
      [:pre {:class (if valid? "alert alert-success" "alert alert-danger")}
       (str valid?)])
    (catch js/Error _ [:div.alert.alert-warning "???"])))

(defn errors [schema value]
  (try
    [:div
     (or (seq (for [[i error] (map-indexed vector (:errors (m/explain schema value)))
                    :let [error' (me/with-error-message error)]]
                [:div.alert.alert-danger
                 {:key i}
                 [:b (:message error') ": "]
                 [:span (-> error' (update :schema m/form) (dissoc :message) (cond-> (not (:type error')) (dissoc :type)) pr-str)]]))
         [:div.alert.alert-success "nil"])]
    (catch js/Error _ [:div.alert.alert-warning "???"])))

(defn humanized-errors [schema value]
  (if-let [errors (try (some-> (m/explain schema value)
                               (me/with-spell-checking)
                               (me/humanize)
                               (pretty)
                               (str/trim))
                       (catch js/Error _ nil))]
    [:div.alert.alert-danger [:pre errors]]
    [:div.alert.alert-success "nil"]))

(defn code [id opts]
  (try [editor id nil (merge opts {:value ""})]
       (catch js/Error _)))

(defn swagger-schema [schema]
  (let [schema (try (edn/read-string schema) (catch js/Error _))]
    [:<>
     [:h3 "Swagger2 Schema"]
     [code "swagger2-schema" (try (pretty (ms/transform schema)) (catch js/Error _ ""))]]))

(defn app []
  (let [schema (read-schema (-> state* deref :schema))
        value (read-value (-> state* deref :value))]

    [:div#malli.container
     [:div.row
      [:p.col-12.lead
       [:span [:a {:href "https://github.com/metosin/malli"
                   :target "_blank"}
               "TrustBlocks"]
        " playground"]]]
     [:div
      [examples]
      [:h3 "Schema"]
      [editor "schema" (r/cursor state* [:schema])]
      [:h3 "Value"]
      [editor "value" (r/cursor state* [:value])]
      [:h3 "Valid"]
      [valid schema value]
      [:h3 "Errors"]
      [humanized-errors schema value]
      [errors schema value]
      [:div
       {:class (if (not= @state* @delayed-state*) "overlay")}
       [:h3 "Sample values"]
       [code "samples"]
       [:h3 "Transformed Schema"]
       [:div
        [:h5 {:style {:color "grey"}} "JSON Schema"]
        [code "json-schema"]
        [:h5 {:style {:color "grey"}} "Swagger Schema"]
        [code "swagger-schema"]
        [:h5 {:style {:color "grey"}} "DOT"]
        [code "dot-schema" {:mode "dot"}]]
       [:h3 "Inferred Schema"]
       [:div
        [:h5 {:style {:color "grey"}} "from value"]
        [code "inferred"]
        [:h5 {:style {:color "grey"}} "from samples"]
        [code "inferred-samples"]]
       [:h3 "Sample values from Inferred Schema"]
       [:div
        [:h5 {:style {:color "grey"}} "from value"]
        [code "samples-inferred"]
        [:h5 {:style {:color "grey"}} "from samples"]
        [code "samples-inferred-samples"]]]]]))

(defn mount-app-element []
  (when-let [el (js/document.getElementById "app")]
    (rd/render [app] el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
