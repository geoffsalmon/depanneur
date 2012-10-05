(ns depanneur.core
  (:import [java.io StringReader StringWriter]))

(defn- mod-ptr
  "Alters the data pointer in a state by a function"
  [s f]
  (assoc s :ptr (f (:ptr s))))

(defn- mod-cell
  "Alters the current data cell in a state by a function"
  [s f]
  (assoc s (:ptr s) (f (get s (:ptr s) 0))))

;; dispatch table for the individual commands
(def ^{:private true} cmds
  {\> (fn [s] (mod-ptr s inc))
   ;; TODO: handle pointer underflow?
   \< (fn [s] (mod-ptr s dec))

   ;; TODO: handle data cell underflow/overflow?
   \+ (fn [s] (mod-cell s inc))
   \- (fn [s] (mod-cell s dec))

   \. (fn [s]
        (.write *out* (int (get s (:ptr s) 0)))
        s)
   \, (fn [s]
        (let [c (.read *in*)]
          (if (neg? c)
            ;; EOF, so leave the cell unchanged? TODO: This behaviour
            ;; should be configurable?
            s
            (mod-cell s (fn [_] c)))))
   })

(defn run-block?
  "Determine if a block should be run or re-run"
  [state]
  (not (zero? (get state (:ptr state) 0))))

(defn interpret-block [b state is-main]
  (if (or is-main (run-block? state))
    ;; reduce over the commands in this block
    (let [s (reduce
             (fn [s cmd]
               (if (char? cmd)
                 ;; execute command
                 ((get cmds cmd) s)
                 ;; execute sub block
                 (interpret-block cmd s false)))
             state b)]
      (if is-main s (recur b s false)))
    state))

(defn interpret
  ([b]
     (interpret b nil))
  ([b in]
     (binding [*in* (or (if (string? in) (StringReader. in) in)
                        ;; default to an empty StringReader to avoid
                        ;; accidentally blocking on stdin
                        (StringReader. ""))]
       (interpret-block b {:ptr 0} true)))
  ([b in out]
     (binding [*out* (or out (StringWriter.))]
       (interpret b in))))


;;(interpret (parse hw))
;;(interpret (parse rot13) "foobar")

;; Some example brainfuck programs from wikipedia
(def hw "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")

(def rot13 "-,+[                         Read first character and start outer character reading loop
    -[                       Skip forward if character is 0
        >>++++[>++++++++<-]  Set up divisor (32) for division loop
                               (MEMORY LAYOUT: dividend copy remainder divisor quotient zero zero)
        <+<-[                Set up dividend (x minus 1) and enter division loop
            >+>+>-[>>>]      Increase copy and remainder / reduce divisor / Normal case: skip forward
            <[[>+<-]>>+>]    Special case: move remainder back to divisor and increase quotient
            <<<<<-           Decrement dividend
        ]                    End division loop
    ]>>>[-]+                 End skip loop; zero former divisor and reuse space for a flag
    >--[-[<->+++[-]]]<[         Zero that flag unless quotient was 2 or 3; zero quotient; check flag
        ++++++++++++<[       If flag then set up divisor (13) for second division loop
                               (MEMORY LAYOUT: zero copy dividend divisor remainder quotient zero zero)
            >-[>+>>]         Reduce divisor; Normal case: increase remainder
            >[+[<+>-]>+>>]   Special case: increase remainder / move it back to divisor / increase quotient
            <<<<<-           Decrease dividend
        ]                    End division loop
        >>[<+>-]             Add remainder back to divisor to get a useful 13
        >[                   Skip forward if quotient was 0
            -[               Decrement quotient and skip forward if quotient was 1
                -<<[-]>>     Zero quotient and divisor if quotient was 2
            ]<<[<<->>-]>>    Zero divisor and subtract 13 from copy if quotient was 1
        ]<<[<<+>>-]          Zero divisor and add 13 to copy if quotient was 0
    ]                        End outer skip loop (jump to here if ((character minus 1)/32) was not 2 or 3)
    <[-]                     Clear remainder from first division if second division was skipped
    <.[-]                    Output ROT13ed character from copy and clear it
    <-,+                     Read next character
]                            End character reading loop")

;; (use 'criterium.core)
;; depanneur.core> (bench (interpret (parse rot13) "foobaasdfasdfr" nil))
;; Evaluation count : 1620 in 60 samples of 27 calls.
;;              Execution time mean : 38.808896 ms
;;     Execution time std-deviation : 403.324636 us
;;    Execution time lower quantile : 38.477741 ms ( 2.5%)
;;    Execution time upper quantile : 39.174752 ms (97.5%)
;;
;; Found 2 outliers in 60 samples (3.3333 %)
;; 	low-severe	 1 (1.6667 %)
;; 	low-mild	 1 (1.6667 %)
;;  Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
