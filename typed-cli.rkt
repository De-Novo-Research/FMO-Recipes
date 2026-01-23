#lang racket
(require racket/contract)

;; Command-line subset recipe generator
;; Usage: racket subset-recipe-cli.rkt input.csv output.csv

(provide (contract-out
          [string-trim (-> string? string?)]
          [string-empty? (-> string? boolean?)]
          [parse-csv-line (-> string? (listof string?))]
          [read-csv-file (-> path-string? (listof (listof string?)))]
          [parse-input-csv (-> (listof (listof string?)) (listof (list/c string? number? boolean?)))]
          [generate-optimized-recipe (-> (listof string?) 
                                         (listof string?) 
                                         (values (listof list?) 
                                                 (listof (list/c symbol? (listof exact-integer?)))
                                                 (hash/c symbol? (listof exact-integer?))
                                                 (hash/c symbol? symbol?)))]
          [compute-quantities-for-ops (-> (listof list?)
                                            (listof (list/c symbol? (listof exact-integer?)))
                                            (hash/c string? number?)
                                            (hash/c symbol? (listof exact-integer?))
                                            (listof string?)
                                            (hash/c symbol? symbol?)
                                            (hash/c symbol? (hash/c string? number?)))]
          [escape-csv-field (-> any/c string?)]
          [write-csv-row (-> (listof any/c) output-port? void?)]
          [generate-recipe-csv (-> (listof list?)
                                   (hash/c symbol? (hash/c string? number?))
                                   (hash/c symbol? (listof exact-integer?))
                                   (listof string?)
                                   path-string?
                                   void?)]
          [main (-> path-string? path-string? void?)]
          ))

;; -------------------------
;; String utility functions
;; -------------------------
(define (string-trim s)
  (regexp-replace* #px"^\\s+|\\s+$" s ""))

(define (string-empty? s)
  (= (string-length s) 0))

;; -------------------------
;; CSV parsing functions
;; -------------------------

(define (parse-csv-line line)
  "Parse a single CSV line into fields"
  (define fields '())
  (define current-field "")
  (define in-quotes? #f)
  
  (define (add-field!)
    (set! fields (cons current-field fields))
    (set! current-field ""))
  
  (let loop ([i 0])
    (when (< i (string-length line))
      (define char (string-ref line i))
      (cond
        [(and (char=? char #\") (not in-quotes?))
         (set! in-quotes? #t)
         (loop (+ i 1))]
        [(and (char=? char #\") in-quotes?)
         (if (and (< (+ i 1) (string-length line))
                  (char=? (string-ref line (+ i 1)) #\"))
             (begin
               (set! current-field (string-append current-field "\""))
               (loop (+ i 2)))
             (begin
               (set! in-quotes? #f)
               (loop (+ i 1))))]
        [(and (char=? char #\,) (not in-quotes?))
         (add-field!)
         (loop (+ i 1))]
        [else
         (set! current-field (string-append current-field (string char)))
         (loop (+ i 1))])))
  
  (add-field!)
  (reverse fields))

(define (read-csv-file filepath)
  "Read CSV file and return list of rows"
  (call-with-input-file filepath
    (lambda (in)
      (define lines '())
      (let loop ([line (read-line in)])
        (unless (eof-object? line)
          (set! lines (cons (parse-csv-line line) lines))
          (loop (read-line in))))
      (reverse lines))))

(define (parse-input-csv csv-data)
  "Parse input CSV data into structured format"
  (when (null? csv-data)
    (error "Empty CSV file"))
  
  (define headers (first csv-data))
  (define rows (rest csv-data))
  
  ;; Validate headers (case insensitive)
  (define expected-headers '("Dye" "Amount" "FMO"))
  (define normalized-headers (map (lambda (h) (string-downcase (string-trim h))) headers))
  
  (unless (>= (length normalized-headers) 3)
    (error "CSV must have at least 3 columns: Dye, Amount, FMO"))
  
  ;; Parse each row
  (define valid-data '())
  (for ([row rows] [row-num (in-range 1 (add1 (length rows)))])
    (when (>= (length row) 3)
      (define dye-name (string-trim (first row)))
      (define qty-str (string-trim (second row)))
      (define leave-out-str (string-trim (third row)))
      
      (unless (or (string-empty? dye-name) (string-empty? qty-str))
        (define qty (string->number qty-str))
        (unless qty
          (error (format "Invalid quantity '~a' in row ~a" qty-str (add1 row-num))))
        
        (define leave-out? 
          (or (string-ci=? leave-out-str "true")
              (string-ci=? leave-out-str "yes") 
              (string-ci=? leave-out-str "1")
              (string-ci=? leave-out-str "y")))
        
        (set! valid-data (cons (list dye-name qty leave-out?) valid-data)))))
  
  (reverse valid-data))

;; -------------------------
;; Generate subsets algorithm
;; -------------------------
(define (generate-optimized-recipe all-elements leave-out-elements)
  "Implements the optimal 3m + r - 5 algorithm for partial subset construction."
  
  ;; --- State and Helpers ---
  (define computed-sets (make-hash))
  (define operations '())
  (define alias-map (make-hash)) ; For tracking aliased targets

  (define (set-union s1 s2) (remove-duplicates (append s1 s2)))
  (define (perform-union result-name input1-name input2-name)
    (define set1 (hash-ref computed-sets input1-name))
    (define set2 (hash-ref computed-sets input2-name))
    (define new-set (sort (set-union set1 set2) <))
    (hash-set! computed-sets result-name new-set)
    (set! operations (cons (list result-name input1-name input2-name new-set) operations))
    result-name)
  
  ;; --- Setup ---
  (define n (length all-elements))
  (define L leave-out-elements)
  (define C (filter (lambda (x) (not (member x L))) all-elements))
  (define m (length L))
  (define r (length C))

  (define element->global-idx (make-hash))
  (for ([elem all-elements] [i (in-range 1 (add1 n))])
    (hash-set! element->global-idx elem i))
  
  (for ([elem all-elements])
    (define idx (hash-ref element->global-idx elem))
    (hash-set! computed-sets (string->symbol (format "e~a" idx)) (list idx)))

  ;; --- Algorithm for m >= 3 ---
  
  ;; Phase 1: Compute C_union (the common core)
  (define c-union-name #f)
  (when (> r 0)
    (define c-elem-syms
      (map (lambda (c-elem) (string->symbol (format "e~a" (hash-ref element->global-idx c-elem)))) C))
    (cond
      [(= r 1) (set! c-union-name (first c-elem-syms))]
      [else
       (let* ([u1 (first c-elem-syms)]
              [u2 (second c-elem-syms)]
              [first-union (perform-union 'C-core2 u1 u2)])
         (set! c-union-name
               (for/fold ([current-union first-union])
                         ([next-elem (cddr c-elem-syms)]
                          [i (in-range 2 r)])
                 (perform-union (string->symbol (format "C-core~a" (add1 i)))
                                current-union
                                next-elem))))]))
  
  ;; Phase 2: Build FC chain (Forward Chain with Core)
  (define l-elem-syms
    (map (lambda (l-elem) (string->symbol (format "e~a" (hash-ref element->global-idx l-elem)))) L))
  (define fc-names (make-hash))
  (let* ([l_sym_1 (first l-elem-syms)]
         [fc1-name (if c-union-name
                       (perform-union 'L-FC1 c-union-name l_sym_1)
                       l_sym_1)])
    (hash-set! fc-names 1 fc1-name)
    (let loop ([i 2] [prev-fc-name fc1-name])
      (when (< i m) ; Build FC_2 up to FC_{m-1}
        (let* ([l_sym_i (list-ref l-elem-syms (- i 1))]
               [new-fc-name (perform-union (string->symbol (format "L-FC~a" i)) prev-fc-name l_sym_i)])
          (hash-set! fc-names i new-fc-name)
          (loop (add1 i) new-fc-name)))))

  ;; Phase 3: Build B' chain (Backward Chain on L-only)
  (define b-prime-names (make-hash))
  (let* ([l_sym_m (list-ref l-elem-syms (- m 1))]
         [l_sym_m-1 (list-ref l-elem-syms (- m 2))]
         [bm-1-name (perform-union (string->symbol (format "L-B~a" (- m 1))) l_sym_m-1 l_sym_m)])
    (hash-set! b-prime-names (- m 1) bm-1-name)
    (let loop ([i (- m 2)] [prev-b-name bm-1-name])
      (when (>= i 2) ; Build B'_{m-2} down to B'_2
        (let* ([l_sym_i (list-ref l-elem-syms (- i 1))]
               [new-b-name (perform-union (string->symbol (format "L-B~a" i)) l_sym_i prev-b-name)])
          (hash-set! b-prime-names i new-b-name)
          (loop (sub1 i) new-b-name)))))

  ;; Phase 4: Final Merges to create S_k targets
  (define final-targets '())
  ;; Target for l_m (last element of L) - THIS IS AN ALIAS
  (let* ([l_m_elem (list-ref L (- m 1))]
         [global-idx (hash-ref element->global-idx l_m_elem)]
         [s-name (string->symbol (format "S~a" global-idx))]
         [source-name (hash-ref fc-names (- m 1))])
    (hash-set! computed-sets s-name (hash-ref computed-sets source-name))
    (hash-set! alias-map s-name source-name) ; Track the alias
    (set! final-targets (cons (list s-name (hash-ref computed-sets s-name)) final-targets)))

  ;; Target for l_1 (first element of L)
  (let* ([l_1_elem (first L)]
         [global-idx (hash-ref element->global-idx l_1_elem)]
         [s-name (string->symbol (format "S~a" global-idx))]
         [b2-name (hash-ref b-prime-names 2)])
    (if c-union-name
        (perform-union s-name c-union-name b2-name)
        (begin ; This case is an alias
          (hash-set! computed-sets s-name (hash-ref computed-sets b2-name))
          (hash-set! alias-map s-name b2-name))) ; Track the alias
    (set! final-targets (cons (list s-name (hash-ref computed-sets s-name)) final-targets)))
    
  ;; Targets for l_i (i=2 to m-1)
  (for ([i (in-range 2 m)])
    (let* ([l_i_elem (list-ref L (- i 1))]
           [global-idx (hash-ref element->global-idx l_i_elem)]
           [s-name (string->symbol (format "S~a" global-idx))]
           [fc-input-name (hash-ref fc-names (- i 1))]
           [b-input-name (if (= (+ i 1) m)
                             (list-ref l-elem-syms (- m 1)) ; B'_{m} is just l_m
                             (hash-ref b-prime-names (+ i 1)))])
      (perform-union s-name fc-input-name b-input-name)
      (set! final-targets (cons (list s-name (hash-ref computed-sets s-name)) final-targets))))

  ;; Return values
  (values (reverse operations) (reverse final-targets) computed-sets alias-map))

;; -------------------------
;; Compute quantities (FIXED: now uses alias-map)
;; -------------------------
(define (compute-quantities-for-ops ops targets quantities computed-sets elements alias-map)
  (define q (make-hash))
  (define n (length elements))
  (define element->index (let ([ht (make-hash)]) (for ([elem elements] [i (in-range 1 (add1 n))]) (hash-set! ht elem i)) (lambda (elem) (hash-ref ht elem))))
  
  ;; Seed the initial quantities for all final targets
  (for ([target targets])
    (define step-name (first target))
    (define subset-indices (second target))
    (define step-q (make-hash))
    (for ([idx subset-indices])
      (define elem (list-ref elements (- idx 1)))
      (hash-set! step-q elem (hash-ref quantities elem)))
    
    (hash-set! q step-name step-q)
    
    ;; If this target is an alias, also seed the underlying operation's name
    (define aliased-op (hash-ref alias-map step-name #f))
    (when aliased-op
      (hash-set! q aliased-op step-q)))
      
  ;; Propagate quantities backward through the operations graph
  (for ([op (reverse ops)])
    (define result-step (first op))
    (define in1-step (second op))
    (define in2-step (third op))
    (define result-q (hash-ref q result-step #f))
    (when result-q
      (define in1-indices (hash-ref computed-sets in1-step))
      (define in1-q (hash-ref q in1-step (make-hash)))
      (define in2-q (hash-ref q in2-step (make-hash)))
      (for ([(elem qty) (in-hash result-q)])
        (define elem-idx (element->index elem))
        (if (member elem-idx in1-indices)
            (hash-set! in1-q elem (+ (hash-ref in1-q elem 0) qty))
            (hash-set! in2-q elem (+ (hash-ref in2-q elem 0) qty))))
      (hash-set! q in1-step in1-q)
      (hash-set! q in2-step in2-q)))
  q)

;; -------------------------
;; CSV output generation
;; -------------------------

(define (escape-csv-field field)
  "Escape a field for CSV output"
  (define field-str (if (string? field) field (format "~a" field)))
  (cond
    [(regexp-match? #rx"[,\"\r\n]" field-str)
     (format "\"~a\"" (string-replace field-str "\"" "\"\""))]
    [else field-str]))

(define (write-csv-row row port)
  "Write a single CSV row to port"
  (display (string-join (map escape-csv-field row) ",") port)
  (newline port))

(define (generate-recipe-csv ops quantities computed-sets elements output-path)
  "Generate CSV output with recipe steps"
  (define n (length elements))
  (define element->index 
    (let ([ht (make-hash)]) 
      (for ([elem elements] [i (in-range 1 (add1 n))]) 
        (hash-set! ht elem i)) 
      (lambda (elem) (hash-ref ht elem))))
  
  (define (step-to-elements step) 
    (map (lambda (idx) (list-ref elements (- idx 1))) 
         (hash-ref computed-sets step '())))
  
  (define csv-rows '())
  
  ;; Add header row
  (set! csv-rows (cons (list "result_set" "input1_set" "input1_volume" "input2_set" "input2_volume" 
                            "dye_compositions") csv-rows))
  
  ;; Helper function to get display name for a step
  (define (step-display-name step)
    (define step-str (symbol->string step))
    (cond
      [(regexp-match #rx"^e([0-9]+)$" step-str)
       => (lambda (match)
            (define idx (string->number (second match)))
            (list-ref elements (- idx 1)))]
      [else step-str]))
  
  ;; Process each operation
  (for ([op ops])
    (define step (first op))
    (define in1-step (second op))
    (define in2-step (third op))
    (define res-indices (fourth op))
    (define q-step (hash-ref quantities step (make-hash)))
    (define in1-indices (hash-ref computed-sets in1-step))
    
    ;; Calculate quantities for each input
    (define q-for-in1 (make-hash))
    (define q-for-in2 (make-hash))
    (for ([(elem qty) (in-hash q-step)])
      (if (member (element->index elem) in1-indices)
          (hash-set! q-for-in1 elem qty)
          (hash-set! q-for-in2 elem qty)))
    
    (define total-qty-in1 (apply + (hash-values q-for-in1)))
    (define total-qty-in2 (apply + (hash-values q-for-in2)))
    
    ;; Format dye compositions
    (define res-set-names (map (lambda (idx) (list-ref elements (- idx 1))) res-indices))
    (define dye-compositions 
      (string-join 
        (map (lambda (elem) 
               (format "~a|~a" elem (hash-ref q-step elem 0))) 
             (sort res-set-names string<?)) 
        ";"))
    
    ;; Add row to CSV using display names for singleton sets
    (define csv-row (list (step-display-name step)
                         (step-display-name in1-step)
                         (number->string total-qty-in1)
                         (step-display-name in2-step) 
                         (number->string total-qty-in2)
                         dye-compositions))
    (set! csv-rows (cons csv-row csv-rows)))
  
  ;; Calculate total required quantities for each dye
  (define total-quantities (make-hash))
  (for ([elem elements])
    (define elem-idx (element->index elem))
    (define e-step-symbol (string->symbol (format "e~a" elem-idx)))
    (define total-qty (apply + (hash-values (hash-ref quantities e-step-symbol (make-hash)))))
    (hash-set! total-quantities elem total-qty))
  
  ;; Add total quantities section
  (set! csv-rows (cons '() csv-rows)) ; Empty row separator
  (set! csv-rows (cons (list "TOTAL QUANTITIES REQUIRED") csv-rows))
  (set! csv-rows (cons (list "dye" "total_quantity") csv-rows))
  
  (for ([elem (sort elements string<?)])
    (define total-qty (hash-ref total-quantities elem 0))
    (when (> total-qty 0)
      (set! csv-rows (cons (list elem (number->string total-qty)) csv-rows))))
  
  ;; Write CSV file
  (call-with-output-file output-path
    (lambda (out)
      (for ([row (reverse csv-rows)])
        (write-csv-row row out)))
    #:exists 'replace))

;; -------------------------
;; Main function
;; -------------------------
(define (main input-csv output-csv)
  "Main function to process CSV input and generate recipe CSV output"
  (define csv-data (read-csv-file input-csv))
  (define parsed-data (parse-input-csv csv-data))
  
  ;; Validate that at least one item is marked "leave out"
  (unless (ormap third parsed-data)
    (error "At least one dye must be marked 'leave out' (true/yes/1/y)"))
  
  ;; Sort data alphabetically by dye name
  (define sorted-data (sort parsed-data (lambda (a b) (string<? (first a) (first b)))))
  (define elements (map first sorted-data))
  (define quantities (make-hash (map (lambda (d) (cons (first d) (second d))) sorted-data)))
  (define n (length elements))
  
  (when (< n 3)
    (error "At least 3 dyes are required in total"))
  
  ;; Determine which subsets are needed
  (define leave-out-elements
    (for/list ([data sorted-data] #:when (third data))
      (first data)))
  
  ;; The optimal partial algorithm is designed for m >= 3 targets
  (when (< (length leave-out-elements) 3)
    (error "The optimized recipe generator requires at least 3 dyes to be marked 'leave out'."))
  
  ;; Generate subset operations using the new optimized algorithm
  (define-values (final-ops final-targets computed-sets alias-map)
    (generate-optimized-recipe elements leave-out-elements))
  
  ;; Compute quantities and generate output
  (define q (compute-quantities-for-ops final-ops final-targets quantities computed-sets elements alias-map))
  (generate-recipe-csv final-ops q computed-sets elements output-csv)
  
  (printf "Recipe generated successfully: ~a\n" output-csv))

;; -------------------------
;; Command line interface
;; -------------------------
(module+ main
  (define args (current-command-line-arguments))
  (cond
    [(= (vector-length args) 2)
     (define input-file (vector-ref args 0))
     (define output-file (vector-ref args 1))
     
     ;; Check if input file exists
     (unless (file-exists? input-file)
       (error (format "Input file does not exist: ~a" input-file)))
     
     (with-handlers ([exn:fail? (lambda (e) 
                                 (printf "Error: ~a\n" (exn-message e))
                                 (exit 1))])
       (main input-file output-file))]
    [else
     (printf "Usage: racket subset-recipe-cli.rkt input.csv output.csv\n")
     (printf "\nInput CSV format:\n")
     (printf "  Column 1: dye (string)\n") 
     (printf "  Column 2: final quantity (number)\n")
     (printf "  Column 3: leave out? (true/yes/1/y for yes, anything else for no)\n")
     (exit 1)]))