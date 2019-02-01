(import :gerbil/expander
        :gerbil/gambit
        :scheme/process-context
        :std/format
        :std/interactive
        :std/misc/uuid
        :std/net/repl
        :std/sort
        :std/srfi/1
        :std/sugar
        :thunknyc/apropos
        :drewc/repl-history)

(export start-treadmill!
        eval-string/input-string
        eval/input
        eval/sentinel
        complete
        completion-meta
        uuid->string
        random-uuid)

(def (start-treadmill!)
  (let* ((s (start-repl-server! address: "127.0.0.1:0"))
         (port (socket-info-port-number
                (tcp-server-socket-info
                 (thread-specific s)))))
    (printf "Running network REPL on port ~A.\n" port)
    (_gx#load-expander!)
    (thread-join! s)))

(def (module-context mod)
  (try
   (gx#import-module mod #f #f)
   (catch (e)
     (error "Module does not exist." mod))))

(defrules eval/sentinel ()
  ((_ form)
   (let (sentinel (uuid->string (random-uuid)))
     (printf "|~A>>" sentinel)
     (let (result (eval 'form))
       (printf "~S<<~A|\n" result sentinel)))))

(def (eval-and-add-history expression (module #f))
  (try
   (let (result
         (if module
           (parameterize
               ((current-expander-allow-rebind? #t)
                (gx#current-expander-context (module-context module)))
             (eval expression))
           (eval expression)))
     (repl-history% 'add! expression result)
     result)
   (catch (e)
     (eprintf "Message: ~A\n"
              (error-message e)))))

(def (eval/input e p (mod #f))
  (let ((out (open-output-string))
        (err (open-output-string)))
    (parameterize ((current-input-port (or p (current-input-port)))
                   (current-output-port out)
                   (current-error-port err))
      (let (result
            (call-with-values (lambda () (eval-and-add-history e mod))
              (lambda vals vals)))
        `(,result
          ,(get-output-string out)
          ,(get-output-string err))))))

(def (read-string s)
  (let (p (open-input-string s))
    (try
     (parameterize ((current-input-port p))
       (let lp ((vs '()) (val (read)))
         (if (eof-object? val) (reverse! vs)
             (lp (cons val vs) (read)))))
     (catch (e)
       (error "Reading form failed -- check for completeness.")))))

(def (eval-string/input-string e-s i-s (mod #f))
  (try
   (let* ((exprs (read-string e-s))
          (input (open-input-string i-s)))
     (let (result-sets (map (cut eval/input <> input mod) exprs))
       (fold (lambda (result accum)
               (with (([rvals rout rerr] result)
                      ([avals aout aerr] accum))
                 (list (append avals (map (cut format "~S" <>) rvals))
                       (string-append aout rout)
                       (string-append aerr rerr))))
             '(() "" "")
             result-sets)))
   (catch (e)
     `(() ""
       ,(format "Message: ~S\n" (error-message e))))))

(def (sort-by-length lis)
  (sort lis (lambda (a b)
              (let ((la (string-length a))
                    (lb (string-length b)))
                (if (= la lb)
                  (string<? a b)
                  (< la lb))))))

(def (complete str)
  (let* ((matches (apropos-re str))
         (names (map (lambda (el) (symbol->string (car el)))
                     (cadar matches))))
    (sort-by-length names)))

(def (name-entry name)
  (let* ((db (current-apropos-db))
         (entry (hash-ref db 'names (hash))))
    (hash-ref entry (string->symbol name) '())))

(def (entry-type entry)
  (let ((final (last entry)))
    (if (symbol? final) final (last final))))

(def (entry-string entry)
  (let ((t (entry-type entry)))
    (string-ref (symbol->string t) 0)))

(def (meta-entry entry)
  (let ((mod-name (car entry))
        (type-string (entry-string entry)))
    (format "~A<~A>" mod-name type-string)))

(def (completion-meta name)
  (let ((entry (name-entry name)))
    (sort-by-length (map meta-entry entry))))
