(module cmd-script (script-entry
                    script-stringize
                    script-entry?
                    script-entry.get-cd
                    script-entry.get-environ
                    script-entry.get-cmd-list
                    script-entry.set-cd
                    script-entry.set-env
                    script-entry.set-environ
                    script-entry.set-cmd-list
                    cmd-builder?
                    cmd-builder-new
                    cmd-builder.script
                    script.cd
                    script.cmd
                    script.set-env
                    )
  (import (chicken base))
  (import scheme)
  (import srfi-1)
  (import (chicken condition))
  
  (define (script-entry #!key (cd ".") (env '()) (cmd-list '()) (type 'work-env))
    `(script-entry (cd . ,cd)
                   (env . ,env)
                   (cmd-list . ,cmd-list)
                   (type . ,type)))

  (define (script-type-error msg)
    (condition `(script-type-error msg ,msg)))

  (define (script-entry? se)
    (and (pair? se)
         (eq? (car se) 'script-entry)))

  (define (script-stringize val)
    (cond ((symbol? val) (symbol->string val))
          ((string? val) val)
          (#t (abort (script-type-error "Expected string")))
    ))

  (define (script-entry.get-field se fname)
    (cdr (assoc fname (cdr se))))

  (define (script-entry.set-field! se fname value)
    (set-cdr! (assoc fname (cdr se)) value))

  (define (script-entry.copy se)
    (cons 'script-entry (alist-copy (cdr se))))

  (define (script-entry.set-field se fname value)
    (let ((rval (script-entry.copy se)))
      (begin
        (script-entry.set-field! rval fname value)
        rval)))

  (define (script-entry.get-cd se)
    (script-entry.get-field se 'cd))

  (define (script-entry.set-cd se path)
    (script-entry.set-field se 'cd path))

  (define (script-entry.get-environ se)
    (script-entry.get-field se 'env))

  (define (script-entry.set-environ se env)
    (script-entry.set-field se 'env env))

  (define (script-entry.set-env se var value)
    (let ((var.str (script-stringize var))
          (value.str (script-stringize value)))
      (script-entry.set-environ
        se
        (alist-cons var.str
                    value.str
                    (alist-delete var.str (script-entry.get-environ se)))
      )))

  (define (script-entry.get-cmd-list se)
    (script-entry.get-field se 'cmd-list))

  (define (script-entry.set-cmd-list se cmd-list)
    (script-entry.set-field se 'cmd-list cmd-list))

  (define (script-entry.get-type se)
    (script-entry.get-field se 'type))

  (define (script-entry.set-type se type)
    (script-entry.set-field se 'type type))

  (define (cmd-builder? f)
    (and (pair? f)
         (eq? (car f) 'cmd-builder)
         (eq? (length f) 3)))

  (define (cmd-builder-new)
    `(cmd-builder () ,(script-entry.set-type (script-entry) 'work)))

  (define (cmd-builder.script f) (cadr f))
  (define (cmd-builder.entry f) (caddr f))

  (define (cmd-builder.promote-cmd f)
    `(cmd-builder ,(append (cmd-builder.script f)
                           (list (cmd-builder.entry f)))
                  ,(script-entry.set-cmd-list (script-entry.set-type (cmd-builder.entry f) 'work)
                                              '())
      ))

  (define (cmd-builder.promote-work f)
    `(cmd-builder ,(cmd-builder.script f)
                  ,(script-entry.set-type (cmd-builder.entry f) 'cmd)))

  (define (cmd-builder.rewrite-entry f new_entry)
    `(cmd-builder ,(cmd-builder.script f)
                  ,new_entry))

  (define (cmd-builder.work-ready f)
    (if (eq? (script-entry.get-type (cmd-builder.entry f)) 'work)
        f
        (cmd-builder.promote-cmd f)))

  (define (cmd-builder.cmd-ready f)
    (if (eq? (script-entry.get-type (cmd-builder.entry f)) 'cmd)
        f
        (cmd-builder.promote-work f)))

  (define (cmd-builder.apply-work f func)
    (let ((workable (cmd-builder.work-ready f)))
      (cmd-builder.rewrite-entry workable (func (cmd-builder.entry workable)))
     ))

  (define (cmd-builder.apply-cmd f func)
    (let ((cmd-ready (cmd-builder.cmd-ready f)))
      (cmd-builder.rewrite-entry cmd-ready (func (cmd-builder.entry cmd-ready)))
     ))

  (define (cmd-builder.add-cmd f cmd)
    (cmd-builder.apply-cmd f
                           (lambda (ce)
                             (script-entry.set-cmd-list ce
                                                        (append (script-entry.get-cmd-list ce)
                                                                (list cmd))))))

  (define (script.cd new-dir)
    (lambda (builder)
      (cmd-builder.apply-work builder
                              (lambda (we) (script-entry.set-cd we new-dir)))
      ))

  (define (script.cmd cmd-list)
    (lambda (builder)
      (cmd-builder.add-cmd builder `(cmd . ,cmd-list))))

  (define (script.set-env var value)
    (lambda (builder)
      (cmd-builder.apply-work builder
                              (lambda (we) (script-entry.set-env we var value)))
    ))


)
