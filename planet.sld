(define-library (planet)
  (export make-feed
          feed-id
          feed-title
          feed-url
          entry-author
          entry-title
          entry-content-xhtml
          entry-iso-date
          entry-us-english-date
          entry-english-year-and-month
          planet-refresh-feed
          planet-all-entries-ever
          planet-n-newest-entries
          planet-group-entries-by-date
          write-html-file)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (srfi 1)
          (srfi 13)
          (srfi 132))
  (cond-expand
   (chicken
    (import (only (chicken file)
                  create-directory
                  rename-file)
            (only (http-client)
                  with-input-from-request)
            (only (sxml-transforms)
                  SXML->HTML)
            (only (traversal)
                  group-by)
            (only (atom)
                  content-xhtml
                  entry-author
                  entry-content
                  entry-title
                  entry-updated
                  feed-entries
                  read-atom-feed))))
  (begin

    (define (take-at-most list n)
      (let loop ((n n) (new-list '()) (list list))
        (if (or (null? list) (< n 1)) (reverse new-list)
            (loop (- n 1) (cons (car list) new-list) (cdr list)))))

    (define (disp . xs)
      (for-each display xs)
      (newline))

    (define (edisp . xs)
      (parameterize ((current-output-port (current-error-port)))
        (apply disp xs)))

    (define (writeln x)
      (write x)
      (newline))

    (define (copy-binary-port input output)
      (let ((buffer (make-bytevector (* 64 1024) 0)))
        (let loop ()
          (let ((n-read (read-bytevector! buffer input)))
            (if (eof-object? n-read) #t
                (begin (write-bytevector buffer output 0 n-read)
                       (loop)))))))

    (define (copy-textual-port input output)
      (define buffer-size (* 64 1024))
      (let loop ()
        (let ((chunk (read-string buffer-size input)))
          (or (eof-object? chunk)
              (begin (write-string chunk output)
                     (loop))))))

    (define (supersede-text-file filename thunk)
      (let ((new-filename (string-append filename ".new")))
        (with-output-to-file new-filename thunk)
        (rename-file new-filename filename #t)))

    (define (path-append . paths)
      (string-join paths "/"))

    (define english-month-names
      (vector
       "January"
       "February"
       "March"
       "April"
       "May"
       "June"
       "July"
       "August"
       "September"
       "October"
       "November"
       "December"))

    ;;;

    (define-record-type feed
      (make-feed id title url)
      feed?
      (id    feed-id)
      (title feed-title)
      (url   feed-url))

    (define (feed-cache-file feed cache-directory)
      (path-append cache-directory (string-append (feed-id feed) ".xml")))

    (define (planet-refresh-feed feed cache-directory)
      (let ((cache-file (feed-cache-file feed cache-directory)))
        (create-directory cache-directory)
        (with-input-from-request
         (feed-url feed)
         #f
         (lambda ()
           (supersede-text-file
            cache-file (lambda ()
                         (copy-textual-port (current-input-port)
                                            (current-output-port))))))))

    (define (read-feed-from-cache feed cache-directory)
      (let ((cache-file (feed-cache-file feed cache-directory)))
        (call-with-port (open-binary-input-file cache-file) read-atom-feed)))

    (define (entry-content-xhtml entry)
      (content-xhtml (entry-content entry)))

    (define (entry-rfc3339-digits n entry)
      (let ((rfc3339 (entry-updated entry)))
        (if (and rfc3339 (>= (string-length rfc3339) n))
            (substring rfc3339 0 n)
            (error "No date"))))

    (define (entry-iso-date entry) (entry-rfc3339-digits 10 entry))

    (define (entry-iso-month entry) (entry-rfc3339-digits 7 entry))

    (define (entry-y-m-d-integers entry)
      (let* ((yyyy-mm-dd (entry-iso-date entry))
             (year  (string->number (substring yyyy-mm-dd 0 4)))
             (month (string->number (substring yyyy-mm-dd 5 7)))
             (day   (string->number (substring yyyy-mm-dd 8 10))))
        (values year month day)))

    (define (entry-us-english-date entry)
      (let-values (((year month day) (entry-y-m-d-integers entry)))
        (let ((month-name (vector-ref english-month-names (- month 1))))
          (string-append
           (number->string day) " " month-name " " (number->string year)))))

    (define (entry-english-year-and-month entry)
      (let-values (((year month _) (entry-y-m-d-integers entry)))
        (let ((month-name (vector-ref english-month-names (- month 1))))
          (string-append month-name " " (number->string year)))))

    (define (entry-iso-date<? entry1 entry2)
      (string<? (entry-iso-date entry1)
                (entry-iso-date entry2)))

    (define (planet-all-entries-ever feeds cache-directory)
      (let ((entries (append-map feed-entries
                                 (map (lambda (feed)
                                        (read-feed-from-cache
                                         feed cache-directory))
                                      feeds))))
        (reverse (list-stable-sort entry-iso-date<? entries))))

    (define (planet-n-newest-entries n feeds cache-directory)
      (take-at-most (planet-all-entries-ever feeds cache-directory) n))

    (define (planet-group-entries-by-date entries)
      (group-by entry-iso-date entries))

    (define (planet-group-entries-by-month entries)
      (group-by entry-iso-month entries))

    (define (write-html-file filename sxml)
      (supersede-text-file
       filename (lambda ()
                  (write-string "<!DOCTYPE html>")
                  (SXML->HTML sxml)
                  (newline))))))
