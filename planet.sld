(define-library (planet)
  (export make-feed
          feed-id
          feed-title
          feed-url
          entry-title
          entry-iso-date
          entry->html
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
          (srfi 132)
          (srfi 193))
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
                  read-atom-feed
                  feed-entries
                  entry-updated
                  entry-title))))
  (begin

    (define (take-at-most list n)
      (let loop ((n n) (new-list '()) (list list))
        (if (< n 1) (reverse new-list)
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

    (define (path-append . paths)
      (string-join paths "/"))

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
           (let ((temp-file (string-append cache-file ".new")))
             (call-with-port
              (open-output-file temp-file)
              (lambda (output)
                (copy-textual-port (current-input-port) output)))
             (rename-file temp-file cache-file #t))))))

    (define (read-feed-from-cache feed cache-directory)
      (let ((cache-file (feed-cache-file feed cache-directory)))
        (call-with-port (open-binary-input-file cache-file) read-atom-feed)))

    (define (entry-iso-date entry)
      (let ((rfc3339 (entry-updated entry)))
        (if (and rfc3339 (>= (string-length rfc3339) 10))
            (substring rfc3339 0 10)
            (error "No date"))))

    (define (entry-iso-date<? entry1 entry2)
      (string<? (entry-iso-date entry1)
                (entry-iso-date entry2)))

    (define (entry->html entry)
      `(h3 ,(entry-title entry)))

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

    (define (write-html-file filename sxml)
      (let ((new-filename (string-append filename ".new")))
        (with-output-to-file new-filename
          (lambda ()
            (write-string "<!DOCTYPE html>")
            (SXML->HTML sxml)
            (newline)))
        (rename-file new-filename filename #t)))))
