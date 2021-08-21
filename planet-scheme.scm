(import (scheme base) (srfi 193) (planet))

(define (disp . xs) (for-each display xs) (newline))

(define first car)

(define cache-directory ".cache")

(define base-url "https://planet.scheme.org/")

(define title "Planet Scheme")

(define description
  (string-append
   "Scheme blog aggregator. Collects blog posts from individuals"
   " and projects around the Scheme community."))

(define feeds
  (list
   (make-feed
    "emacsninja"
    "Vasilij Schneidermann"
    "https://emacsninja.com/scheme.atom")
   (make-feed
    "idiomdrottning"
    "Idiomdrottning"
    "https://idiomdrottning.org/blog/programs")))

(define (front-page)
  `(html
    (@ (lang "en"))
    (head
     (meta (@ charset "UTF-8"))
     (title ,title)
     (link (@ (rel "stylesheet") (href "/schemeorg.css")))
     (meta (@ (name "viewport")
              (content "width=device-width, initial-scale=1")))
     (meta (@ (name "description")
              (content ,description))))
    (body
     (h1 ,title)
     ,@(map
        (lambda (entries)
          `(section
            (h2 ,(entry-english-year-and-month (first entries)))
            ,@(map (lambda (entry)
                     `(article
                       (h3 ,(entry-title entry))
                       ,(or (entry-content-xhtml entry)
                            `(div))
                       (p "by " ,(entry-author entry))))
                   entries)))
        (planet-group-entries-by-date
         (planet-n-newest-entries 10 feeds cache-directory))))))

;;

(define (refresh-cache all?)
  (for-each (lambda (feed)
              (let ((new? (not (feed-cached? feed cache-directory))))
                (when (or all? new?)
                  (disp "Refreshing " (feed-url feed))
                  (planet-refresh-feed feed cache-directory))))
            feeds))

(define (generate-from-cache)
  (disp "Writing www/index.html")
  (write-html-file "www/index.html" (front-page)))

(define (only-command-line-arg? arg)
  (and (= 1 (length (command-args)))
       (string=? arg (first (command-args)))))

(define (main)
  (cond ((null? (command-args))
         (refresh-cache #t)
         (generate-from-cache))
        ((only-command-line-arg? "-n")
         (refresh-cache #f)
         (generate-from-cache))
        ((only-command-line-arg? "-o")
         (generate-from-cache))
        (else
         (error "Usage: ./planet-scheme [-n]"))))

(main)
