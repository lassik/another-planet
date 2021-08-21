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
            (h2 ,(entry-us-english-date (first entries)))
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

(define (refresh-cache)
  (for-each (lambda (feed)
              (disp "Refreshing " (feed-url feed))
              (planet-refresh-feed feed cache-directory))
            feeds))

(define (generate-from-cache)
  (disp "Writing index.html")
  (write-html-file "index.html" (front-page)))

(define (main)
  (cond ((null? (command-args))
         (refresh-cache)
         (generate-from-cache))
        ((and (= 1 (length (command-args)))
              (string=? "-n" (first (command-args))))
         (generate-from-cache))
        (else
         (error "Usage: ./planet-scheme [-n]"))))

(main)
