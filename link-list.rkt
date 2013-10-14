#lang racket

(require (prefix-in h: html)
         (prefix-in x: xml))

(provide link-list
         framed-list)

(define (link-list filename ulist tlist [target "_self"])
  (let ([outfile (open-output-file filename)])
    (x:write-xexpr
     `(html
       (head
        (meta ((content "text/html; charset=UTF-8") (http-equiv "Content-Type"))) "\n"
        (title "Link List") "\n")
       (body "\n"
             (ul . ,(map (λ (url title) `(li (a ((href ,url) (target ,target)) ,title))) 
                         ulist tlist))))
     outfile)
    (flush-output outfile)
    (close-output-port outfile)))

(define (framed-list filename listfilename ulist tlist)
  (link-list listfilename ulist tlist "main")
  (let ([outfile (open-output-file filename)])
    (x:write-xexpr
     `(html
       (head
        (meta ((content "text/html; charset=UTF-8") (http-equiv "Content-Type"))) "\n"
        (title "Framed List") "\n"
        (base ((target "main"))) "\n")
       (frameset ((cols "20%, 80%"))
                 (frame ((src ,(regexp-replace #rx".*/" listfilename ""))
                         (name "nav")))
                 (frame ((src "about:blank") (name "main")))))
     outfile)
    (flush-output outfile)
    (close-output-port outfile)))
