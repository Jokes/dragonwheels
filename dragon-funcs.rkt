#lang racket

(require "link-list.rkt" "dragon-list.rkt")
(provide print-url print-list-f print-list-m framed-list-f framed-list-m)

(define (url mother father [base "http://trollslum.net/fr/"])
  (string-append base "?id=" 
                 (apply string-append 
                        (map 
                         (λ (a b) (string-append (number->string a) "," (number->string b) ","))
                         (Dragon-numbers mother) (Dragon-numbers father)))
                 "1"))

(define (print-url mother father)
  (displayln
   (string-append (url mother father) "\t\t" (name-dragon mother) "/" (name-dragon father))))

(define (print-list-f mother [mlist male-dragons])
  (for-each (λ (father) (print-url mother father)) mlist))
(define (print-list-m father [flist female-dragons])
  (for-each (λ (mother) (print-url mother father)) flist))

(define (framed-list-f mother [mlist male-dragons])
  (framed-list (string-append "dragons/" (name-dragon mother) ".html")
               (string-append "dragons/_link-" (name-dragon mother) ".html")
               (map (λ (father) (url mother father)) 
                    mlist)
               (map (λ (father) (string-append (name-dragon mother) "/" (name-dragon father))) 
                    mlist)))
(define (framed-list-m father [flist female-dragons])
  (framed-list (string-append "dragons/" (name-dragon father) ".html")
               (string-append "dragons/_link-" (name-dragon father) ".html")
               (map (λ (mother) (url mother father)) 
                    flist)
               (map (λ (mother) (string-append (name-dragon mother) "/" (name-dragon father))) 
                    flist)))
