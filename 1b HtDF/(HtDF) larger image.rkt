;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |(HtDF) larger image|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Image Image -> Boolean
;; function returns true if image1 has a larger area than image2, assuming image1 and image2 are the same type of shape

;(define (image1_larger? img1 img2) true) ;stub

(check-expect (image1_larger? (square 4 "solid" "red") (square 8 "solid" "red")) false)
(check-expect (image1_larger? (square 8 "solid" "red") (square 4 "solid" "red")) true)
(check-expect (image1_larger? (square 4 "solid" "red") (square 4 "solid" "red")) false)
(check-expect (image1_larger? (triangle 20 "solid" "red") (triangle 4 "solid" "red")) true)
(check-expect (image1_larger? (star 8 "solid" "red") (star 4 "solid" "red")) true)
(check-expect (image1_larger? (square 4 "solid" "red") (circle 2 "solid" "red")) true)

;(define (image1_larger? img1 img2)   ;template
;  (...img1 img2)

 
(define (image1_larger? img1 img2)
  (> (* (image-height img1) (image-width img1))
     (* (image-height img2) (image-width img2))))
