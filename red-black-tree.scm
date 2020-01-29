;; File: red-black-tree.scm
;; By Amine Sami (aminesami) - 2020

(define-library (binary-search-tree red-black-tree)
  (import (scheme base) (scheme cxr))
  (export init-operations empty-tree
          remove-color-tag tree->list)

  (begin
    (define (make-node left content color right)
      (list left content color right))
    (define (get-left node) (car node))
    (define (get-right node) (cadddr node))
    (define (get-content node) (cadr node))
    (define (get-color node) (if (null? node) 'black (caddr node)))

    (define (balance color left content right)
      ;; helper id to reconstruct the node
      (define (id) (make-node left content color right))
      ;; helper return to reconstruct the balance tree
      (define (return ll x lr y rl z rr)
        ;; root is red and both child black
        (make-node (make-node ll x 'black lr)
                   y
                   'red
                   (make-node rl z 'black rr)))
        ;; looking for red-red violation on the left branch
      (define (try-left ll lr)
        (cond ((eq? (get-color ll) 'red)
               ;;        o
               ;;     //   \
               ;;  left    right
               ;;  //  \
               ;; ll   lr
               ;;
               ;; v v v v v v v v
               ;;
               ;;   left
               ;;  // \\
               ;; ll    o
               ;;     /  \
               ;;   lr   right
               (return (get-left ll) (get-content ll) (get-right ll)
                       (get-content left)
                       lr content right))
              ;;            o
              ;;         //   \
              ;;       left   right
              ;;      /  \\
              ;;     ll   lr
              ;;
              ;; v v v v v v v v v v v v
              ;;
              ;;           lr
              ;;      //       \\
              ;;   left          o
              ;;   /   \       /   \
              ;; ll  (/ lr) (lr \)  right
              ((eq? (get-color lr) 'red)
               (return ll (get-content left) (get-left lr)
                       (get-content lr)
                       (get-right lr) content right))
              ;; no violation on left branch so look in right branch
              ((eq? (get-color right) 'red)
               (try-right (get-left right) (get-right right)))
              (else (id))))
      (define (try-right rl rr)
        (cond ((eq? (get-color rl) 'red)
               ;;           o
               ;;         /  \\
               ;;      left  right
               ;;            //  \
               ;;           rl   rr
               ;;
               ;; v v v v v v v v v v v v
               ;;
               ;;          rl
               ;;      //       \\
               ;;     o         right
               ;;   /    \      /    \
               ;; left (/ rl) (rl \)  rr
               (return left content (get-left rl)
                       (get-content rl)
                       (get-right rl) (get-content right) rr))
              ((eq? (get-color rr) 'red)
               ;;         o
               ;;       /   \\
               ;;    left  right
               ;;           / \\
               ;;          rl  rr
               ;;
               ;; v v v v v v v v v
               ;;
               ;;       right
               ;;     //     \\
               ;;     o      rr
               ;;   /   \
               ;; left   rl
               (return left content rl
                       (get-content right)
                       (get-left rr) (get-content rr) (get-right rr)))
              (else (id))))
      (cond ((eq? color 'red) (id))
            ((eq? (get-color left) 'red)
             (try-left (get-left left) (get-right left)))
            ((eq? (get-color right) 'red)
             (try-right (get-left right) (get-right right)))
            (else (id))))

    (define (init-operations =? <?)
      ;; simple dichotomic search
      (define (search x y node)
        (if (null? node)
            (and y (=? x y) y)
            (let ((z (get-content node)))
              (if (<? x z)
                  (search x y (get-left node))
                  (search x z (get-right node))))))
      ;; insert uses a helper ins to propagate balance 
      ;; and insertion throught the tree
      (define (insert x tree)
        (define (ins node)
          (if (null? node)
              (make-node '() x 'red '()) ;; new nodes are always red
              (let ((y (get-content node)))
                (cond ((<? x y)
                       (balance (get-color node)
                                (ins (get-left node))
                                y
                                (get-right node)))
                      ((=? x y)
                       (make-node (get-left node)
                                  x (get-color node)
                                  (get-right node)))
                      (else (balance (get-color node)
                                     (get-left node)
                                     y
                                     (ins (get-right node))))))))
        (let ((res (ins tree)))
          (make-node (get-left res)
                     (get-content res) 'black ;; root is always black
                     (get-right res))))
      (values
       (lambda (x tree) (search x #f tree))
       (lambda (x tree) (insert x tree))))

    (define empty-tree '())
    
    (define (remove-color-tag root)
      (if (null? root)
          '()
          (list (remove-color-tag (get-left root))
                (get-content root)
                (remove-color-tag (get-right root)))))
    
    (define (tree->list root)
      (if (null? root)
          '()
          (let ((rest (tree->list (get-right root))))
            (append (tree->list (get-left root))
                    (cons (get-content root) rest)))))))
