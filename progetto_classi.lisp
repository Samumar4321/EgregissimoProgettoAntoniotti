
;;
(defun flatten (L)
    (if (null L)
        NIL
        (if (atom (first L))
            (cons (first L) (flatten (rest L)))
            (append (flatten (first L)) (flatten (rest L))))))


;; Definisce una hash table globale per memorizzare le specifiche di classe
(defparameter *classes-specs* (make-hash-table))


;; Funzione che aggiunge le specifiche di classe alla hash table
(defun add-class-spec (name class-spec) 
  (setf (gethash name *classes-specs*) class-spec))


;; Funzione che restituisce le specifiche di una classe dalla hash table
(defun get-class-spec (name) 
  (gethash name *classes-specs*))


;; Funzione che definisce la classe
(defun def-class (class-name parents &rest parts)
  ;; Verifica la validita'  di class-name e parents
  (cond ((not (symbolp class-name)) (error (format T "ERROR --> Class-name: ~S is not a symbol ~%" class-name)))
        ((not (listp parents)) (error (format  "ERROR --> Parents: ~S is not a valid list ~%" parents))))
  
  ;;ereditarietà
  (let ((all-parents (remove-duplicates
                      (flatten (mapcar #'inherit-parents parents))
                      :test #'equal
                      :from-end
                      T)))
    (let ((parents-fields (remove-duplicates 
                          (inherit-fields all-parents)
                          :test #'fields-equal
                          :from-end
                          t)))
      
      ;; Inizializza la lista di campi e metodi
      (if (null parts)
          (let ((class-structure `(class, class-name, all-parents, parents-fields, ())))
            (add-class-spec class-name class-structure))
        ;; ELSE
        (let ((fields (check-fields (car parts)))
              (methods (check-methods (cadr parts))))          
          ;; Analizzo i fields
          (let ((all-fields (remove-duplicates
                             (append 
                              (mapcar 
                               (lambda (elem)
                                 (cond ((null elem) ())
                                       ((not (listp elem)) (error 
                                                            "Errore elem not list"))
                                       ((list elem) (if (not 
                                                         (null 
                                                          (assoc 
                                                           (first elem) 
                                                           parents-fields)))
                                                        (let ((found 
                                                               (assoc 
                                                                (first elem) 
                                                                parents-fields)))
                                                          (if (equal 
                                                               (third elem) 
                                                               'T)
                                                              (check-field 
                                                               (list 
                                                                (first elem) 
                                                                (second elem) 
                                                                (third found)))
                                                            ;;ELSE
                                                            (cond ((and (is-class (third found))
                                                                        (is-class (third elem))
                                                                        (equal (third found) (third elem)))
                                                                   elem)
                                                                  ((subtypep 
                                                                    (third elem) 
                                                                    (third found))
                                                                   elem)
                                                                  (T (error 
                                                                      (format t 
                                                                              "ERROR --> Elem: ~S is not of correct type~%" 
                                                                              elem))))))
                                                      ;;ELSE
                                                      elem))))
                               
                               fields)
                              parents-fields)
                             :test #'fields-equal
                             :from-end
                             T)))
            
            ;; Costruisce la struttura della classe
            (write fields)
            (terpri)
            (write parents-fields)
            (terpri)
            (write all-fields)
            (let ((class-structure `(class, class-name, all-parents, all-fields, methods)))
              ;; Aggiunge la specifica della classe al registro globale
              (add-class-spec class-name class-structure)))))))
  ;; Ritorna il nome della classe
  class-name)
  
(defun fields-equal (field1 field2)
  (cond ((equal field1 field2) T)
        ((equal (first field1) (first field2)) T)
        (T NIL)))


(defun check-fields (fields)
  (cond ((not (listp fields)) (error (format t "ERROR --> Fields: ~S is not a list ~%" fields)))
        ((equal (car fields) 'fields) (remove-duplicates 
                                       (mapcar #'check-field (rest fields)) 
                                       :test #'fields-equal 
                                       :from-end 
                                       t))
        (T (error (format 
                   t 
                   "ERROR --> Fields: ~S is not a valid fields list, valid list = (fields &optional field ...) ~%"
                   fields)))))


(defun check-field (item)
  (cond ((null item) ())
        ((equal (length item) 1) (list 
                                  (first item) 
                                  NIL 
                                  T))
        ((equal (length item) 2) (list 
                                  (car item) 
                                  (second item) 
                                  T))
        ((equal (length item) 3) (let ((name (first item))
                                       (value (second item))
                                       (type (third item)))
                                   (symbolp name)                                  
                                   (cond ((is-class type) (cond ((null value)
                                                                 (list 
                                                                  name
                                                                  value
                                                                  type))
                                                                ((is-instance value)
                                                                 (list
                                                                  name
                                                                  value
                                                                  type))
                                                                (T (error 
                                                                    (format
                                                                     t
                                                                     "ERROR --> Value: ~S in Field: ~S is not an instance~%" 
                                                                     value 
                                                                     name)))))         
                                         ((typep value type) (list 
                                                              name 
                                                              value 
                                                              type))
                                         (T (error (format
                                                     t
                                                     "ERROR --> Value: ~S in Field: ~S is of incorrect type ~%" 
                                                     value 
                                                     name))))))
        (T (error (format 
                   t 
                   "ERROR --> Field: ~S is not a valid field, valid field = (name &optional value &optional type) ~%" 
                   item)))))


(defun methods-equal (met1 met2)
  (cond ((equal met1 met2) T)
        ((equal (first met1) (first met2)) (if (equal (length (second met1)) (length (second met2)))
                                                   T
                                                 NIL))
        (T NIL)))


(defun check-methods (methods)
  (cond ((not (listp methods)) (error (format t "ERROR --> Methods ~S is not a list ~%" parents)))
        ((equal (car methods) 'methods) (remove-duplicates (mapcar #'check-method (rest methods)) :test #'methods-equal :from-end t))
        (T (error (format 
                   t 
                   "ERROR --> Methods ~S is not a valid methods list, a valid list = (methods &optional methods ...) ~%" 
                   methods)))))


(defun check-method (item)
  (cond ((null item) ())        
        ((equal (length item) 3) (let ((name (first item))
                                       (args (second item))
                                       (body (third item)))
                                   (symbolp name)                                                 
                                   (list 
                                    name 
                                    args 
                                    body))) 
        (T (error (format 
                   t 
                   "ERROR --> Method: ~S is not a valid method, a valid method = (name (args) (body)) ~%" 
                   item)))))


;; Funzione che restituisce T se class name esiste altrimenti nil
(defun is-class (class-name)
  (cond ((not (symbolp class-name)) (error (format 
                                            t 
                                            "ERROR --> Class-name: ~S is not a symbol ~%" 
                                            class-name)))
        ((equal class-name T) T)
        ((not (null (get-class-spec class-name))) T)
        (T NIL)))


(defun field (instance field-name)
  (cond ((not (symbolp field-name)) (error (format 
                                            t 
                                            "ERROR --> Field-name: ~S is not a symbol ~%" 
                                            field-name)))
        ((not (is-instance instance)) (error (format 
                                              t 
                                              "ERROR --> Instance: ~S is not a valid instance ~%" 
                                              instance)))
        ((is-instance instance) (if (null (assoc field-name (third instance))) 
                                    (error (format 
                                            t 
                                            "ERROR --> Field-name: ~S is not in Instance: ~A ~%" 
                                            field-name
                                            instance))
                                  ;; ELSE
                                  (second (assoc field-name (third instance))))) 
        (T (error (format 
                   t 
                   "ERROR -->(field ~S ~S) unhandled ~%" 
                   instance
                   field-name)))))


(defun field* (instance field-name)
  (write field-name)
  (cond ((not (listp field-name)) (error (format 
                                          t 
                                          "ERROR --> Field-name: ~S is not a list ~%" 
                                          field-name)))
        ((equal (length field-name) 0) (error (format 
                                          t 
                                          "ERROR --> Field-name: ~S cannot be an empty list ~%" 
                                          field-name)))
        ((equal (length field-name) 1) (field instance (first field-name)))
        ((listp field-name) (field* (field instance (first field-name)) (rest field-name)))        
        (T (error (format 
                   t 
                   "ERROR -->(field ~S ~S) unhandled ~%" 
                   instance
                   field-name)))))


(defun get-class-fields(class-name)
  (cond ((not (is-class class-name)) (error (format 
                                             t 
                                             "ERROR --> Class-name: ~S is not a defined class ~%" 
                                             class-name)))
        ((is-class class-name) (fourth (get-class-spec class-name)))
        (T (error (format 
                   t 
                   "ERROR --> (get-class-fields ~S) unhandled ~%" 
                   class-name)))))


(defun get-class-field (class-name field-name)
  (cond ((not (symbolp field-name)) (error (format 
                                             t 
                                             "ERROR --> Field-name: ~S is not a symbol ~%" 
                                             field-name)))
        ((not (is-class class-name)) (error (format 
                                             t 
                                             "ERROR --> Class-name: ~S is not a defined class ~%" 
                                             class-name)))
        ((is-class class-name) (if (assoc field-name (get-class-fields class-name)) 
                                   (assoc field-name (get-class-fields class-name))
                                 ;; ELSE
                                 ;;(error (format 
                                 ;;        t 
                                 ;;        "ERROR --> Field-name: ~A is not a defined field in ~A ~%" 
                                 ;;        field-name 
                                 ;;        class-name))))
                                 NIL))
        (T (error (format 
                   t 
                   "ERROR --> (get-class-field ~S ~S) unhandled ~%" 
                   class-name 
                   field-name)))))


(defun get-class-methods(class-name)
  (cond ((not (symbolp class-name)) (error (format t "ERROR --> Class-name: ~S is not an symbol ~%" class-name)))
        ((not (is-class class-name)) (error (format t "ERROR --> Class-name: ~S is not a defined class ~%" class-name)))
        ((is-class class-name) (fifth (get-class-spec class-name)))
        (T (error (format 
                   t 
                   "ERROR --> (get-class-methods ~S) unhandled ~%" 
                   class-name)))))


(defun get-class-method (class-name method-name)
  (cond ((not (symbolp method-name)) 
         (error (format 
                 t 
                 "ERROR --> Method-name: ~S is not an symbol ~%" 
                 method-name)))
        ((not (is-class class-name)) 
         (error (format 
                 t 
                 "ERROR --> Class-name: ~S is not a defined class ~%" 
                 class-name)))
        ((is-class class-name) 
         (if (assoc method-name (get-class-methods class-name)) 
             (assoc 
              method-name 
              (get-class-methods class-name))
           ;;ELSE
           (error (format 
                   t 
                   "ERROR --> Method: ~S is not a valid method~%" 
                   method-name))))
        (T (error (format 
                   t 
                   "ERROR --> (get-class-method ~S ~S) unhandled~%" 
                   class-name 
                   method-name)))))


(defun make (class-name &rest slots)
  (write slots)
  (cond ((not (is-class class-name)) (error (format 
                                             t 
                                             "ERROR --> Class: ~S is not a defined class~%" 
                                             class-name))) 
        (T ;; Create a new instance of a class
           ;; Ottiene le specifiche della classe
           (let ((class-spec (get-class-spec class-name)))
             ;; Se slot null fai qualcosa, altrimenti altro
             (cond ((null slots) (list 
                                  'oolinstance 
                                  class-name 
                                  (mapcar #'remove-type (fourth class-spec)))) 
                   ((if (listp slots) 
                        ;; se i fields sono pari (nome-field value) allora ok altrimenti ne manca 1
                        (if (evenp (length slots))
                            (list 
                             'oolinstance 
                             class-name 
                             (remove-duplicates (append (rec-instance-modify-fields class-name slots) (fourth class-spec)) 
                                                :test #'fields-equal 
                                                :from-end
                                                t))
                          ;; ELSE
                          (error (format 
                                   t 
                                   "ERROR --> Fields: ~S one or more fields are not correctly defined ~%" 
                                   slots)))
                      ;; ELSE
                      (error (format 
                               t 
                               "ERROR --> Fields: ~S is not a list ~%" 
                               slots)))))))))

(defun remove-type (field)
  (cond ((null field) ())
        ((and (listp field) 
              (equal (length field) 3)) (list (first field)
                                              (second field)))
        (T (error (format 
                   t 
                   "ERROR --> (remove-type ~S) unhandled~%" 
                   field)))))



(defun is-instance (instance &optional class-name)  
  (cond ((or (null class-name) (equal class-name T)) (check-instance instance))
        ((not (is-class class-name))  (error (format 
                                              t 
                                              "ERROR --> Class-name: ~S is not a defined class ~%" 
                                              class-name)))
        ((is-class class-name) 
         (if (not (check-instance instance)) (error (format 
                                                     t
                                                     "ERROR --> Instance: ~S is not a valid instance~%"
                                                     instance))
           ;; ELSE
           (if (null (member class-name (third (get-class-spec (second instance))))) 
               (if (equal class-name (second instance))
                   T
                 ;;ELSE
                 NIL)
             ;;ELSE
             T)))
        (T (error (format 
                   t 
                   "ERROR --> (is-instance ~S ~S) unhandled~%" 
                   instance
                   class-name)))))


(defun check-instance (instance) 
  (if (and (listp instance) 
           (not (null instance)) 
           (equal (length instance) 3)
           (equal (first instance) 'oolinstance))
      (let ((class-fields (get-class-fields (second instance)))
            (instance-fields (third instance)))
        (is-instance-app instance-fields class-fields))           
    ;; ELSE
    NIL))


(defun is-instance-app (instance-fields class-fields) 
(cond ((not (listp instance-fields)) NIL)
      ((not (listp class-fields)) NIL)
      ((and (null instance-fields) (not (null class-fields))) NIL)
      ((and (not (null instance-fields)) (null class-fields)) NIL)
      ((and (null instance-fields) (null class-fields)) T)
      (T (let ((instance-field (first instance-fields)))
           (if (null (assoc (first instance-field) class-fields))
               NIL
             ;; ELSE
             (if (is-instance-app (rest instance-fields) 
                                  (remove-if (lambda (elem) 
                                               (fields-equal instance-field elem)) 
                                             class-fields))
                 T
               ;; ELSE
               NIL))))))
      

;; Prende i field di una classe, recupera uno specifico field, in particolare il suo type,
;; e controlla che new-value sia un valore ammesso
;; ritorna -1 se field-name non è un field della classe class-name
(defun enable-modify(class-name field-name new-value)
  (let ((type (third (get-class-field class-name field-name))))
       (if (null type)
           -1
         ;; ELSE DA FARE modifica per classi
         (cond ((is-class type) (is-instance new-value type))
               ((typep new-value type) T)
               (T NIL)))))


(defun rec-instance-modify-fields (class-name slots)
  (if (null slots) ()
    ;; ELSE
    (let ((field-name (first slots))
          (field-value (second slots))
          (rest  (cddr slots)))
      (case (enable-modify class-name field-name field-value)
        ('NIL (error (format 
                      t 
                    "ERROR --> Field-name: ~S is not of correct type in Class: ~S ~%" 
                    field-name
                    class-name)))
        ('-1 (error (format 
                     t 
                     "ERROR --> Field-name: ~S is not a defined field in Class: ~S ~%" 
                     field-name
                     class-name)))
        (T (append 
             (list (list field-name field-value))
             (rec-instance-modify-fields class-name rest)
             ))))))


(defun inherit-fields (classes)
  (cond ((not (listp classes)) (Error))
        ((null classes) ())
        ((listp classes) (append 
                          (get-class-fields (first classes))
                          (inherit-fields (rest classes))))))


(defun inherit-parents (class-name)
  (remove-duplicates
         (flatten (inherit-parents-app class-name))
         :test #'equal
         :from-end
         t))


(defun inherit-parents-app (class-name)
  (cond ((null class-name) ())
        ((not (is-class class-name)) (error (format 
                                 t 
                                 "ERROR --> Parent: ~S is not a defined class~%"
                                 class-name)))
        ((is-class class-name) (let ((parents (third (get-class-spec class-name)))) ;;recupero la lista dei miei parents
                                 (append (list class-name) (mapcar #'inherit-parents parents))
                                 ;;eseguiti ricorsivamente su ogni parent(mapcar)
                                 ;;recupera i tuoi fields
                                 ;;appendi ai tuoi fields i fields dei parent ottenuti da (mapcar)
                                 ;; RICORDA CAZZO DI DEPTH FIRST PRIMA TUTTO DEL PRIMO PADRE E DEI SUOI PADRI
                                 ))
        (T (error (format 
                   t 
                   "ERROR --> (inherit-fields ~S)unhandled~%" 
                   class-name)))))

#|
(defun process-method (method-name method-spec)
#| ... and here a miracle happens ... |#
;;1) crea funzione lambda che prende almeno 1 cosa in input ovvero 'this' (istanza), 
;;   controlla che l'istanza sia una istanza
;;   recupera la classe a cui appartiene
;;   recupera il metodo corretto in base al nome e poi recupera gli args e il body
;;2) associa tale lambda al nome del methodo
(eval (rewrite-method-code method-name method-spec)))
|#

;; method-name --> nome metodo (symbolo)
;; method-spec --> ((args) (body))
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
        (lambda
                (this &rest args)
          (if (not (is-instance this))
              (error "Error --> instance not valid")
            ;; ELSE
            (let ((method-specs (get-method (second this) method-name)))
              (apply
               (cddr method-specs)
               (append (list this) args)
               )
              ))))
  (eval (rewrite-method-code method-name method-spec)))


;; Funziona
(def-class 'person nil '(fields (name "Eve" string) (age 21 integer)) '(methods))

;; Funziona
(def-class 'student '(person) 
           '(fields (name "Eva Lu Ator" string) (university "Berkeley" string)) 
           '(methods (bau-bau () (write "bau bau")) 
                     (bau-bau () (write "miao miao"))
                     (talk (&optional (out *standard-output*)) (format 
                                                                out
                                                                "My name is ~A~%My age is ~D~%" 
                                                                (field this 'name) 
                                                                (field this 'age)))))

;;OK
(def-class 'animale2 '(person) '(fields (zampe 16 integer) (name "Animale2")) '(methods))

;OK
(def-class 'animale '(student animale2) '(fields (zampe 4 integer) (name "Animale" string)) '(methods))

(def-class 'padrone '(student) '(fields (animale1 NIL animale)) '(methods))
;; Funziona
(is-class 'person)

(defparameter eve (make 'person))

(defparameter adam (make 'person 'name "Adam"))

(defparameter s1 (make 'student 'name "Eduardo De Filippo" 'age 108))

(defparameter s2 (make 'student))

(defparameter s3 (make 'student 'age 42))










