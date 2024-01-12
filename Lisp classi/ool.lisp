;;;; -*- Mode: Lisp -*-

;;; ool.lisp


(defparameter errorFieldInstance
  "ERROR -> Field-name:~S is not in Instance:~A~%")

(defparameter errorInstance
  "ERROR -> Value:~S in Field:~S is not an instance~%")

(defparameter errorType
  "ERROR -> Value:~S in Field:~S is of incorrect type~%")


;;; flatten (List)
;;;
;;; List --> lista da appiattire
;;;
;;; Data una lista su piu' livelli, la appiattisce elimando tutte le parentesi
;;; interne e la ritorna
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


;;; elaborate-field (elem parents-fields)
;;;
;;; elem --> field da analizzare
;;; parents-fields --> lista composta da fields delle classi parenti
;;;
;;; Prende un field e una lista di fields e controlla se field e' presente
;;; nella lista:
;;; 1) se lo e' allora lo elabora tenendo conto del type
;;;    del field corrispondente nella lista
;;; 2) se non e' presente esegue controlli standard e lo ritorna
(defun elaborate-field (elem parents-fields)
  (cond ((null elem) ())
        ((not (listp elem)) (error 
                             "Errore elem is not a list"))
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
                               (cond
				 ((and
				   (is-class (third found))
                                   (is-class (third elem))
                                   (equal (third found) (third elem)))
                                  elem)
                                 ((subtypep 
                                   (third elem) 
                                   (third found))
                                  elem)
                                 (T
				  (error 
                                   (format 
				    t 
				    "ERROR -> Elem:~S is of incorrect type~%"
				    elem))))))
			 ;;ELSE
			 elem))))


;;; elaborate-method (elem parents-methods)
;;;
;;; elem --> metodo da analizzare
;;; parents-fields --> lista composta da metodi delle classi parenti
;;;
;;; Prende un metodo e una lista di metodi e controlla se il metodo e' presente
;;; nella lista:
;;; 1) se lo e' allora lo elabora tenendo conto che e' un override,
;;;    deve avere quindi lo stesso nome e gli stessi args, poi lo processa
;;; 2) se non e' presente esegue controlli standard e poi lo processa
(defun elaborate-method (elem parents-methods)
  (cond ((null elem) ())
        ((not (listp elem)) (error 
                             "Errore elem is not a list"))
        ((list elem) (if (not 
                          (null 
                           (assoc 
                            (first elem) 
                            parents-methods)))
                         (let ((found 
                                (assoc 
                                 (first elem) 
                                 parents-methods)))
                           (if (and (methods-equal elem found)
                                    (equal (second elem) (second found)))
                               (list (first elem)
                                     (second elem)
                                     (process-method (first elem) elem))
                               ;;ELSE
                               (T (error 
                                   (format 
                                    t 
                                    "ERROR -> Elem:~S while override method~%"
                                    elem)))))
			 ;;ELSE
			 (list (first elem)
                               (second elem)
                               (process-method (first elem)
                                               elem))))))



;;; def-class (class-name parents &rest parts)
;;;
;;; class-name --> simbolo che rappresenta il nome della classe
;;; parents --> lista composta da nomi di classi padri
;;; parts --> parametro opzionale, lista che rappresenta fields e metodi
;;;
;;; Dopo controlli standard, ricava la lista completa dei parents e
;;; recupera i loro fields e metodi.
;;; Se parts = null, allora definisci subito la classe,
;;; altrimenti recupera metodi e fields e
;;; comparali con i corrispetivi dei parents e poi definisci la classe
(defun def-class (class-name parents &rest parts)
  (cond ((not (symbolp class-name))
	 (error
	  (format
	   t
	   "ERROR -> Class-name:~S is not a symbol~%"
	   class-name)))
        ((not (listp parents))
	 (error
	  (format
	   t
	   "ERROR -> Parents:~S is not a valid list~%"
	   parents)))
        ((is-class class-name) (error "Class already defined")))  
  (let ((all-parents (remove-duplicates
                      (flatten (mapcar #'inherit-parents parents))
                      :test #'equal
                      :from-end
                      T)))
    (let ((parents-fields (remove-duplicates 
                           (inherit-fields all-parents)
                           :test #'fields-equal
                           :from-end
                           t))
          (parents-methods (remove-duplicates
                            (inherit-methods all-parents)
                            :test #'methods-equal
                            :from-end
                            t)))
      (if (null parts)
          (let ((class-structure `(class,
				   class-name,
				   all-parents,
				   parents-fields,
				   ())))
            (add-class-spec class-name class-structure))
          ;; ELSE
          (let ((fields (check-parts parts '1))
		(methods (check-parts parts '2)))          
            (let ((all-fields (remove-duplicates
                               (append 
				(mapcar 
				 (lambda (elem)
                                   (elaborate-field elem parents-fields))
				 fields)
				parents-fields)
                               :test #'fields-equal
                               :from-end
                               T))
                  (class-methods (remove-duplicates
                                  (mapcar 
                                   (lambda (elem)
                                     (elaborate-method elem parents-methods))
				   methods)
                                  :test #'methods-equal
                                  :from-end
                                  T)))
              (let ((class-structure `(class,
				       class-name,
				       all-parents,
				       all-fields,
				       class-methods)))
		(add-class-spec class-name class-structure)))))))
  class-name)


;;; check-parts (parts condition)
;;;
;;; parts --> lista di fields e metodi da analizzare
;;; condition --> valore numerico che specifica cosa voglio che
;;;               la funzioni ritorni, 1 per fields o 2 per metodi
;;;
;;; controlla ogni elemento di parts e lo ritorna in una lista,
;;; questa e' poi filtrata in base a condition e ritornata
(defun check-parts (parts condition)
  (let ((fields ())
        (methods ())
        (all-parts (mapcar #'check-part parts)))
    (apply #'append (remove NIL (mapcar (lambda (elem)
					  (if (equal (first elem) condition)
					      (rest elem)))
					all-parts)))))


;;; check-part (part)
;;;
;;; part --> lista che indica una lista di fields o  method 
;;;
;;; Dopo controlli standard controllo che l'elemento passato sia un field
;;; o un metodo e lo ritorno dopo i controlli
(defun check-part (part)
  (cond ((null part) ())
        ((listp part) (cond ((equal (first part) 'fields)
			     (append '(1) (check-fields part)))
                            ((equal (first part) 'methods)
			     (append '(2) (check-methods part)))
                            (T
			     (error
			      "Some parts have aren't correctly formatted"))))
        (T
	 (error
	  "One or more parts are not lists"))))


;;; fields-equal (field1 field2)
;;;
;;; field1 --> lista che rappresenta un field
;;; field2 --> lista che rappresenta un field
;;;
;;; Controlla se 2 fields sono uguali.
;;; Sono uguali se:
;;; 1) sono uguali in nome, valore e tipo
;;; 2) sono uguali in nome
(defun fields-equal (field1 field2)
  (cond ((equal field1 field2) T)
        ((equal (first field1) (first field2)) T)
        (T NIL)))


;;; check-fields (fields)
;;;
;;; fields --> lista che indica una lista di fields
;;;
;;; Controlla che fields sia nel formato corretto,
;;; dopodiche' controlla ogni field e lo ritorna
;;; eliminando eventuali duplicati.
;;; Ritorna poi la lista di fields controllati
(defun check-fields (fields)
  (cond ((not (listp fields))
	 (error
	  (format
	   t
	   "ERROR -> Fields:~S is not a list~%"
	   fields)))
        ((equal (car fields) 'fields) (remove-duplicates 
                                       (mapcar #'check-field (rest fields)) 
                                       :test #'fields-equal 
                                       :from-end 
                                       t))
        (T (error (format 
                   t 
                   "ERROR --> Fields: ~S is not a valid fields list~%"
                   fields)))))


;;; check-field (item)
;;;
;;; item --> lista che rappresenta un field
;;;
;;; Controlla che item sia nel formato corretto,
;;; dopodiche' ci sono 3 possibilita':
;;; 1) lista con solo nome, ritorna un field con quel nome,
;;;    valore NIL e type T (any)
;;; 2) lista con nome e valore, ritorna un field con quel nome,
;;;    quel valore e type T (any)
;;; 3) lista con nome, valore e type, controlla che valore sia di quel type e
;;;    ritorna un field con quel nome, quel valore e quel type 
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
                                   (cond ((is-class type)
					  (cond ((null value)
                                                 (list 
                                                  name
                                                  value
                                                  type))
                                                ((is-instance value)
                                                 (list
                                                  name
                                                  value
                                                  type))
                                                (T
						 (error 
                                                  (format
                                                   t
                                                   errorInstance
                                                   value 
                                                   name)))))         
                                         ((typep value type) (list 
                                                              name 
                                                              value 
                                                              type))
                                         (T (error
					     (format
                                              t
                                              errorType
                                              value 
                                              name))))))
        (T (error
	    (format 
             t 
             "ERROR -> Field:~S is not a valid field~%" 
             item)))))


;;; methods-equal (met1 met2)
;;;
;;; met1 --> lista che rappresenta un metodo
;;; met2 --> lista che rappresenta un metodo
;;;
;;; Controlla se 2 metodi sono uguali.
;;; Sono uguali se:
;;; 1) sono uguali in nome e numero di args
(defun methods-equal (met1 met2)
  (cond ((equal met1 met2) T)
        ((equal (first met1) (first met2)) (if (equal (length (second met1))
						      (length (second met2)))
                                               T
					       ;;ELSE
                                               NIL))
        (T NIL)))


;;; check-methods (methods)
;;;
;;; methods --> lista che indica una lista di metodi
;;;
;;; Controlla che methods sia nel formato corretto,
;;; dopodiche' controlla ogni metodo e lo ritorna
;;; eliminando eventuali duplicati.
;;; Ritorna poi la lista di metodi controllati
(defun check-methods (methods)
  (cond ((not (listp methods))
	 (error
	  (format
	   t
	   "ERROR -> Methods:~S is not a list~%"
	   parents)))
        ((equal (car methods) 'methods) (remove-duplicates
					 (mapcar
					  #'check-method
					  (rest methods))
					 :test #'me
					 thods-equal :f
					 rom-end
					 t))
        (T (error
	    (format 
             t 
             "ERROR -> Methods:~S is not a valid methods list~%" 
             methods)))))


;;; check-method (item)
;;;
;;; item --> lista che rappresenta un metodo
;;;
;;; Controlla che item sia nel formato corretto,
;;; dopodiche' controlla che args sia una lista e body non sia nullo e
;;; ritorna il metodo
(defun check-method (item)
  (cond ((null item) ())        
        ((equal (length item) 3) (let ((name (first item))
                                       (args (second item))
                                       (body (third item)))
                                   (if (and (symbolp name)
                                            (listp args)
                                            (not (null body)))
                                       (list 
                                        name 
                                        args 
                                        body)
                                       ;;ELSE
                                       (error
					"ERROR -> Method is not a valid method"
					)))) 
        (T (error
	    (format 
             t 
             "ERROR -> Method:~S is not a valid method~%" 
             item)))))


;;; is-class (class-name)
;;;
;;; class-name --> simbolo che rappresenta il nome di una classe definita
;;;
;;; Controlla che la classe sia definita e ritorna T se esiste, NIL altrimenti
;;; Se class-name = T ritorna T
(defun is-class (class-name)
  (cond ((not (symbolp class-name))
	 (error
	  (format 
           t 
           "ERROR -> Class-name:~S is not a symbol~%"
	   class-name)))
        ((equal class-name T) T)
        ((not (null (get-class-spec class-name))) T)
        (T NIL)))


;;; field (instance field-name)
;;;
;;; isntance --> lista che rappresenta una istanza
;;; field-name --> simbolo che rappresenta il nome di un field dell'istanza
;;;
;;; Dopo aver controllato che instance sia una istanza,
;;; se field-name e' un field dell'istanza ne ritorna il valore
(defun field (instance field-name)
  (cond ((not (symbolp field-name))
	 (error
	  (format 
           t 
           "ERROR -> Field-name:~S is not a symbol~%"
	   field-name)))
        ((not (is-instance instance))
	 (error
	  (format 
           t 
           "ERROR -> Instance:~S not an instance~%"
           instance)))
        ((is-instance instance) (if (null (assoc field-name (third instance))) 
                                    (error
				     (format 
                                      t 
                                      errorFieldInstance
                                      field-name
                                      instance))
                                    ;; ELSE
                                    (second
				     (assoc field-name
					    (third instance))))) 
        (T (error
	    (format 
             t 
             "ERROR ->(field ~S ~S) unhandled~%" 
             instance
             field-name)))))


;;; field* (instance field-name)
;;;
;;; isntance --> lista che rappresenta una istanza
;;; field-name --> lista che rappresenta la catena di
;;;                nomi di fields da scorrere
;;;
;;; Dopo aver controllato che instance sia una istanza,
;;; se la lista non e' vuota, recupera il valore del primo field di field-name
;;; se field-name ha lunghezza 1 ritorna il valore del field,
;;; altrimenti chiamati ricorsivamente sul valore preso come instance
(defun field* (instance field-name)
  (cond ((not (listp field-name))
	 (error
	  (format 
           t 
           "ERROR -> Field-name:~S is not a list~%" 
           field-name)))
        ((equal (length field-name) 0)
	 (error
	  (format 
           t 
           "ERROR -> Field-name:~S is empty~%" 
           field-name)))
        ((equal (length field-name) 1) (field instance (first field-name)))
        ((listp field-name) (field*
			     (field
			      instance (first field-name))
			     (rest field-name)))        
        (T
	 (error
	  (format 
           t 
           "ERROR ->(field ~S ~S) unhandled~%" 
           instance
           field-name)))))


;;; get-class-fields (class-name)
;;;
;;; class-name --> simbolo che indica il nome di una classe
;;;
;;; Dato il nome di una classe, recuperane i fields e ritornali
(defun get-class-fields (class-name)
  (cond ((not (is-class class-name))
	 (error
	  (format 
           t 
           "ERROR -> Class-name:~S is not a defined class~%" 
           class-name)))
        ((is-class class-name) (fourth (get-class-spec class-name)))
        (T
	 (error
	  (format 
           t 
           "ERROR -> (get-class-fields ~S) unhandled~%" 
           class-name)))))


;;; get-class-field (class-name field-name)
;;;
;;; class-name --> simbolo che indica il nome di una classe
;;; field-name --> simbolo che indica il nome di un field della classe
;;;
;;; Dato il nome di una classe,
;;; recupera il field con nome field-name e ritornalo
(defun get-class-field (class-name field-name)
  (cond ((not (symbolp field-name))
	 (error
	  (format 
           t 
           "ERROR -> Field-name:~S is not a symbol~%"
	   field-name)))
        ((not (is-class class-name))
	 (error
	  (format 
           t 
           "ERROR -> Class-name:~S is not a defined class~%" 
           class-name)))
        ((is-class class-name) (if (assoc field-name
					  (get-class-fields class-name)) 
                                   (assoc field-name
					  (get-class-fields class-name))
				   ;;ELSE
				   NIL))
        (T
	 (error
	  (format 
           t 
           "ERROR -> (get-class-field ~S ~S) unhandled~%" 
           class-name 
           field-name)))))


;;; get-class-methods (class-name)
;;;
;;; class-name --> simbolo che indica il nome di una classe
;;;
;;; Dato il nome di una classe, recuperane i metodi e ritornali
(defun get-class-methods(class-name)
  (cond ((not (symbolp class-name))
	 (error
	  (format
	   t
	   "ERROR -> Class-name:~S is not an symbol~%"
	   class-name)))
        ((not (is-class class-name))
	 (error
	  (format
	   t
	   "ERROR -> Class-name:~S is not a defined class~%"
	   class-name)))
        ((is-class class-name) (fifth (get-class-spec class-name)))
        (T
	 (error
	  (format 
           t 
           "ERROR -> (get-class-methods ~S) unhandled~%" 
           class-name)))))


;;; get-class-method (class-name method-name)
;;;
;;; class-name --> simbolo che indica il nome di una classe
;;; method-name --> simbolo che indica il nome di un metodo della classe
;;;
;;; Dato il nome di una classe,
;;; recupera il metodo con nome method-name e ritornalo.
;;; Se non e' presente nella classe stessa,
;;; chiama la funzione sui parents della classe
(defun get-class-method (class-name method-name)
  (cond ((not (symbolp method-name)) 
         (error
	  (format 
           t 
           "ERROR -> Method-name:~S is not an symbol~%" 
           method-name)))
        ((not (is-class class-name)) 
         (error
	  (format 
           t 
           "ERROR -> Class-name:~S is not a defined class~%" 
           class-name)))
        ((is-class class-name) 
         (if (assoc method-name (get-class-methods class-name)) 
             (assoc 
              method-name 
              (get-class-methods class-name))
             ;;ELSE
             (if (null (third (get-class-spec class-name)))
		 NIL
		 ;;ELSE
		 (first (mapcar (lambda (elem)
				  (get-class-method elem method-name))
				(third (get-class-spec class-name)))))))
        (T
	 (error
	  (format 
           t 
           "ERROR -> (get-class-method ~S ~S) unhandled~%" 
           class-name 
           method-name)))))


(defun make (class-name &rest slots)
  (cond ((not (is-class class-name))
	 (error
	  (format 
           t 
           "ERROR -> Class:~S is not a defined class~%" 
           class-name))) 
        (T (let ((class-spec (get-class-spec class-name)))
             (cond ((null slots) (list 
                                  'oolinstance 
                                  class-name 
                                  (mapcar #'remove-type (fourth class-spec)))) 
                   ((if (listp slots) 
                        ;; se i fields sono pari (nome-field value) allora ok
			;;altrimenti ne manca 1
                        (if (evenp (length slots))
                            (list 
                             'oolinstance 
                             class-name 
                             (remove-duplicates
			      (append
			       (rec-instance-modify-fields
				class-name slots)
			       (fourth class-spec)) 
                              :test #'fields-equal 
                              :from-end
                              t))
                            ;; ELSE
                            (error
			     (format 
                              t 
                              "ERROR -> Fields:~S not correctly defined~%" 
                              slots)))
			;; ELSE
			(error
			 (format 
                          t 
                          "ERROR -> Fields:~S is not a list~%" 
                          slots)))))))))


;;; remove-type (field)
;;;
;;; field --> lista che rappresenta un field
;;;
;;; Controlla che field sia di forma corretta,
;;; dopodiche' ritornalo in forma (Name Value), rimuovendo il type
(defun remove-type (field)
  (cond ((null field) ())
        ((and (listp field) (equal (length field) 3)) (list
						       (first field)
						       (second field)))
        (T
	 (error
	  (format 
           t 
           "ERROR -> (remove-type ~S) unhandled~%" 
           field)))))


;;; is-instance (instance &optional class-name)
;;;
;;; instance --> lista che rappresenta una lista
;;; class-name --> opzionale, simbolo che rappresenta una classe
;;;
;;; Se class-name = null o T, controlla che instance sia una istanza
;;; di forma corretta, ritorna T se vero, NIL se falso.
;;; Se class-name \= null o T, controlla che sia una classe definita e
;;; che instance sia una instanza di forma corretta e
;;; di classe class-name o superclasse class-name,
;;; ritorna T se vero, NIL se falso
(defun is-instance (instance &optional class-name)  
  (cond ((or (null class-name)
	     (equal class-name T))
	 (check-instance instance))
        ((not (is-class class-name))
	 (error
	  (format 
           t 
           "ERROR -> Class-name:~S is not a defined class~%" 
           class-name)))
        ((is-class class-name) 
         (if (not (check-instance instance))
	     (error
	      (format 
               t
               "ERROR -> Instance:~S is not a valid instance~%"
               instance))
             ;; ELSE
             (if (null (member class-name
			       (third
				(get-class-spec
				 (second instance))))) 
		 (if (equal class-name (second instance))
                     T
                     ;;ELSE
                     NIL)
		 ;;ELSE
		 T)))
        (T
	 (error
	  (format 
           t 
           "ERROR -> (is-instance ~S ~S) unhandled~%" 
           instance
           class-name)))))


;;; check-instance (instance)
;;;
;;; instance --> lista che rappresenta una istanza
;;;
;;; Controlla che instance sia una istanza di forma corretta,
;;; ovvero:
;;; 1) sia una lista, non nulla di 3 elementi
;;; 2) il primo elemento deve essere uguale a oolinstance
;;; 3) il secondo elemento deve essere il nome di una classe definita
;;; 4) il terzo deve essere una lista di fields corretti in base alla classe
;;; Ritorna T se passa tutti i controlli, NIL se ne fallisce anche solo 1
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


;;; is-instance-app (instance-fields class-fields)
;;;
;;; instance-field --> lista che rappresenta dei fields di una istanza
;;; class-fields --> lista che rappresenta dei field di una classe
;;;
;;; Funzione di appoggio per is-instance,
;;; controlla che ogni elemento in class-fields abbia una corrispondenza
;;; in instance-fields.
;;; In sostanza controlla che solo e tutti i fields di una classe
;;; siano presenti in una istanza di quella classe
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
                                               (fields-equal instance-field
							     elem)) 
                                             class-fields))
                 T
		 ;; ELSE
		 NIL))))))


;;; enable-modify (class-name field-name new-value)
;;;
;;; class-name --> simbolo che indica il nome di una classe
;;; field-name --> simbolo che indica il nome di un field
;;; new-value --> nuovo valore del field con nome field-name
;;;
;;; Recupera il type del field con nome field-name dalla classe,
;;; controlla che new-value sia un valore del tipo type.
;;; Ritorna -1 se field-name non e' un field della classe class-name,
;;; NIL se value e' di tipo errato,
;;; T se value e' di tipo corretto
(defun enable-modify(class-name field-name new-value)
  (let ((type (third (get-class-field class-name field-name))))
       (if (null type)
           -1
         (cond ((is-class type) (is-instance new-value type))
               ((typep new-value type) T)
               (T NIL)))))


(defun rec-instance-modify-fields (class-name slots)
  (if (null slots)
      ()
      ;; ELSE
      (let ((field-name (first slots))
            (field-value (second slots))
            (rest  (cddr slots)))
	(case (enable-modify class-name field-name field-value)
          ('NIL
	   (error
	    (format 
	     t 
             "ERROR -> Field-name:~S is not of correct type in Class:~S~%" 
             field-name
             class-name)))
          ('-1
	   (error
	    (format 
             t 
             "ERROR -> Field-name:~S is not a defined field in Class:~S ~%" 
             field-name
             class-name)))
        (T (append 
             (list (list field-name field-value))
             (rec-instance-modify-fields class-name rest)
             ))))))


;;; inherit-fields (classes)
;;;
;;; classes --> lista di nomi di classi
;;;
;;; Ritorna una lista composta da tutti i fields delle classi in classes,
;;; i fields sono ordinati in base alla comparsa delle classi e
;;; ci possono essere ripetizioni di fields
(defun inherit-fields (classes)
  (cond ((not (listp classes))
	 (error
	  "List of parents is not a lost"))
        ((null classes) ())
        ((listp classes) (append 
                          (get-class-fields (first classes))
                          (inherit-fields (rest classes))))))


;;; inherit-methods (classes)
;;;
;;; classes --> lista di nomi di classi
;;;
;;; Ritorna una lista composta da tutti i metodi delle classi in classes,
;;; i metodi sono ordinati in base alla comparsa delle classi e
;;; ci possono essere ripetizioni di metodi
(defun inherit-methods (classes)
  (cond ((not (listp classes))
	 (error
	  "List of parents is not a list"))
        ((null classes) ())
        ((listp classes) (append 
                          (get-class-methods (first classes))
                          (inherit-methods (rest classes))))))


;;; inherit-parents (class-name)
;;;
;;; class-name ---> simbolo che rappresenta il nome di una classe
;;;
;;; Dato il nome di una classe, recupera i suoi parents e
;;; rimuovi eventuali duplicati, poi ritornali
(defun inherit-parents (class-name)
  (remove-duplicates
         (flatten (inherit-parents-app class-name))
         :test #'equal
         :from-end
         t))


;;; inherit-parents-app (class-name)
;;;
;;; class-name ---> simbolo che rappresenta il nome di una classe
;;;
;;; Funzione di appoggio per inheri-parents,
;;; recupero tutti i parents della classe class-name,
;;; questo comprende anche i parents ereditati da altre classi, e li ritorno.
;;; Possono esserci duplicati
(defun inherit-parents-app (class-name)
  (cond ((null class-name) ())
        ((not (is-class class-name))
	 (error
	  (format 
           t 
           "ERROR -> Parent:~S is not a defined class~%"
           class-name)))
        ((is-class class-name) (let ((parents (third (get-class-spec
						      class-name))))
                                 (append (list class-name)
					 (mapcar #'inherit-parents parents))))
        (T
	 (error
	  (format 
           t 
           "ERROR -> (inherit-fields ~S)unhandled~%" 
           class-name)))))


(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
        (lambda
            (this &rest args)
          (if (not (is-instance this))
              (error "ERROR -> instance not valid")
              ;; ELSE
              (let ((method-specs (get-class-method (second this)
						    method-name)))
		(if method-specs
                    (apply (third method-specs)
			   (append (list this) args))
                    ;;ELSE
                    (error "ERROR -> No such method for this instance"))))))
  (eval (rewrite-method-code method-spec)))



(defun rewrite-method-code (method-spec)
               (list 'lambda (append (list 'this)
                                     (second method-spec))
                     (cons 'progn (cddr method-spec))))


;; Funziona
(def-class 'person nil '(methods) '(fields (name "Eve" string) (age 21 integer)))

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










