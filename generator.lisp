#++(require '3bil2-ffigen)
(in-package 3bil2-ffigen)

(defvar *classes*)



(defun load-classfile (file classes)
  (zip:with-zipfile (z file)
    (zip:do-zipfile-entries (n e z)
      (when (alexandria:ends-with-subseq ".class" n)
        (let ((c (zip:zipfile-entry-contents e)))
          #++(format t "got ~s: ~s ~s~%" n
                     (length c)
                     (zip:zipfile-entry-comment e))
          (when (plusp (length c))
            (let ((class (flex:with-input-from-sequence (s c)
                           (expand-class (parse-java-classfile s)))))
              #++(format t "  ~s . ~s~%  ~s~%"
                         (getf class :extends)
                         (getf class :class)
                         (getf class :implements))
              (setf (gethash (getf class :class) classes) class))))))))

#++
(progn
  (setf *classes* (make-hash-table :test 'equal))
  (time (load-classfile (asdf:system-relative-pathname
                         '3bil2-ffigen "android.28.jar")
                        *classes*)))

(defun load-android.jar ()
  (let ((file-name (asdf:system-relative-pathname '3bil2-ffigen "android.jar"))
        (classes (make-hash-table :test 'equal)))
    (unless (probe-file file-name)
      (error  "you need android.jar from android SDK, see README.md"))
    (load-classfile file-name classes)
    classes))
#++
(time (load-android.jar))
;(defvar *name-map*)


(defvar *name-special-cases*
  (alexandria:plist-hash-table
   '("IEEEremainder" "ieee-remainder"
     "setRGBtoYUV" "set-rgb-to-yuv"
     "setYUVtoRGB" "set-yuv-to-rgb"
     "is5GHzBandSupported" "is-5-GHz-Band-Supported"
     "is80211mcResponder" "is-80211mc-responder"
     "isA2dpPlaying" "is-a2dp-playing"
     "isBluetoothA2dpOn" "is-Bluetooth-A2dp-On"
     "isIPv4CompatibleAddress" "is-IPv4-Compatible-Address"
     "setIpv4Encapsulation" "set-Ipv4-Encapsulation"
     "isP2pSupported" "IS-P2P-SUPPORTED"
     "setBluetoothA2dpOn" "set-Bluetooth-A2dp-On")
   :test 'equal))

;; todo: make GL names match cl-opengl "glClearBufferuiv"  etc
;; (and/or write cl-opengl compatible wrapper)

(defun fixcaps (n)
  (or (gethash n *name-special-cases*)
      (coerce
       (loop for last3 = nil then last2
             for last2 = nil then last
             for last = nil then a
             for (a b . more) on (coerce n 'list)
             when (or (and last
                           (not (position last "_xXFIU"))
                           (not (position a "_dDxXfiou"))
                           (not (eql (not (digit-char-p last))
                                     (not (digit-char-p a)))))
                      ;; try to match cl-opengl for some
                      ;; names. possibly should just handle gl*
                      ;; completely separately to remove gl- and so
                      ;; stuff like "glTexGenfOES" can match cl-opengl
                      ;; too
                      (and last2 ;; "matrix1f etc
                           (eql last2 #\i)
                           (eql last #\x)
                           (position a "1234"))
                      (and last3 ;; "4x4fx" etc
                           (position last3 "1234")
                           (eql last2 #\x)
                           (position last "1234")
                           (position a "fd"))
                      (and last
                           (or (lower-case-p last))
                           (upper-case-p a))
                      (and last
                           (not (eql last #\_))
                           (upper-case-p a)
                           b
                           (and (or (lower-case-p b))
                                (or more
                                    (not (eql b #\s))))))
               collect #\-
             if (eql a #\_)
               collect #\-
             else
               collect a
             while b)
       'string)))

(defun translate-java-name (name)
  (let* ((s1 (position #\/ name :from-end t))
         (package (if s1 (subseq name 0 s1) ""))
         (name (if s1 (subseq name (1+ s1)) name))
         (name-split (split-sequence:split-sequence #\$ name
                                                    :remove-empty-subseqs t)))
    (list (string-upcase package)
          (format nil "~@:(~{~a~^$~}~)" (mapcar #'fixcaps name-split)))))

(defun translate-field-name (f)
  (let* ((.name (getf f :name))
         (name (string-upcase (fixcaps .name)))
         (access (getf f :access))
         (static (member :static access))
         (static/final (and static
                            (member :final access)))
         (attributes (getf f :attributes))
         (constant (assoc :constant-value attributes)))
    (assert (if constant static/final t))
    #++(when (and static (not static/final))
         (break "~s: ~s ~s ~s"
                %current-field%
                (assoc :constant-value attributes) attributes f))
    (if static
        (if constant
            (format nil "+~a+" name)
            (format nil "*~a*" name))
        name)))

(defun update-classes (classes)
  (labels ((add-inherited-methods (class-name)
             (symbol-macrolet ((class (gethash class-name classes)))
               (unless (getf class :full-methods)
                 (let ((fm (make-array (length (getf class :methods))
                                       :adjustable t :fill-pointer 0))
                       (ff (make-array (length (getf class :fields))
                                       :adjustable t :fill-pointer 0))
                       (index (make-hash-table :test 'equal)))
                   (setf (getf class :full-methods) fm)
                   (setf (getf class :full-fields) ff)
                   (labels ((add (cn c key tag vec &key update)
                              #++(when (string= class-name  "java/lang/Exception")
                                   (break "ex"))
                              (loop for x across (or (getf c key) #())
                                    for name = (getf x :name)
                                    for type = (getf x :type)
                                    for from = (getf x :from)
                                    for old = (gethash (list tag name type)
                                                       index)
                                    if old
                                      do (pushnew (list cn
                                                        (getf x :access))
                                                  (getf (aref vec old) :from)
                                                  :test 'equalp)
                                         (when update
                                           (unless (equal (getf (aref vec old)
                                                                :access)
                                                          (getf x :access))
                                             #++(format t "@@@@ ~s ~s~%"
                                                        class-name name))
                                           (setf (getf (aref vec old) :access)
                                                 (getf x :access))
                                           (setf (getf (aref vec old)
                                                       :attributes)
                                                 (getf x :attributes)))
                                    else
                                      do (setf (gethash (list tag name type)
                                                        index)
                                               (length vec))
                                         (when from
                                           (setf x (alexandria:remove-from-plist
                                                    x :from)))
                                         (pushnew (list cn (getf x :access))
                                                  from
                                                  :test 'equalp)
                                         (vector-push-extend
                                          (list*
                                           :from from
                                           x)
                                          vec))))
                     (loop for cn in (remove nil
                                             (cons (getf class :extends)
                                                   (getf class :implements)))
                           do (add-inherited-methods (getf (gethash cn classes)
                                                           :class))
                              (add cn (gethash cn classes) :full-methods :m fm)
                              (add cn (gethash cn classes) :full-fields :f ff))
                     (add class-name class :methods :m fm :update t)
                     (add class-name class :fields :f ff :update t)))))))
    (loop for key in (alexandria:hash-table-keys classes)
          do (symbol-macrolet ((class (gethash key classes)))
               (add-inherited-methods key)
               (setf (getf class :lisp-name)
                     (translate-java-name (getf class :class)))))))

(defmacro with-vars (() &body body)
  `(let ((*classes* (make-hash-table :test 'equal))
         #++(*name-map* (make-hash-table :test 'equal)))
     ,@body))


#++
(let ((c (alexandria:copy-hash-table *classes*)))
  (update-classes c)
  c)
(defvar %current-field% nil)

(defun package-name-for-class (c)
  (when (stringp c)
    (setf c (gethash c *classes* c)))
  (if (stringp c)
      (string-downcase c)
      (let ((l (getf c :lisp-name)))
        (assert l)
        (format nil "~(~a/~a~)" (first l) (second l)))))

(defun make-packages (classes)
  ;; fixme: do this separately
  (update-classes classes)
  ;; for now, class java.lang.Foo is defined as java/lang:foo, with
  ;; public methods/fields (including inherited) exported from
  ;; package java/lang/foo.
  (let ((p (make-hash-table :test 'equal))
        (done (make-hash-table))
        (work (alexandria:hash-table-values classes))
        (merge-names (make-hash-table :test 'equal))
        #+=
        (public-members (alexandria:hash-table-values :test 'equal)))

    (labels ((.ensure-package (n)
               (or (gethash n p)
                   (let ((pp (make-hash-table :test 'equal)))
                     (setf (gethash :name pp) n)
                     (setf (gethash n p) pp))))
             (.intern (n pn jn &key export access)
               (let ((p (.ensure-package pn)))
                 #++(when (string-equal n "get-chars")
                      (format t "intern ~s into ~s ~s~%" n pn jn)
                      (when (gethash n p)
                        (format t " (was ~s)~%" (gethash n p))))
                 (if (gethash n p)
                     (progn
                       (setf (gethash pn (getf (gethash n p) :export))
                             export)
                       (setf (gethash pn (getf (gethash n p) :access))
                             access)
                       (gethash n p))
                     (setf (gethash n p)
                           (progn #++ (format t "intern ~s ~s ~s~%" n pn jn)
                                  (list :name (list n pn jn)
                                        :export (alexandria:plist-hash-table
                                                 (list pn export)
                                                 :test 'equalp)
                                        :access (alexandria:plist-hash-table
                                                 (list pn access)
                                                 :test 'equalp)))))))
             (access (sym pn access)
               (find access (gethash pn (getf sym :access))))
             (find-interface-conflicts (c)
               (when (stringp c)
                 (assert (gethash c classes))
                 (setf c (gethash c classes)))
               (when (and (getf c :extends)
                          (getf c :implements))
                 (let ((names (make-hash-table :test 'equal)))
                   (loop for s in (cons (getf c :extends)
                                        (getf c :implements))
                         for sc = (gethash s classes)
                         for scn = (getf sc :lisp-name)
                         for scpn = (package-name-for-class sc)
                         for scp = (gethash scpn p)
                         do (loop
                              for k being the hash-keys of scp
                                using (hash-value sym)
                              when (and (stringp k)
                                        (or (access sym scpn :public)
                                            (access sym scpn :protected)))
                                do (pushnew (getf sym :name)
                                            (gethash (first (getf sym :name))
                                                     names)
                                            :test 'equal)))
                   (loop for nn in (alexandria:hash-table-values names)
                         when (> (length nn) 1)
                           do #++(format t "@@~s: ~s ~s~%"
                                         (getf c :lisp-name)
                                         (caar nn)
                                         nn)
                              (pushnew (mapcar 'second nn)
                                       (gethash (caar nn)
                                                merge-names)
                                       :test 'equal))))
               (setf (gethash c done) t))
             (merge-symbols (name new &rest old)
               #++(format t "merge ~s <- ~s~%"
                          name (loop for i in old collect (getf
                                                           (gethash :class i)
                                                           :class :?)))
               (let* ((s (gethash name new))
                      (a (getf s :access))
                      (x (getf s :export)))
                 (loop for p in old
                       for os = (gethash name p)
                       for oa = (getf os :access)
                       for ox = (getf os :export)
                       unless (eq a oa)
                         do #++(format t " :access~%")
                            (loop for k being the hash-keys of oa
                                    using (hash-value v)
                                  do (assert (or (not (gethash k a))
                                                 (equal v (gethash k a))))
                                  #++(format t "~s -> ~s~%" k v)
                                     (setf (gethash k a) v))
                       unless (eq x ox)
                         do  #++(format t " :export~%")
                             (loop for k being the hash-keys of ox
                                     using (hash-value v)
                                   do (assert (or (not (gethash k x))
                                                  (equal v (gethash k x))))
                                   #++(format t "~s -> ~s~%" k v)
                                      (setf (gethash k x) v))
                       unless (eql p new)
                         do (setf (gethash name p) s)
                       #++(format t "add ~s to use for ~s~%"
                                  (gethash :name p) (gethash :name new))
                       #++(pushnew (gethash :name) (gethash :use p)
                                   :test 'string=))))
             (merge-interface-members ()
               (loop for k being the hash-keys of merge-names
                       using (hash-value v)
                     do #++(format t "~s:~%~{  ~s~%~}" k v)
                     #++(format t "~s:~%" k)
                        (loop
                          for (p1 p2) in v
                          for diff = (mismatch p1 p2)
                          for prefix = (cond
                                         ((plusp diff)
                                          (unless (char= (char p1 (1- diff))
                                                         #\/)
                                            #++(format t "x ~s (~s)~%" diff
                                                       (char p1 (1- diff)))
                                            #++(format t "@ ~s~%"
                                                       (position #\/ p1
                                                                 :from-end t
                                                                 :end diff))
                                            (setf diff
                                                  (1+ (or (position #\/ p1
                                                                    :from-end t
                                                                    :end diff)
                                                          (1- diff)))))
                                          (subseq p1
                                                  0
                                                  (1- diff)))
                                         ((alexandria:starts-with-subseq
                                           "java" p1)
                                          p1)
                                         ((alexandria:starts-with-subseq
                                           "java" p2)
                                          p2)
                                         ((alexandria:starts-with-subseq
                                           "android" p1)
                                          p1)
                                         (t p2))
                          for pp = (gethash prefix p)
                          for sn1 = (getf (gethash k (gethash p1 p)) :name)
                          for s = (or (gethash k pp)
                                      (.intern k prefix (third sn1)
                                               :export nil
                                               :access nil))
                          do #++(format t "  ~s~%  ~s~% = ~s~% package: ~s sym: ~s~%~%"
                                        p1 p2 prefix
                                        (not (not pp))
                                        (when pp (gethash k pp)))
                             (merge-symbols k (gethash prefix p)
                                            (gethash p1 p)
                                            (gethash p2 p))
                          #++(when (gethash k (gethash))
                               (format t "")))))
             (walk (c)
               (when (stringp c)
                 (assert (gethash c classes))
                 (setf c (gethash c classes)))
               (when (and c (not (gethash c done)))
                 (walk (getf c :extends))
                 (map 'nil #'walk (getf c :implements))
                 (let* ((jn (getf c :class))
                        (tx (getf c :lisp-name))
                        (pn (string-downcase (first tx)))
                        (n (second tx))
                        (fn (package-name-for-class c)))
                   (let ((cs (.intern n pn jn :export t
                                              :access (list :class c))))
                     (declare (ignore cs))
                     (.ensure-package fn)
                     #++(setf (gethash n (.ensure-package fn)) cs)
                     (setf (gethash :class (.ensure-package fn)) c))

                   (assert (not (gethash :use (.ensure-package fn))))
                   ;; add class name to containing package and class
                   ;; package (latter mostly to simplify :USE, so :USE
                   ;; FOO/BAR/BAZ will make class BAZ available
                   ;; instead of needing to specify FOO/BAR:BAZ) if
                   ;; not :USEing, probably better to use FOO/BAR:BAZ
                   ;; rather than FOO/BAR/BAZ:BAZ
                   (setf (gethash :use (.ensure-package fn))
                         (copy-list
                          (remove 'nil (cons (getf c :extends)
                                             (getf c :implements)))))
                   ;; import members/fields from superclasses
                   (when (getf c :extends)
                     (loop with fnp = (.ensure-package fn)
                           for s in (cons (getf c :extends)
                                          (getf c :implements))
                           for sc = (gethash s classes)
                           for scn = (getf sc :lisp-name)
                           for scpn = (package-name-for-class sc)
                           for scp = (gethash scpn p)
                           do (loop
                                for k being the hash-keys of scp
                                  using (hash-value sym)
                                when (and (stringp k)
                                          (or (access sym scpn :public)
                                              (access sym scpn :protected)))
                                  do (setf (gethash (first (getf sym :name))
                                                    fnp)
                                           sym))))
                   (loop for m across (getf c :full-methods)
                         for n = (getf m :name)
                         for a = (getf m :access)
                         do (.intern (string-upcase (fixcaps n)) fn (list jn n)
                                     :export (find :public a)
                                     :access (cons :method a)))
                   (loop for f across (getf c :full-fields)
                         for n = (getf f :name)
                         for a = (getf f :access)
                         for tn = (let ((%current-field% (cons (getf c :lisp-name)
                                                               n)))
                                    (translate-field-name f))
                         do (.intern tn fn (list jn n)
                                     :export (find :public a)
                                     :access (cons :method a)))
                   (setf (gethash c done) t)))))
      (.ensure-package "java")
      (map nil #'walk work)
      (clrhash done)
      ;; fix any symbols inherited from both superclass and an
      ;; interface (we combine them all into 1 symbol on the
      ;; assumption they are semantically the 'same thing'
      (map nil #'find-interface-conflicts work)
      (merge-interface-members)
      p)))

#++
(make-packages *classes*)

(defun imports-for-package (class pn pp)
  (let* ((imp (loop with imports = (make-hash-table :test 'equalp)
                    for k being the hash-keys of pp
                      using (hash-value s)
                    when (stringp k)
                      do (let ((sn (first (getf s :name)))
                               (sp (second (getf s :name))))
                           (when (string/= pn sp)
                             #++ (break "~s ~s ~s" sn sp (gethash :usec pp))
                             (unless (and (find sp (gethash :use pp)
                                                :test 'string-equal)
                                          (not
                                           (gethash sn
                                                    (getf s :export))))
                               #++(break "~s ~s ~s" sn sp pp)
                               (pushnew sn (gethash sp imports)
                                        :test 'string-equal))))
                    finally (return imports))))
    #++(when class
         (pushnew (second (getf class :lisp-name))
                  (gethash (first (getf class :lisp-name)) imp)))
    (values imp
            (remove-duplicates
             (append (alexandria:hash-table-keys imp)
                     (gethash :use pp)
                     (when class
                       (list (first (getf class :lisp-name)))))))))

(defun intern-tree (tree)
  (let ((hash (make-hash-table :test #'equal)))
    (labels ((r (node)
               #++(format t "~s = ~s~%" node (gethash node hash))
               (or (gethash node hash)
                   (setf (gethash node hash)
                         (if (atom node)
                             node
                             (cons (r (car node))
                                   (r (cdr node))))))))
      (r tree))))
#++
(let ((*print-circle* t))
  (print
   (intern-tree '((:from
                   (("android/view/ContextThemeWrapper" (:public :static :final))
                    ("android/content/ContextWrapper" (:public :static :final))
                    ("android/content/Context" (:public :static :final)))
                   :name "MODE_MULTI_PROCESS" :type "I" :access
                   (:public :static :final) :attributes
                   ((:constant-value 4) (:|Deprecated| #())
                    (:runtime-visible-annotations ("Ljava/lang/Deprecated;"))))
                  android/app/activity:+mode-no-localized-collators+
                  (:from
                   (("android/view/ContextThemeWrapper" (:public :static :final))
                    ("android/content/ContextWrapper" (:public :static :final))
                    ("android/content/Context" (:public :static :final)))
                   :name "MODE_NO_LOCALIZED_COLLATORS" :type "I" :access
                   (:public :static :final) :attributes ((:constant-value 16)))))))


#++
(defun write-package (pf class pn pp packages)
  (format pf "(defpackage #:~a~%" pn)
  ;; :use superclass/interfaces for 'public' members
  (when (gethash :use pp)
    (format pf "  (:use ~{#:~a~^~%        ~})~%"
            (mapcar
             'package-name-for-class
             (sort (copy-list (gethash :use pp)) 'string<))))
  ;; :import-from for class name and any symbols from other
  ;; packages. (protected inherited names, merged implicit
  ;; protocol names from interfaces)
  (let* ((imp (imports-for-package class pn pp))
         (class (gethash :class pp))
         (name (getf class :lisp-name))
         (super (getf class :extends))
         (shadow nil))
    ;; if class name is defined in superclass package,
    ;; shadowing import from parent dir instead (for
    ;; example java.sql.date extends java.util.date, so we
    ;; want java.sql.date:date to be java.sql:date rather
    ;; than java.util:date (which must be distinct to
    ;; define both classes))
    (when (and class super
               (gethash (second name)
                        (gethash (package-name-for-class super)
                                 packages)))
      (when (string-equal (getf class :class) "java/sql/date")
        (break "break ~s ~s"
               name
               (gethash (package-name-for-class super)
                        packages)))
      (setf shadow t)
      (setf (gethash (first name) imp)
            (remove (second name)
                    (gethash (first name) imp)
                    :test 'string-equal))
      (when (zerop (length (gethash (second name) imp)))
        (remhash (second name) imp)))
    (when shadow
      (format pf "~&  (:shadowing-import-from ~(#:~a #:~a~))"
              (first name) (second name)))
    (loop for from being the hash-keys
            of imp
              using (hash-value syms)
          when syms
            do #++ (break "-~a-" syms)
               (format pf "~&  (:import-from #:~a~
                                ~{~%                #:~a~})"
                       (package-name-for-class from)
                       (sort syms 'string<))))
  (let ((intern (loop for k being the hash-keys of pp
                        using (hash-value s)
                      when (and (stringp k)
                                (string= (second (getf s :name))
                                         pn)
                                (not (getf s :export)))
                        collect (first (getf s :name)))))
    (when intern
      (format pf "~&  (:intern ~{#:~a~^~%           ~})"
              (sort intern 'string<))))
  ;; :export public members
  (format pf "~&  (:export ~{#:~a~^~%           ~})"
          (sort
           (loop for k being the hash-keys of pp
                   using (hash-value s)
                 #+do (break "foo ~s ~s" k s)
                 when (and (stringp k)
                           (getf s :export))
                   collect (first (getf s :name)))
           'string<))
  (format pf ")~%")
  ;; add class and members to 3bil2 environment
  (when class
    (format pf "~%(3bil2::register-ffi-class~%  '~(~:@{~a:~a~}~)"
            (getf class :lisp-name))
    (format pf "~%  :name ~s" (getf class :class))
    (when (getf class :extends)
      (format pf "~%  :extends '~(~:@{~a:~a~}~)"
              (translate-java-name (getf class :extends))))
    (when (getf class :implements)
      (format pf "~%  :implements '~(~:<~@{~a:~a~^ ~}~:@>~)"
              (loop for i in (getf class :implements)
                    append (translate-java-name i))))
    (when (plusp (length (getf class :full-methods)))
      (let ((*print-circle* t))
        (format pf "~%  :methods '~:<~@{~a ~s~^ ~}~:@>"
                (intern-tree
                 (loop for m across (getf class :full-methods)
                       for n = (getf m :name)
                       for ln = (string-upcase (fixcaps n))
                       for sym = (getf (gethash ln pp) :name)
                       for access = (getf m :access)
                       for pn = (package-name-for-class class)
                       for mc = (copy-list m)
                       #++collect (second sym)
                       collect (format nil "~(~a~a~a~)"
                                       pn
                                       (if (member :public access)
                                           ":"
                                           "::")
                                       (first sym))
                       ;; only store most recent definition of method to
                       ;; save space (hopefully correct one in case of
                       ;; interfaces+superclasses)
                       do (setf (getf mc :from)
                                (list (car (getf mc :from))))
                       collect mc)))))
    (when (plusp (length (getf class :full-fields)))
      (let ((*print-circle* t))
        (format pf "~%  :fields '~:<~@{~a ~s~^ ~}~:@>"
                (intern-tree
                 (loop for f across (getf class :full-fields)
                                        ;for n = (getf f :name)
                       for ln = (translate-field-name f)
                       for sym = (getf (gethash ln pp) :name)
                       for access = (getf f :access)
                       for pn = (package-name-for-class class)
                       for fc = (copy-list f)

                       ;;collect (second sym)
                       collect (format nil "~(~a~a~a~)"
                                       pn
                                       (if (member :public access)
                                           ":"
                                           "::")
                                       (first sym))
                       ;; only store most recent definition of field to
                       ;; save space (hopefully correct one in case of
                       ;; interfaces+superclasses)
                       do (setf (getf fc :from)
                                (list (car (getf fc :from))))
                       collect fc)))))
    (format pf "~%  :access '~s" (getf class :access))
    (format pf "~%  :attributes '~s" (getf class :attributes))
    (format pf ")")))

(defun expand-package (class pn pp packages)
  (let* ((package-name pn)
         ;; :use superclass/interfaces for 'public' members
         (use (mapcar
               'package-name-for-class
               (sort (copy-list (gethash :use pp)) 'string<)))
         ;; :import-from for class name and any symbols from other
         ;; packages. (protected inherited names, merged implicit
         ;; protocol names from interfaces)
         (shadow nil)
         (import
           (let* ((imp (imports-for-package class pn pp))
                  (class (gethash :class pp))
                  (name (getf class :lisp-name))
                  (super (getf class :extends)))
             ;; if class name is defined in superclass package,
             ;; shadowing import from parent dir instead (for
             ;; example java.sql.date extends java.util.date, so we
             ;; want java.sql.date:date to be java.sql:date rather
             ;; than java.util:date (which must be distinct to
             ;; define both classes))
             (when (and class super
                        (gethash (second name)
                                 (gethash (package-name-for-class super)
                                          packages)))
               #+=(when (string-equal (getf class :class) "java/sql/date")
                    (break "break ~s ~s"
                           name
                           (gethash (package-name-for-class super)
                                    packages)))
               (setf shadow t)
               (setf (gethash (first name) imp)
                     (remove (second name)
                             (gethash (first name) imp)
                             :test 'string-equal))
               (when (zerop (length (gethash (second name) imp)))
                 (remhash (second name) imp)))
             (when shadow
               (setf shadow (list (first name) (second name))))
             (loop for from being the hash-keys
                     of imp
                       using (hash-value syms)
                   when syms
                     collect (list
                              (package-name-for-class from)
                              (sort syms 'string<)))))
         (intern (sort
                  (loop for k being the hash-keys of pp
                          using (hash-value s)
                        when (and (stringp k)
                                  (string= (second (getf s :name))
                                           pn)
                                  (not (getf s :export)))
                          collect (first (getf s :name)))
                  'string<))
         ;; :export public members
         (public (sort
                  (loop for k being the hash-keys of pp
                          using (hash-value s)
                        #+do (break "foo ~s ~s" k s)
                        when (and (stringp k)
                                  (getf s :export))
                          collect (first (getf s :name)))
                  'string<)))
    `(defpackage ,(string-upcase package-name)
       ,@(when use
           `((:use ,@(mapcar 'string-upcase (alexandria:ensure-list use)))))
       ,@(when shadow
           (break "shadow ~s" shadow)
           `((:shadowing-import-from ,shadow)))
       ,@(loop for (p s) in import
               collect `(:import-from ,(string-upcase p)
                                      ,@ (mapcar 'string-upcase s)))
       ,@ (when intern
            (break "intern ~s" intern)
            `((:intern ,intern)))
          (:export ,@public))))


(defun expand-ffi-def (class pp)
  ;; separate since we need to evaluate package definitions first
  ;; before this can be expanded, since it needs to intern things
  (flet ((sym (n)
           (find-symbol (string-upcase (second n)) (string-upcase (first n)))))

    (list '3bil2::register-ffi-class
          (list 'quote (sym (getf class :lisp-name)))
          :name (list 'quote (getf class :class))
          :extends (when (getf class :extends)
                     (list 'quote
                           (sym (translate-java-name (getf class :extends)))))
          :implements (list 'quote
                            (loop for i in (getf class :implements)
                                  collect (sym (translate-java-name i))))
          :methods
          (list 'quote
                (loop for m across (getf class :full-methods)
                      for n = (getf m :name)
                      for ln = (string-upcase (fixcaps n))
                      for sym = (getf (gethash ln pp) :name)
                      for access = (getf m :access)
                      for pn = (package-name-for-class class)
                      collect (sym (list pn (first sym)))
                      collect m))
          :fields
          (list 'quote
                (loop for f across (getf class :full-fields)
                      for ln = (translate-field-name f)
                      for sym = (getf (gethash ln pp) :name)
                      for access = (getf f :access)
                      for pn = (package-name-for-class class)
                      collect (sym (list pn (first sym)))
                      collect f))
          :access (list 'quote (getf class :access))
          :attributes (list 'quote (getf class :attributes)))))


#++
(defvar *pp* (make-packages *classes*))
#++
(let* ((n "java/sql/date")
       (p (gethash n *pp*))
       (c (gethash :class p)))
  (list (expand-package c n p *pp*)
        (expand-ffi-def c n p *pp*)))

#++
(let* ((n "android/app/activity")
       (p (gethash n *pp*))
       (c (gethash :class p)))
  (list (expand-package c n p *pp*)
        (expand-ffi-def c p)))


(defun write-packages (packages dir)
  (loop with msg = ";;; Generated file, do not edit"
        with *print-case* = :downcase
        for pn being the hash-keys of packages using (hash-value pp)
        for fn = (merge-pathnames (format nil "~(~a~)/package.lisp"
                                          pn)
                                  dir)
        for class = (gethash :class pp)
        do (ensure-directories-exist fn)
           (format t "~&~s~%" fn)
           (when (probe-file fn)
             (with-open-file (pf fn)
               (assert (string= msg (read-line pf)))))
           (with-open-file (pf fn :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
             (format pf "~a~%~%" msg)
             (write-package pf class pn pp packages))))

(defun write-packages-flat (packages dir)
  (let ((done (make-hash-table :test 'equalp))
        (fn (merge-pathnames (format nil "full-package.lisp") dir))
        (msg ";;; Generated file, do not edit")
        (*print-case* :downcase))
    (when (probe-file fn)
      (with-open-file (pf fn)
        (assert (string= msg (read-line pf)))))
    (ensure-directories-exist fn)
    (with-open-file (pf fn :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (format pf "~a~%~%" msg)
      (labels ((r (pn)
                 (unless (gethash pn done)
                   (format t " --> ~s (= ~s)~%" pn
                           (package-name-for-class pn))
                   (let* ((pp (gethash (package-name-for-class pn) packages))
                          (class (gethash :class pp))
                          (def (with-output-to-string (s)
                                 (write-package s class pn pp packages))))
                     (format pf "~%")
                     #++(when (string= pn "android/util/attribute-set")
                          (break "~s ~s"
                                 pn (multiple-value-list (imports-for-package class pn pp))))
                     #++(map nil #'r (getf class :use))
                     #++++(when (string-equal pn "ORG/XMLPULL/V1/XML-PULL-PARSER")
                            (break "foo"))
                     #++(when class
                          (r (package-name-for-class
                              (first (getf class :lisp-name)))))
                     (map nil #'r
                          (nth-value 1 (imports-for-package class pn pp)))
                     (setf (gethash pn done) t)
                     (format pf "~a" def)))))
        (loop for pn being the hash-keys of packages
              do (format t "~&~s~%" pn)
                 (r pn))))))



(defun expand-packages (packages)
  (let ((done (make-hash-table :test 'equalp))
        (out '()))
    (labels ((r (pn)
               (unless (gethash pn done)
                 (let* ((pp (gethash (package-name-for-class pn) packages))
                        (class (gethash :class pp))
                        (def (expand-package class pn pp packages)))
                   (map nil #'r
                        (nth-value 1 (imports-for-package class pn pp)))
                   (setf (gethash pn done) t)
                   (push def out)))))
      (loop for pn being the hash-keys of packages
            do (r pn)))
    (cons 'progn
          (reverse out))))

(defun expand-ffi-defs (packages)
  ;; fixme: save order from expand-pakages?
  (let ((done (make-hash-table :test 'equalp))
        (out '()))
    (labels ((r (pn)
               (unless (gethash pn done)
                 (let* ((pp (gethash (package-name-for-class pn) packages))
                        (class (gethash :class pp))
                        (def (when class (expand-ffi-def class pp))))
                   (map nil #'r
                        (nth-value 1 (imports-for-package class pn pp)))
                   (setf (gethash pn done) t)
                   (when def (push def out))))))
      (loop for pn being the hash-keys of packages
            do (r pn)))
    (cons 'progn
          (reverse out))))

#++
(time
 (expand-packages (make-packages *classes*)))
#++(time
 (subseq
  (expand-ffi-defs *pp*)
  0 10))

#++
(time
 (write-packages-flat (make-packages *classes*)
                      (asdf:system-relative-pathname '3bil2-ffi-flat "")))
#++
(time
 (write-packages (make-packages *classes*)
                 (asdf:system-relative-pathname '3bil2-ffi "")))


(defun write-systems (packages dir)
  (let ((msg ";;; Generated file, do not edit")
        (fn (merge-pathnames "3bil2-ffi.asd" dir))
        (systems nil))
    (ensure-directories-exist fn)
    (when (probe-file fn)
      (with-open-file (pf fn)
        (assert (string= msg (read-line pf)))))
    (with-open-file (pf fn :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (format pf "~a~%~%" msg)

      (loop for pn being the hash-keys of packages using (hash-value pp)
            for class = (gethash :class pp)
            for system = (format nil "3bil2-ffi/~(~a~)" pn)
            do (format t "~&~s~%" pn)
               (format pf "(defsystem ~a~%" system)
               (push system systems)
                                        ;   (break "~s" class)
               (format pf "  :description \"3bil2 ffi definitions for ~a\"~%"
                       (or (getf class :class)
                           (string-downcase pn)))
               (let (#++(imp (loop with imports = (make-hash-table :test 'equal)
                                   for k being the hash-keys of pp
                                     using (hash-value s)
                                   when (stringp k)
                                     do (let ((sn (first (getf s :name)))
                                              (sp (second (getf s :name))))
                                          (when (and (string/= pn sp)
                                                     (not (getf s :export)))
                                            (pushnew sn (gethash sp imports)
                                                     :test 'string=)))
                                   finally (return imports))
                             (gethash))
                     (*print-case* :downcase))
                 (format pf "  :depends-on (3bil2~%               ~{3bil2-ffi/~(~a~)~^~%               ~})~%"
                         (remove-duplicates
                          (mapcar
                           'package-name-for-class

                           (sort (copy-list
                                  (nth-value 1
                                             (imports-for-package class pn pp)))
                                 'string<)


                           #++(append
                               (sort (copy-list (gethash :use pp)) 'string<)
                               ;; depending on imports might be redundant,
                               ;; but easier than checking entire hierarchy
                               ;; to make sure
                               (loop with use = (gethash :use pp)
                                     for from being the hash-keys of imp
                                     unless (member from use :test 'equalp)
                                       collect from)))
                          :test 'string-equal)))
               (format pf "  :components ((:file \"~(~a~)/package\"))"
                       pn)
               (format pf ")~%~%"))
      (format pf "(defsystem 3bil2-ffi
  :description \"3bil2 ffi definitions\"
  :depends-on (~{~a~^~%               ~}))~%"
              (nreverse systems)))))

#++
(time
 (write-systems (make-packages *classes*)
                (asdf:system-relative-pathname '3bil2-ffi "")))
#++
(defparameter *o2*
  (loop for i in (let ((n (make-hash-table :test 'equal)))
                   (loop for c in (alexandria:hash-table-values *classes*)
                         do (loop for m across (getf c :methods)
                                  do (setf (gethash (getf m :name) n) t)))
                   (sort (alexandria:hash-table-keys n) 'string<))
        when (ppcre:scan "([A-Z][A-Z])|([^a-zA-Z])" i)
          do (format t "~&~s -> ~s~%" i (string-upcase(fixcaps i)))
          and collect (string-upcase(fixcaps i))))

#++
(let ((dup (make-hash-table :test 'equal)))
  (loop for i in (let ((n (make-hash-table :test 'equal)))
                   (loop for c in (alexandria:hash-table-values *classes*)
                         do (loop for m across (getf c :methods)
                                  do (setf (gethash (getf m :name) n) t)))
                   (sort (alexandria:hash-table-keys n) 'string<))
        for f = (string-upcase(fixcaps i))
        do (pushnew i (gethash f dup) :test 'string=))
  (loop for (k . v) in (alexandria:hash-table-alist dup)
        when (> (length v) 1)
          do (format t "~&~s <- ~s~&" k v))
  (loop for c in (alexandria:hash-table-values *classes*)
        do (loop for m across (getf c :methods)
                 for f = (string-upcase(fixcaps (getf m :name)))
                 when (> (length (gethash f dup)) 1)
                   do (format t "~s . ~s / ~s~%" (getf c :class) (getf m :name) f))))

#++
(:CLASS "java/lang/Object"
 :EXTENDS NIL
 :IMPLEMENTS NIL
 :ACCESS (:PUBLIC :SUPER)
 :METHODS #((:NAME "<init>" :TYPE "()V" :ACCESS (:PUBLIC))
            (:NAME "getClass" :TYPE "()Ljava/lang/Class;" :ACCESS (:PUBLIC :FINAL)
             :ATTRIBUTES ((:SIGNATURE "()Ljava/lang/Class<*>;")))
            (:NAME "hashCode" :TYPE "()I" :ACCESS (:PUBLIC))
            (:NAME "equals" :TYPE "(Ljava/lang/Object;)Z" :ACCESS (:PUBLIC))
            (:NAME "clone" :TYPE "()Ljava/lang/Object;" :ACCESS (:PROTECTED)
             :ATTRIBUTES ((:EXCEPTIONS "java/lang/CloneNotSupportedException")))
            (:NAME "toString" :TYPE "()Ljava/lang/String;" :ACCESS (:PUBLIC))
            (:NAME "notify" :TYPE "()V" :ACCESS (:PUBLIC :FINAL :NATIVE))
            (:NAME "notifyAll" :TYPE "()V" :ACCESS (:PUBLIC :FINAL :NATIVE))
            (:NAME "wait" :TYPE "(J)V" :ACCESS (:PUBLIC :FINAL)
             :ATTRIBUTES ((:EXCEPTIONS "java/lang/InterruptedException")))
            (:NAME "wait" :TYPE "(JI)V" :ACCESS (:PUBLIC :FINAL :NATIVE)
             :ATTRIBUTES ((:EXCEPTIONS "java/lang/InterruptedException")))
            (:NAME "wait" :TYPE "()V" :ACCESS (:PUBLIC :FINAL :NATIVE)
             :ATTRIBUTES ((:EXCEPTIONS "java/lang/InterruptedException")))
            (:NAME "finalize" :TYPE "()V" :ACCESS (:PROTECTED)
             :ATTRIBUTES ((:EXCEPTIONS "java/lang/Throwable"))))
 :FIELDS #()
 :ATTRIBUTES ((:SOURCE-FILE "Object.java")))
