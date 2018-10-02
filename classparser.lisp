(in-package 3bil2-ffigen)

;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html

(defvar *magic* #xcafebabe)

(defun read-u8 (s)
  (read-byte s))

(defun read-u8* (s c)
  (let ((a (make-array c :element-type '(unsigned-byte 8))))
    (assert (= c (read-sequence a s)))
    a))

(defun read-u16 (s)
  (logior
   (ash (read-byte s) 8)
   (read-byte s)))

(defun read-u32 (s)
  (logior
   (ash (read-byte s) 24)
   (ash (read-byte s) 16)
   (ash (read-byte s) 8)
   (read-byte s)))

(defun read-s32 (s)
  (let ((u (read-u32 s)))
    (if (logbitp 31 u)
        (dpb u (byte 32 0) -1)
        u)))

(declaim (inline encode-float32 decode-float32 encode-float64 decode-float64))
(ieee-floats:make-float-converters encode-float32 decode-float32 8 23 t)
(ieee-floats:make-float-converters encode-float64 decode-float64 11 52 t)

(defun read-float (s)
  (let ((u (read-u32 s)))
    (decode-float32 u)))

(defun read-u64 (s)
  (logior
   (ash (read-u32 s) 32)
   (read-u32 s)))

(defun read-s64 (s)
  (let ((u (read-u64 s)))
    (if (logbitp 63 u)
        (dpb u (byte 64 0) -1)
        u)))

(defun read-double (s)
  (let ((u (read-u64 s)))
    (decode-float64 u)))

(defun read-table (s reader)
  (let ((c (read-u16 s)))
    (coerce (loop repeat c collect (funcall reader s))
            'vector)))


(defun read-string (s)
  (let* ((l (read-u16 s))
         (b (read-u8* s l)))
    ;; fixme: .class files use 'modified utf-8'
    (or (babel:octets-to-string b :errorp nil :encoding :utf-8)
        (babel:octets-to-string b :encoding :iso-8859-1))))


(defun read-constant-table (s)
  (let* ((c (read-u16 s))
         (a (make-array c :initial-element nil)))
    (loop
      with i = 1
      while (< i c)
      do (setf
          (aref a i)
          (let ((tag (read-u8 s)))
            (incf i)
            (ecase tag
              (1 (read-string s))
              (3 (read-s32 s))
              (4 (read-float s))
              ;; long and double count as 2 constants
              (5 (incf i) (read-s64 s))
              (6 (incf i) (read-double s))
              (7 (list :class (read-u16 s)))
              (8 (list :string (read-u16 s)))
              (9 (list :field (read-u16 s) (read-u16 s)))
              (10 (list :method (read-u16 s) (read-u16 s)))
              (11 (list :interface (read-u16 s) (read-u16 s)))
              (12 (list :name+type (read-u16 s) (read-u16 s)))
              (15 (list :method-handle (read-u8 s) (read-u16 s)))
              (16 (list :method-type (read-u16 s)))
              (18 (list :invoke-dynamic (read-u32 s)))))))
    a))

(defvar *access-flags* ;; class/field
  '(:public            ;; #x0001
    :private           ;; #x0002
    :protected         ;; #x0004
    :static            ;; #x0008
    :final             ;; #x0010
    :super             ;; #x0020
    :volatile          ;; #x0040
    :transient         ;; #x0080
    :native            ;; #x0100
    :interface         ;; #x0200
    :abstract          ;; #x0400
    :strict            ;; #x0800
    :synthetic         ;; #x1000
    :annotation        ;; #x2000
    :enum))            ;; #x4000

(defvar *method-access-flags*
  '(:public :private :protected :static :final :synchronized :bridge
    :varargs :native nil :abstract :strict :synthetic))

(defun read-access-flags (s &key method)
  (let ((f (read-u16 s))
        (flags (if method
                   *method-access-flags*
                   *access-flags*)))
    (loop for i below 16
          for flag = (pop flags)
          when (logbitp i f)
            collect (or flag i))))

(defun read-attribute (s)
  (list :name (read-u16 s)
        :data (let ((c (read-u32 s)))
                (read-u8* s c))))

(defun read-field (s)
  (list :access (read-access-flags s)
        :name (read-u16 s)
        :descriptor (read-u16 s)
        :attributes (read-table s 'read-attribute)))

(defun read-method (s)
  (list :access (read-access-flags s :method t)
        :name (read-u16 s)
        :descriptor (read-u16 s)
        :attributes (read-table s 'read-attribute)))

(defun parse-java-classfile (s)
  (let ((m (read-u32 s)))
    (assert (= m *magic*))
    (list :magic m
          :minor-version (read-u16 s)
          :major-version (read-u16 s)
          :constants (read-constant-table s)
          :access (read-access-flags s)
          :this (read-u16 s)
          :super (read-u16 s)
          :interfaces (read-table s 'read-u16)
          :fields (read-table s 'read-field)
          :methods (read-table s 'read-method)
          :attributes (read-table s 'read-attribute))))

(defvar *current-attribute-type* "")
(defun expand-class (c)
  (destructuring-bind (&key constants access this super interfaces fields
                         methods attributes
                       &allow-other-keys)
      c
    (labels ((lookup (i)
               (etypecase i
                 (null i)
                 (string i)
                 (integer (lookup (aref constants i)))
                 ((cons (eql :class))
                  (lookup (second i)))
                 ((cons (eql :string))
                  (lookup (second i)))
                 ((cons (eql :name+type))
                  (list (lookup (second i)) (lookup (third i))))))
             (read-rva-ev (s)
               (let ((tag (code-char (read-u8 s))))
                 (ecase tag
                   ;; constant pool
                   ((#\B #\C #\D #\F #\I #\J #\S #\Z)
                    (aref constants (read-u16 s)))
                   (#\s
                    (lookup (read-u16 s)))
                   ;; enum
                   (#\e
                    (list :enum (lookup (read-u16 s))
                          :value (lookup (read-u16 s))))
                   ;; class info
                   (#\c
                    (list :class (lookup (read-u16 s))))
                   ;; annotation
                   (#\@
                    (list :annotation (read-annotation s)))
                   ;; array
                   (#\[
                    (let ((c (read-u16 s)))
                      (list :array (loop repeat c collect (read-rva-ev s))))))))
             (read-annotation (s)
               (list* (lookup (read-u16 s))
                      (loop repeat (read-u16 s)
                            collect (lookup (read-u16 s))
                            collect (read-rva-ev s))))
             (read-rvas (data)
               (flex:with-input-from-sequence (s data)
                 (let ((c (read-u16 s)))
                   (loop
                     repeat c
                     collect (read-annotation s)))))
             (read-rvpas (data)
               (flex:with-input-from-sequence (s data)
                 (loop
                   repeat (read-u8 s)
                   collect (loop
                             repeat (read-u16 s)
                             collect (read-annotation s)))))
             (expand-attribute (a)
               (destructuring-bind (&key name data) a
                 (let ((n (lookup name)))
                   (cond
                     ((string= n "Code")
                      (list :code :skipped))
                     ((string= n "ConstantValue")
                      (let ((i (flex:with-input-from-sequence (s data)
                                 (aref constants (read-u16 s)))))
                        (list :constant-value
                              (ecase *current-attribute-type*
                                ((:I :B :J :S :Z)
                                 i)
                                (:c
                                 (code-char i))
                                ((:f :d)
                                 (assert (typep i '(or float
                                                    (member :negative-infinity
                                                     :positive-infinity
                                                     :not-a-number))))
                                 i)
                                (:|Ljava/lang/String;|
                                 (assert (typep i '(or string
                                                    (cons (eql :string)))))
                                 (lookup i))))))
                     ((string= n "InnerClasses")
                      (list* :inner-classes
                             (flex:with-input-from-sequence (s data)
                               (let ((c (read-u16 s)))
                                 (loop repeat c
                                       for icii = (read-u16 s)
                                       for ocii = (read-u16 s)
                                       for ini = (read-u16 s)
                                       for inaf = (read-access-flags s)
                                       collect (list :inner (lookup icii)
                                                     :outer (lookup ocii)
                                                     :name (lookup ini)
                                                     :access inaf))))))
                     ((string= n "EnclosingMethod")
                      (list* :enclosing-method
                             (flex:with-input-from-sequence (s data)
                               (list :class (lookup (read-u16 s))
                                     :method (lookup (read-u16 s))))))
                     ((string= n "Exceptions")
                      (list* :exceptions
                             (flex:with-input-from-sequence (s data)
                               (loop repeat (read-u16 s)
                                     collect (lookup (read-u16 s))))))
                     ((string= n "Signature")
                      (list :signature
                            (flex:with-input-from-sequence (s data)
                              (lookup (read-u16 s)))))
                     ((string= n "SourceFile")
                      (list :source-file
                            (flex:with-input-from-sequence (s data)
                              (lookup (read-u16 s)))))
                     ((string= n "RuntimeVisibleAnnotations")
                      (list* :runtime-visible-annotations
                             (read-rvas data)))
                     ((string= n "RuntimeInvisibleAnnotations")
                      (list* :runtime-invisible-annotations
                             (read-rvas data)))
                     ((string= n "RuntimeVisibleParameterAnnotations")
                      (list* :runtime-visible-parameter-annotations
                             (read-rvpas data)))
                     ((string= n "RuntimeInvisibleParameterAnnotations")
                      (list* :runtime-invisible-parameter-annotations
                             (read-rvpas data)))
                     ((string= n "AnnotationDefault")
                      (list :annotation-default
                            (flex:with-input-from-sequence (s data)
                              (read-rva-ev s))))
                     (t (list (intern n :keyword) data))))))
             (expand-method (m)
               (destructuring-bind (&key access name descriptor attributes) m
                 (let* ((*current-attribute-type*
                          (or (find-symbol (lookup descriptor) :keyword)
                              (lookup descriptor))))
                   (list
                    :name (lookup name)
                    :type (lookup descriptor)
                    :access access
                    :attributes (map 'list #'expand-attribute attributes)))))
             (expand-field (f)
               (destructuring-bind (&key access name descriptor attributes) f
                 (let* ((*current-attribute-type*
                          (or (find-symbol (lookup descriptor) :keyword)
                              (lookup descriptor))))
                   (list
                    :name (lookup name)
                    :type (lookup descriptor)
                    :access access
                    :attributes (map 'list #'expand-attribute attributes))))))
      (list :class (lookup this)
            :extends (lookup super)
            :implements (map 'list #'lookup interfaces)
            :access access
            :methods (map 'vector #'expand-method methods)
            :fields (map 'vector #'expand-field fields)
            :attributes (map 'list #'expand-attribute attributes)))))


#++
(defvar *foo* (make-hash-table :test 'equal))
#++
(time
 (zip:with-zipfile (z (asdf:system-relative-pathname
                       '3bil2-ffigen "android.28.jar"))
   (zip:do-zipfile-entries (n e z)
     (when (alexandria:ends-with-subseq ".class" n)
       (let ((c (zip:zipfile-entry-contents e)))
         (format t "got ~s: ~s ~s~%" n
                 (length c)
                 (zip:zipfile-entry-comment e))
         (when (plusp (length c))
           (setf (gethash n *foo*)
                 (flex:with-input-from-sequence (s c)
                   (expand-class (parse-java-classfile s))))))))))
#++
(:CLASS "java/lang/Object"
 :EXTENDS NIL
 :IMPLEMENTS NIL
 :ACCESS (:PUBLIC :SUPER)
 :METHODS #((:NAME "<init>" :TYPE "()V" :ACCESS (:PUBLIC))
            (:NAME "getClass" :TYPE "()Ljava/lang/Class;"
             :ACCESS (:PUBLIC :FINAL)
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
