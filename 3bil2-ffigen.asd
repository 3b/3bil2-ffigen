(defsystem :3bil2-ffigen
  :description "generator for 3bil2 FFI info for android APIs"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (alexandria cxml xpath split-sequence ieee-floats flexi-streams
                          zip 3bil2/env)
  :serial t
  :components ((:file "package")
               (:file "classparser")
               (:file "generator")
               (:file "auto-wrappers")))
