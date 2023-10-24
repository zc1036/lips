
(in-package :asdf-user)

(defsystem "lips"
    :description "The non-dumb text macro system."
    :version "0.1"
    :author "Zach"
    :licence "GPL 3.0"
    :depends-on (:unix-opts)
    :components ((:file "packages")
                 (:file "lips")))
