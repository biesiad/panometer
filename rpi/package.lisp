(require :swank)
(swank:create-server :port 4005 :dont-close t)

(load "panometer-core")
(load "panometer-web")

