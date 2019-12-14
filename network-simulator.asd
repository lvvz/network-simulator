(asdf:defsystem :network-simulator
  :class :package-inferred-system 
  :description "Network simulator"
  :version "0.0.1"
  :author "Oleksii Vovchok"
  :depends-on (:network-simulator/log
	       :network-simulator/utils
	       :network-simulator/node
	       :network-simulator/network
	       :network-simulator/generators/all
	       :network-simulator/routing
	       :network-simulator/packet
	       :network-simulator/web-ui/all
	       :network-simulator/dot))
