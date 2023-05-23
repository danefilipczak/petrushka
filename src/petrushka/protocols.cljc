(ns petrushka.protocols)

(defprotocol IExpress
  (write [self]) 
  (domainv [self])
  (codomain [self]) 
  (decisions [self]) 
  (translate [self])
  (validate [self])
  (bindings [self]))