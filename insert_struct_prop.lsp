;Author: Serag Hassouna
;************
;Purpose of this lisp Application:-
;This lisp contains functions to assign "structural" properties to lines and 3dfaces, these properties will be used
;in SAP 2000 and ETABS Programs.
;This enables you to provide more data rather than the usual way of importing a dxf file, and eleminate the need to start
;the definition process manually and from scratch.
;Future development of this app will contain more commands and utility functions, allowing you to deal with drawing elements directly.
;AutoCAD is known to be more better in dealing with and editing drawing elements than SAP 2000 and ETABS, this lisp app maintains
;this asset to be used, allowing you to avoid editing the structural model with the more constrained SAP 2000 and ETABS.
;***********************
;Notes for the user to consider while using these commands:-
;1- These commands assume that selected objects are from the same type, all are lines or all are 3dfaces,
;so, use different layers to enable yourself to use <layer isolate> and <layer previous>, so as you can select the proper objects cleanly.
;2-the developer has avoided using "Yes or no" keywords' processing to limit keywords' retrieval to be not more than
;the minimum actual needed amount of supplied data while structuring these commands, the moral is to avoid "in depth" commands,
; and to enable the user to insert data quickly and directly in a clear application of the concept of modularity.
;***********************
;Notes for future developers on top of this lisp:-
;1- Extracting a variant value from a transformed safearray to list will return the same value but as a string,
;and the utility function inVariant-ToString must be used to extract its clean string form.
;See how the utility function safe-getreal is constructed.
;2- Extracting a variant value from a XRecord using assocation list with dxf group code will result into the same value
;but always as a string.
;See how the utility function XRecord-get-element is constructed.
;3-For some unknown reason, at every defined command (function), the dxf group codes must be assigned manualy, the use of
;vla-GetXRecordData may result in different "displayed results", although the designed mechanism works well.
;4-The functions were developed to be more and more reliant on low-level utility functions (general utility functions)
;and the ordinary functions are developed to be of one-degree depth or two-degree at maximum.
;The 1st degree is the degree of general utility functions that elevate the coding from the low level to carry out systematic and
;frquently used and desired tasks, & the 2nd degree is the within the logic needed in the command's function itself.
;A 3rd degree is not preferable for the sake of simplifying conducting debugging processes as much as possible.
;5- For every drawing object, its data are stored in XRecords owned by its extension dictionary, these XRecords have standard
;names as following:-
;	5-1- "SecProp": stores the label of its assigned section property
;	5-2- "DistLoads": stores the values of the assigned distributed loads
;6- A Value that is dependant on a value from a pool of previously supplied values must recieve this needed value as a keyword.
;***********************
;List of functions and commands:-
;1- C:set-frames-distload: assigns distributed load on selected frames, with 2 values at start and end points
;2- C:set-frames-udistload: assigns uniform distributed load on selected lines (frames)
;3- C:set-areas-udistload: assigns uniform distributed load on selected 3dface (shells)
;4- C:def-conc-mat: defines new concrete material properties, or modifies existing one
;5- C;def-frame-secprop: defines new frame section property
;6- C:set-frames-secprop: assigns certain section property to selected lines (frames)
;7- C:def-slab-secprop: defines new shell section property
;8- C:set-slabs-secprop: assigns certain section property to selected lines (frames)
;9- C:addloadpattern: adds a new load pattern
;10- C:addpierid: adds a pier id
;11- C:addspandid: adds a spandral id
;12- C:def-wall-secprop: defines new wall section property
;13- C:draw-shw: draws a vertical shearwall/spandrel to a specified depth, with load assignment
;14- C:assign-mat-solidslab: assigns concrete material property for any chosen (reserved predefined) solid slab section property
;15- C:assign-mat-flatslab: assigns concrete material property for any chosen (reversed predefined) flat slab section property
;16- C:set-solidslabs: assigns a solid slab section property alongside with its dead load [for usage if "Dead" Self Weight Modifier=0]
;17- C:set-flatslabs: assigns a flat slab section property alongside with its dead load [for usage if "Dead" Self Weight Modifier=0]
;18- C:set-restraints: assigns hinged or fixed restrain over a group of joints
;19- C:delines: select lines only then delete them
;***********************

;**GENERAL UTILITY FUNCTIONS**

;**General Data Handling**

;add-or-getXRecord: a utility function that creates or gets a Xrecord, parent dictionary is assumed to exist
(defun add-or-getXRecord (pardict kw / errstat xprop)
  (setq errstat (vl-catch-all-apply 'vla-GetObject (list pardict kw)))
  (if (vl-catch-all-error-p errstat)
    (setq xprop (vla-AddXRecord pardict kw)) ;then part
    (setq xprop (vla-GetObject pardict kw)) ;else part
    );End if
  (progn xprop)
  );End defun

;****

;populate-XRecord: a utility function that populates an XRecord with dxf group codes and correspondant values, XRecord is assumed to
;exist
(defun populate-XRecord (xrecobj size dxfgrcd val / dxfgrcd2 val2)
  (setq dxfgrcd2 (vlax-make-safearray vlax-vbInteger (cons 0 (1- size))))
  (setq val2 (vlax-make-safearray vlax-vbVariant (cons 0 (1- size))))
  (vlax-safearray-fill dxfgrcd2 dxfgrcd)
  (vlax-safearray-fill val2 val)
  (vla-SetXRecordData xrecobj dxfgrcd2 val2)

  (princ) ;clean end
  );End defun

;****

;display-XRecord: a utility function that displays (fo a known-size XRecord) dxf group codes and inserted values in 2 lists
;Note: when used in loops in the middle of the program it may create bugs
(defun display-XRecord (xrecobj size / dfxgrcd vals dxfgrcd2 val2)
  (setq dxfgrcd (vlax-make-safearray vlax-vbInteger (cons 0 (1- size))))
  (setq vals (vlax-make-safearray vlax-vbVariant (cons 0 (1- size))))
  (vla-GetXRecordData xrecobj 'dxfgrcd 'vals)

  (setq dxfgrcd2 (vlax-safearray->list dxfgrcd))
  (setq val2 (vlax-safearray->list vals))
  (print dxfgrcd2)
  (print val2)

  (princ)
  );End defun

;****

;dict-in-dict: a utility function that adds or gets a dictionary to the collection "Dictionaries"
(defun dict-in-dict (pardict kw / errstat xprop)
  (setq errstat (vl-catch-all-apply 'vla-GetObject (list pardict kw)))
  (if (vl-catch-all-error-p errstat)
    (setq xprop (vla-Add pardict kw)) ;then part
    (setq xprop (vla-GetObject pardict kw)) ;else part
    );End if
  (progn xprop)
  );End defun

;****

;dict-in-xdict: a utility function that adds or gets a dictionary to an extension dictionary, wait to be tested on other dictionaries
(defun dict-in-xdict (pardict kw / errstat xprop epardict defdata)
  (setq errstat (vl-catch-all-apply 'vla-GetObject (list pardict kw)))
  (if (vl-catch-all-error-p errstat)
    (progn
      (setq epardict (vlax-vla-object->ename pardict)) ;get the ename of the dictionary
      (setq defdata (list '(0 . "DICTIONARY") '(100 . "AcDbDictionary"))) ;initial dictionary definition data
      (setq defdata (entmakex defdata)) ;create the dictionary (owner is not assigned yet)
      (setq exprop (dictadd epardict kw defdata)) ;add the child dictionary and get its entity name (ownership assignment accomplished)
      (vlax-ename->vla-object epardict) ;convert parent to a vla-object
      (setq xprop (vlax-ename->vla-object exprop)) ;convert child to a vla-object
      ) ;progn, [then part]
    (setq xprop (vla-GetObject pardict kw)) ;[else part]
    );End if
  (progn xprop)
  );End defun

;****

;inVariant-ToString: a utility function that extracts what's stored in variant and returns it as string
(defun inVariant-ToString(inval / val pos)
  (setq val (vl-princ-to-string inval)) ;get the printed form of variant as a string
  (setq val (substr val 11)) ;removes the "<#variant " part
  (setq val (vl-string-right-trim ">" val)) ;removes ">" from the end
  (setq pos (vl-string-position (ascii " ") val)) ;get the 1st " " which by standard follows the numerical value of variant size
  (setq val (substr val (+ pos 2))) ;the needed result

  ;(progn val)
  );End defun

;****

;safe-getreal: a utility function that gets real value from user who has a default one as Variant
;user can choose the default one by pressing keyboard's Enter
(defun safe-getreal (defval ordmsg / masg fc)
  ;defval: the default value
  ;ordmsg: is the order message, but without "\n", "<" and ">" default display characters
  (setq masg (strcat "\n" ordmsg " <" (inVariant-ToString defval) ">: ")) ;default message
  
  (setq fc (getreal masg))
  (if (eq fc nil) (setq fc (atof (inVariant-ToString defval))))

  (progn fc)
  );End defun

;****

;ord-getreal: a utility function that gets real value from user who has a default one as integer or real
(defun ord-getreal (defval ordmsg / masg val)
  ;defval: the default value, should be only integer or real
  ;ordmsg: is the order message, but without "\n", "<" and ">" default display characters
  (setq masg (strcat "\n" ordmsg " <" (rtos defval) ">: "))

  (setq val (getreal masg))
  (if (eq val nil) (setq val defval))

  (progn val)
  );End defun

;****

;pool-getkword: get keyword from user from a pool of keywords specified as a list,
;the function works like a standard autocad getting keyword, a default value can be supplied, but if
;it equals nil the function will proceed normally also.
;Note: defval must be a real, an integer or a string!
(defun pool-getkword (defval ordmsg kwlist / dymode elem kwstr strdefval masg kw)
  ;construct the DYNMODE string (allows user to choose keyword using cursor)
  (setq dymode "[")
  (foreach elem kwlist
    (progn
      (setq dymode (strcat dymode elem "/"))
      );progn, froeach expr
    );End foreach
  (setq dymode (vl-string-right-trim "/" dymode))
  (setq dymode (strcat dymode "]"))

  ;construct the initget keywords' string
  (setq kwstr "")
  (foreach elem kwlist
    (progn
      (setq kwstr (strcat kwstr " " elem))
      );progn. foreach expr
    );End foreach

  ;construct the message string, and get the keyword
  (if (eq defval nil)
    (progn
      (setq masg (strcat "\n" ordmsg " " dymode ": "))
      (initget 1 kwstr)
      (setq kw (getkword masg))
      );progn, no default value [then part]
    (progn
      ;get the string form of defval
      (cond
	((eq (type defval) (type 1)) (setq strdefval (itoa defval)));defualt is integer
	((eq (type defval) (type 1.0)) (setq strdefval (rtos defval)));default is real
	((eq (type defval) (type "string")) (setq strdefval defval));default is string
	);End conditional branches
      (setq masg (strcat "\n" ordmsg " <" strdefval "> " dymode ": "))
      (initget kwstr)
      (setq kw (getkword masg))
      (if (eq kw nil) (setq kw defval))
      );progn, there's a default value [else part]
    );End if

  (progn kw)
  );End defun

;****

;all-elem-in-dict: a utility function that returns a list of all keywords of elements in a dictionary
(defun all-elem-in-dict (dictobj / num i elemname elemlist)
  (setq num (vla-get-count dictobj))
  (setq i 0)
  (repeat num
    (progn
      (setq elem (vla-item dictobj i))
      (setq elemname (vla-get-name elem))
      (setq elemlist (append elemlist (list elemname)))
      (setq i (1+ i))
      );progn, repeat expr
    );End repeat

  (progn elemlist)
  );End defun

;****

;XRecord-get-element: a utility function that returns the association list of an element knowing its dxf group code
;Note: if the value's type is vlax-vbVariant, it will be returned from this function as a string
(defun XRecord-get-element (xrecobj grcd / xrec elem)
  (setq xrec (vlax-vla-object->ename xrecobj))
  (setq elem (cdr (assoc grcd (entget xrec))))
  );End defun

;****

;safeVariant->strlist: a utility function that recieves a XRecord (of a
;known size) of variants and returns these values as strings in a list.
(defun safeVariant->strlist (obj size / dxfgrcd val val2)
  (setq dxfgrcd (vlax-make-safearray vlax-vbInteger (cons 0 (1- size))))
  (setq val (vlax-make-safearray vlax-vbVariant (cons 0 (1- size))))
  (vla-GetXRecordData obj 'dxfgrcd 'val) ;remember the apostroph
  (setq val2 (vlax-safearray->list val))
  (repeat (length val2)
    (setq val2 (append (cdr val2) (list (inVariant-ToString (car val2)))))
    );End repeat

  (progn val2)
  );End defun

;****

;csortpts: a utility function that takes a list of points and sort them ascendently according to their distance from a base point
(defun csortpts (bspt ptslist / pt ptdist rank elem1 elem2 ranklist i j no xyzeq sorted)
  (setq no (length ptslist)) ;get number of points
  ;create a list of distances
  (foreach pt ptslist
    (progn
      (setq ptdist (append ptdist (list (distance bspt pt))))
      );End progn [of foreach]
    );End foreach
  
  ;create a list of ranks
  (setq rank 0 i 0 j 0)
  (repeat no
    (progn
      (setq elem1 (nth i ptdist)) ;get the element of distances list
      (repeat no
	(progn
	  (setq elem2 (nth j ptdist)) ;get the to-be-compared element of distance list
	  (if (>= elem1 elem2) (setq rank (1+ rank))) ;increase rank if elem1 > elem2
	  (setq j (1+ j))
	  );End progn [of 2nd repeat]
	);End 2nd repeat
      (setq ranklist (append ranklist (list rank))) ;append rank to ranklist
      (setq rank 0 j 0) ;reset "rank" & "j" to use in the next iteration
      (setq i (1+ i))
      );End progn [of repeat]
    );End repeat
  
  ;create the list of sorted points (it also removes duplicates)
  (setq i 0)
  (while (<= i no)
    (progn
      (setq r (vl-position i ranklist)) ;get value's position based on its rank
      (if (eq r nil)
	(progn
	  (setq i (1+ i))
	  );End progn (then part [for the filter])
	(progn
	  (setq sorted (append sorted (list (nth r ptslist))))
	  (setq i (1+ i))
	  );End progn (else part [for the filter])
	);End if [for a filter that skips if a rank number doesn't exist [debugging solution]]
      );End progn [of repeat]
    );End repeat
  
  (progn sorted)
  );End defun

;**General Initialization**

;init-conc-matprop: a utility function that initializes "InitConcMat" XRecord
;Its first element is a flag to represent whether they were used before or not
(defun init-conc-matprop (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 2 3 4 6 7 8 9))
  ;data are: flag fc E StrainAtFc UltimateStrain PoisonRatio ThermalCoeff UnitWeight
  (setq val '(0 35000.0 200000000.0 0.003 0.008 0.2 0.0000099 24))
  (populate-Xrecord initprop 8 dxfgrcd val)
  );End defun

;****
;init-last-conc-matprop: a utility function that initializes "LastConcMat" XRecord
(defun init-last-conc-matprop (initprop pardict / dxfgrcd val xprop)
  (setq dxfgrcd '(1 2 3 4 6 7 8 9))
  ;data are: label fc E StrainAtFc UltimateStrain PoisonRatio ThermalCoeff UnitWeight
  (setq val '("C35" 35000.0 200000000.0 0.003 0.008 0.2 0.0000099 24))
  (populate-Xrecord initprop 8 dxfgrcd val)

  (setq xprop (add-or-getXRecord pardict "C35"))
  (populate-XRecord xprop 8 dxfgrcd val)
  );End defun

;****

;init-frame-secprop: a utility function that initializes "InitFrSecProp" XRecord
(defun init-frame-secprop (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 6 2 3 7 8 9))
  ;data are: flag SectionType depth breadth UnitWeight=[depth*breadth*ConcUnitWeight]
  (setq val '(0 "Rec" "C35" "Beam" 0.7 0.3 5.04))
  (populate-Xrecord initprop 7 dxfgrcd val)
  );End defun

;****

;init-last-frame-secprop: a utility function that initializes "LastFrSecPropRec" XRecord
(defun init-last-frame-secprop (initprop pardict / dxfgrcd val xprop)
  (setq dxfgrcd '(1 6 2 3 7 8 9))
  ;data are: label SectionType depth breadth UnitWeight=[depth*breadth*ConcUnitWeight]
  (setq val '("B300X700" "Rec" "C35" "Beam" 0.7 0.3 5.04))
  (populate-Xrecord initprop 7 dxfgrcd val)

  (setq xprop (add-or-getXRecord pardict "B300X700"))
  (populate-XRecord xprop 7 dxfgrcd val)
  );End defun

;****

;init-last-frame-secprop1: a utility function that initializes "LastFrSecPropCir" XRecord
(defun init-last-frame-secprop1 (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 6 2 3 7 8 9))
  ;data are: label SectionType diameter radius UnitWeight=[pi*r^2*ConcUnitWeight]
  (setq val (list "Cir300" "Circular" "C35" "Beam" 0.3 0.15 (* pi 0.15 0.15 24)))
  (populate-Xrecord initprop 7 dxfgrcd val)
  );End defun

;****

;init-last-frame-sectype: a utility function that initializes "LastFrSecType" XRecord
(defun init-last-frame-sectype (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 3))
  ;data are: SectionType(shape) SectionType(element type)
  (setq val '("Rec" "Beam"))
  (populate-Xrecord initprop 2 dxfgrcd val)
  );End defun

;****

;init-sectypes: a utility function that initializes "SecTypes" XRecord
(defun init-sectypes (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 2))
  ;data are: SectionType(shape) SectionType(element type)
  (setq val '("Rec" "Circular"))
  (populate-XRecord initprop 2 dxfgrcd val)
  );End defun

;****

;init-lastfrslbl: a utility function that initializes "LastFrSecLbl" XRecord
(defun init-lastfrslbl (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: LastFrameSecLabel
  (setq val '("B300X700"))
  (populate-XRecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initfrslbl: a utility function that initializes "InitFrSeclbl" XRecord
(defun init-initfrslbl (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: flag
  (setq val '(0))
  (populate-XRecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-slab-secprop: a utility function that initializes "InitSlabSecProp" XRecord
(defun init-slab-secprop (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 2 3 4 6))
  ;data are: flag Material ETABS_Thickness SAP2000_Thickness UnitWeight
  (setq val '(0 "C35" 0.16 0.16 3.84))
  (populate-Xrecord initprop 5 dxfgrcd val)
  );End defun

;****

;init-last-slab-secprop: a utility function that initializes "LastSlabSecProp" XRecord
(defun init-last-slab-secprop (initprop pardict / dxfgrcd val xprop label xprop2)
  (setq dxfgrcd '(1 2 3 4 6))
  ;data are: label Material ETABS_Thickness SAP2000_Thickness UnitWeight
  (setq val '("S160" "C35" 0.16 0.16 3.84))
  (populate-Xrecord initprop 5 dxfgrcd val)

  (setq xprop (add-or-getXRecord pardict "S160"))
  (populate-XRecord xprop 5 dxfgrcd val)

  (foreach label (list '("SOLID120" . 0.12) '("SOLID140" . 0.14) '("SOLID150" . 0.15) '("SOLID160" . 0.16))
    (progn
      (setq val (list (car label) "C35" (cdr label) 0.02 (* 24 (cdr label))))
      (setq xprop2 (add-or-getXRecord pardict (car label)))
      (populate-XRecord xprop2 5 dxfgrcd val)
      );End progn [of foreach]
    );End foreach [of assignment of SAP2000 & ETABS initial section properties]

  (foreach label (list '("FLAT180" . 0.18) '("FLAT200" . 0.2) '("FLAT220" . 0.22) '("FLAT240" . 0.24) '("FLAT250" . 0.25) '("FLAT260" . 0.26))
    (progn
      (setq val (list (car label) "C35" (cdr label) (cdr label) (* 24 (cdr label))))
      (setq xprop2 (add-or-getXRecord pardict (car label)))
      (populate-XRecord xprop2 5 dxfgrcd val)
      );End progn [of foreach]
    );End foreach [of assignment of SAP2000 & ETABS initial section properties]

  );End defun

;****

;init-slab-secprop: a utility function that initializes "InitSlabSecProp" XRecord
(defun init-slab-secprop1 (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 2 3 4 6))
  ;data are: label Material ETABS_Thickness SAP2000_Thickness
  (setq val '("S160" "C35" 0.16 0.16 3.84))
  (populate-Xrecord initprop 5 dxfgrcd val)
  );End defun

;****

;init-lastslbslbl: a utility function that initializes "LastSlabSecLbl" XRecord
(defun init-lastslbslbl (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: label
  (setq val '("S160"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initslbslbl1: a utility function that initializes "InitSlabSecLbl" XRecord
(defun init-initslbslbl1 (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: flag
  (setq val '(0))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initslbslbl: a utility function that initializes "InitSlabSecLbl" XRecord
(defun init-initslbslbl (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: label
  (setq val '("S160"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initloadpat: a utility function that initializes "InitLoadPattern" XRecord
(defun init-initloadpat (initprop pardict / dxfgrcd val xprop xprop2 xprop3)
  (setq dxfgrcd '(1 2))
  ;data are: LoadPat LoadType
  (setq val '("SDL" "Dead"))
  (populate-Xrecord initprop 2 dxfgrcd val)

  
  (setq xprop (add-or-getXRecord pardict "SDL"))
  (populate-XRecord xprop 2 dxfgrcd val)

  (setq val '("Dead" "Dead"))
  (setq xprop2 (add-or-getXRecord pardict "Dead"))
  (populate-XRecord xprop2 2 dxfgrcd val)

  (setq val '("Live" "Live"))
  (setq xprop3 (add-or-getXRecord pardict "Live"))
  (populate-XRecord xprop3 2 dxfgrcd val)
  );End defun

;****

;init-initloadpat1: a utility function that initializes "InitLoadPattern1" XRecord
(defun init-initloadpat1 (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 2))
  ;data are: flag LoadType
  (setq val '(0 "Dead"))
  (populate-Xrecord initprop 2 dxfgrcd val)
  );End defun

;****

;init-lastloadpat: a utility function that initializes "LastLoadPattern" XRecord
(defun init-lastloadpat (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 2))
  ;data are: LoadPat LoadType
  (setq val '("SDL" "Dead"))
  (populate-Xrecord initprop 2 dxfgrcd val)
  );End defun

;****

;init-loadpatypes: a utility function that initializes "LoadPatternTypes" XRecord
(defun init-loadpatypes (initprop / dxfgrcd val)
  (setq dxfgrcd '(1 2 3))
  ;data are: LoadTypes ...
  (setq val '("Dead" "Live" "Other"))
  (populate-Xrecord initprop 3 dxfgrcd val)
  );End defun

;****

;init-initpierid1: a utility function that initializes "InitPierID1" XRecord
(defun init-initpierid1 (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: flag
  (setq val '(0))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initpierid: a utility function that initializes "InitPierID" XRecord
(defun init-initpierid (initprop pardict / dxfgrcd val xprop)
  (setq dxfgrcd '(1))
  ;data are: label
  (setq val '("Pier1"))
  (populate-Xrecord initprop 1 dxfgrcd val)

  (setq xprop (add-or-getXRecord pardict "Pier1"))
  (populate-XRecord xprop 1 dxfgrcd val)
  );End defun

;****

;init-lastpierid: a utility function that initializes "LastPierID" XRecord
(defun init-lastpierid (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: label
  (setq val '("Pier1"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initspandid1: a utility function that initializes "InitSpandID1" XRecord
(defun init-initspandid1 (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: flag
  (setq val '(0))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initspandid: a utility function that initializes "InitSpandID" XRecord
(defun init-initspandid (initprop pardict / dxfgrcd val xprop)
  (setq dxfgrcd '(1))
  ;data are: label
  (setq val '("Spand1"))
  (populate-Xrecord initprop 1 dxfgrcd val)

  (setq xprop (add-or-getXRecord pardict "Spand1"))
  (populate-XRecord xprop 1 dxfgrcd val)
  );End defun

;****

;init-lastspandid: a utility function that initializes "LastSpandID" XRecord
(defun init-lastspandid (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: label
  (setq val '("Spand1"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initwallsprop1: a utility function that initializes "InitWallSecProp1" XRecord
(defun init-initwallsprop1 (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: flag
  (setq val '(0))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initwallsprop: a utility function that initializes "InitWallSecProp" XRecord
(defun init-initwallsprop (initprop pardict / dxfgrcd val xprop)
  (setq dxfgrcd '(1))
  ;data are: label
  (setq val '("Wall1"))
  (populate-Xrecord initprop 1 dxfgrcd val)

  (setq dxfgrcd '(1 2 3))
  ;data are: label Thickness Material
  (setq val '("Wall1" 0.3 "C35"))
  (setq xprop (add-or-getXRecord pardict "Wall1"))
  (populate-XRecord xprop 3 dxfgrcd val)
  );End defun

;****

;init-lastwallsprop: a utility function that initializes "LastWallSecProp" XRecord
(defun init-lastwallsprop (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: label Thickness Material
  (setq val '("Wall1"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initwallloadpat1: a utility function that initializes "InitWallLoadPat1" XRecord
(defun init-initwallloadpat1 (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: flag
  (setq val '(0))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initwallloadpat: a utility function that initializes "InitWallLoadPat" XRecord
(defun init-initwallloadpat (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: LoadPattern
  (setq val '("SDL"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-lastwallloadpat: a utility function that initializes "LastWallLoadPat" XRecord
(defun init-lastwallloadpat (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: LoadPattern
  (setq val '("SDL"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initwalldepth1: a utility function that initializes "InitWallDepth1" XRecord
(defun init-initwalldepth1 (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: Flag
  (setq val '(0))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-initwalldepth: a utility function that initializes "InitWallDepth" XRecord
(defun init-initwalldepth (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: WallDepth
  (setq val '(2))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-lastwalldepth: a utility function that initializes "LastWallDepth" XRecord
(defun init-lastwalldepth (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: Flag
  (setq val '(2))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-lastmatsolidslabs: a utility function that initializes "LastMatSolidSlabs" XRecord
(defun init-lastmatsolidslabs (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: Flag
  (setq val '("C35"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-lastmatflatslabs: a utility function that initializes "LastMatFlatSlabs" XRecord
(defun init-lastmatflatslabs (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: Flag
  (setq val '("C35"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-lastsolidslabsprop: a utility function that initializes "LastSolidSlabSecProp" XRecord
(defun init-lastsolidslabsprop (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: Flag
  (setq val '("SOLID160"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-lastflatslabsprop: a utility function that initializes "LastFlatSlabSecProp" XRecord
(defun init-lastflatslabsprop (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: Flag
  (setq val '("FLAT180"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-lastrestrain: a utility function that initializes "LastRestrain" XRecord
(defun init-lastrestrain (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: Flag
  (setq val '("Hinged"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;init-lastchmatsolidslab: a utility function that initializes "LastChMatSolidSlab" XRecord
(defun init-lastchmatsolidslab (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: Flag
  (setq val '("SOLID160"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;****

;init-lastchmatflatslab: a utility function that initializes "LastChMatFlatSlab" XRecord
(defun init-lastchmatflatslab (initprop / dxfgrcd val)
  (setq dxfgrcd '(1))
  ;data are: Flag
  (setq val '("FLAT160"))
  (populate-Xrecord initprop 1 dxfgrcd val)
  );End defun

;****

;************************

;**GENERAL DECLARATIONS**

(vl-load-com) ;load extension functions of visual lisp
(setq acadobj (vlax-get-acad-object)) ;get vla-object of AutoCAD application
(setq doc (vla-get-ActiveDocument acadobj)) ;get vla-object of the active document
(setq modelspace (vla-get-ModelSpace doc)) ;get vla-object of the model Space
(setq dicts (vla-get-Dictionaries doc)) ;get "Dictionaries" collection
(setq misc (dict-in-dict dicts "Misc")) ;creates the "Misc" dictionary to store "LastConcMat", "InitConcMat", "LastFrSecProp"
;and "InitFrSecProp" XRecords, and others

(setq concmat (dict-in-dict dicts "ConcMaterial")) ;creates the "ConcMaterial" dictionary for defined concrete materials
(setq frsprop (dict-in-dict dicts "FrSecProp")) ;creates the "FrSecProp" dictionary for defined frame sections' properties
(setq slbsprop (dict-in-dict dicts "SlabSecProp")) ;creates the "SlabSecProp" dictionary for defined lab sections' properties
(setq loadpatdict (dict-in-dict dicts "LoadPatterns")) ;creates the "Load Patterns" dictionary for defined load patterns
(setq pierids (dict-in-dict dicts "PierIDs")) ;creates the "PierIDs" dictionary that holds all pier IDs
(setq spandids (dict-in-dict dicts "SpandralIDs")) ;creates the "SpandralIDs" dictionary that holds all spandral IDs
(setq wallsprops (dict-in-dict dicts "WallSecProps")) ;creates the "WallSecProps" dictionary for defined wall section properties

(setq lastcprop (add-or-getXRecord misc "LastConcMat")) ;creates the XRecord of the last defined concrete property
(setq lastfrsprop (add-or-getXRecord misc "LastFrSecPropRec"));creates the XRecord of the last defined frame rectangualr section property
(setq lastfrsprop1 (add-or-getXRecord misc "LastFrSecPropCir"));creates the XRecord of the last defined frame circular section property
(setq lastfrstype (add-or-getXRecord misc "LastFrSecType")) ;creates the XRecord that stores the last type of frame section definition
(setq initcprop (add-or-getXRecord misc "InitConcMat")) ;creates the XRecord of the initial values to define concrete property
(setq initfrsprop (add-or-getXRecord misc "InitFrSecProp")) ;creates the XRecord of the initial values to define frame section property
(setq sectypes (add-or-getXRecord misc "SecTypes")) ;creates the XRecords of the available section types to use.
(setq lastfrslbl (add-or-getXRecord misc "LastFrSecLbl")) ;creates the XRecord of the last assigned frame section
(setq initfrslbl (add-or-getXRecord misc "InitFrSeclbl")) ;creates the XRecord that denote whether the initial value is used or not
(setq initslbsprop (add-or-getXRecord misc "InitSlabSecProp")) ;creates the XRecord to denote whether the initial values is used or not
(setq lastslbsprop (add-or-getXRecord misc "LastSlabSecProp")) ;creates the XRecord of the last assigned slab section
(setq initslbsprop1 (add-or-getXRecord misc "InitSlabSecProp1")) ;creats the XRecord of the initial values of slab section definition
(setq lastslbslbl (add-or-getXRecord misc "LastSlabSecLbl")) ;creates the XRecord of the last assigned slab section
(setq initslbslbl1 (add-or-getXRecord misc "InitSlabSecLbl1")) ;creates the XRecord of the initial default slab section assignment
(setq initslbslbl (add-or-getXRecord misc "InitSlabSecLbl")) ;creates the XRecord that denotes whether the initial definition values are used before or not
(setq initloadpat (add-or-getXRecord misc "InitLoadPattern")) ;creates the XRecord of the initial load pattern definition
(setq initloadpat1 (add-or-getXRecord misc "InitLoadPattern1")) ;creates the XRecord that denotes whether the initial definition values are used before or not
(setq lastloadpat (add-or-getXRecord misc "LastLoadPattern")) ;creates the XRecord of the last defined load pattern
(setq loadpatypes (add-or-getXRecord misc "LoadPatternTypes")) ;create the XRecord of the available load pattern types
(setq initpierid1 (add-or-getXRecord misc "InitPierID1")) ;creates the XRecord that denotes if a pier ID assignmet went or not
(setq initpierid (add-or-getXRecord misc "InitPierID")) ;creates the XRecord of the initial pier ID to be assigned
(setq lastpierid (add-or-getXRecord misc "LastPierID")) ;creates the XRecord of the last assigned pier ID
(setq initspandid1 (add-or-getXRecord misc "InitSpandID1")) ;creates the XRecord that denotes whether a spandral ID assignment went or not
(setq initspandid (add-or-getXRecord misc "InitSpandID")) ;creates the XRecord of the initial spanral ID to be assigned
(setq lastspandid (add-or-getXRecord misc "LastSpandID")) ;creates the XRecord of the last assigned spandral ID
(setq initwallsprop1 (add-or-getXRecord misc "InitWallSecProp1")) ;creates the XRecord that denotes last assignment usability of wall section properties
(setq initwallsprop (add-or-getXRecord misc "InitWallSecProp")) ;creates the XRecord of the initial wall section property to be assigned
(setq lastwallsprop (add-or-getXRecord misc "LastWallSecProp")) ;creates the XRecord of the last assigned wall section property
(setq initwallloadpat1 (add-or-getXRecord misc "InitWallLoadPat1")) ;creates the XRecord that denotes if the initial load pattern value was used
(setq initwallloadpat (add-or-getXRecord misc "InitWallLoadPat")) ;creates the XRecord of the inital load pattern of wall to be assigned
(setq lastwallloadpat (add-or-getXRecord misc "LastWallLoadPat")) ;creates the XRecord of the last assigned load pattern on walls
(setq initwalldepth1 (add-or-getXRecord misc "InitWallDepth1")) ;creates the XRecord that denotes if the inital value of wall depth was used or not
(setq initwalldepth (add-or-getXRecord misc "InitWallDepth")) ;creates the XRecord of the initial value of wall depth
(setq lastwalldepth (add-or-getXRecord misc "LastWallDepth")) ;creates the XRecord of the last assigned wall depth
(setq lastmatsolidslabs (add-or-getXRecord misc "LastMatSolidSlabs")) ;creates the XRecord of the last assigned material on a solid slab section property
(setq lastmatflatslabs (add-or-getXRecord misc "LastMatFlatSlabs")) ;creates the XRecord of the last assigned material on a flat slab section property
(setq lastsolidslabsprop (add-or-getXRecord misc "LastSolidSlabSecProp")) ;creates the XRecord of the last assigned solid slab section property
(setq lastflatslabsprop (add-or-getXRecord misc "LastFlatSlabSecProp")) ;creates the XRecord of the last assigned flat slab section property
(setq lastrestrain (add-or-getXRecord misc "LastRestrain")) ;creates the XRecord of the last assigned restrain
(setq lastchmatsolidslab (add-or-getXRecord misc "LastChMatSolidSlab")) ;creates the XRecord of the last solid slab of changed material property
(setq lastchmatflatslab (add-or-getXRecord misc "LastChMatFlatSlab")) ;creates the XRecord of the last flat slab of changed material property

(init-conc-matprop initcprop) ;initialize XRecord "InitConcMat"
(init-last-conc-matprop lastcprop concmat) ;initialize XRecord "LastConcMat", and assign its values in a XRecord in "ConcMaterial" dictionary
(init-frame-secprop initfrsprop) ;initialize XRecord "InitFrSecProp"
(init-last-frame-secprop lastfrsprop frsprop) ;initialize XRecord "LastFrSecPropRec", and assigns its values in a XRecord in "FrSEcProp" dictionary
(init-last-frame-secprop1 lastfrsprop1) ;initialize XRecord "LastFrSecPropCir"
(init-last-frame-sectype lastfrstype) ;initialize XRecord "LastFrSecType"
(init-sectypes sectypes) ;initialize XRecord "SecTypes"
(init-lastfrslbl lastfrslbl) ;initialize XRecord "LastFrSecLbl"
(init-initfrslbl initfrslbl) ;initialize XRecord "InitFrSeclbl"
(init-slab-secprop initslbsprop) ;initialize XRecord "InitSlabSecProp"
(init-last-slab-secprop lastslbsprop slbsprop) ;initialize XRecord "LastSlabSecProp" and assigns last values to a property in dictionary "SlabSecProp"
(init-slab-secprop1 initslbsprop1) ;initialize XRecord "InitSlabSecProp1"
(init-lastslbslbl lastslbslbl) ;initialize XRecord "LastSlabSecLbl"
(init-initslbslbl1 initslbslbl1) ;initialize XRecord "InitSlabSecLbl1"
(init-initslbslbl initslbslbl) ;initialize XRecord "InitSlabSecLbl"
(init-initloadpat initloadpat loadpatdict) ;initialize the XRecords "InitLoadPattern"->"Misc" & "SDL"->"LoadPatterns"
(init-initloadpat1 initloadpat1) ;initialize XRecord "InitLoadPattern1"
(init-lastloadpat lastloadpat) ;initialize XRecord "LastLoadPattern"
(init-loadpatypes loadpatypes) ;initialize XRecord "LoadPatternTypes"
(init-initpierid1 initpierid1) ;initialize XRecord "InitPierID1"
(init-initpierid initpierid pierids) ;initialize XRecord "InitPierID" -> "Misc" & "Pier1" -> "PierIDs"
(init-lastpierid lastpierid) ;initialize XRecord "LastPierID"
(init-initspandid1 initspandid1) ;initialize XRecord "InitSpandID1"
(init-initspandid initspandid spandids) ;initialize XRecord "InitSpandID" -> "Misc" & "Spand1" -> "SpandralIDs"
(init-lastspandid lastspandid) ;initialize XRecord "LastSpandID"
(init-initwallsprop1 initwallsprop1) ;initialize XRecord "InitWallSecProp1"
(init-initwallsprop initwallsprop wallsprops) ;initialize XRecord "InitWallSecProp" -> "Misc" & "M_Wall1" -> "WallSecProps"
(init-lastwallsprop lastwallsprop) ;initialize XRecord "LastWallSecProp"
(init-initwallloadpat initwallloadpat1) ;initialize XRecord "InitWallLoadPat1"
(init-initwallloadpat initwallloadpat) ;initialize XRecord "InitWallLoadPat"
(init-lastwallloadpat lastwallloadpat) ;initialize XRecord "LastWallLoadPat"
(init-initwalldepth1 initwalldepth1) ;initialize XRecord "InitWallDepth1"
(init-initwalldepth initwalldepth) ;initialize XRecord "InitWallDepth"
(init-lastwalldepth lastwalldepth) ;initialize XRecord "LastWallDepth"
(init-lastmatsolidslabs lastmatsolidslabs) ;initialize XRecord "LastMatSolidSlabs"
(init-lastmatflatslabs lastmatflatslabs) ;initialize XRecord "LastMatFlatSlabs"
(init-lastsolidslabsprop lastsolidslabsprop) ;initialize XRecord "LastSolidSlabSecProp"
(init-lastflatslabsprop lastflatslabsprop) ;initialize XRecord "LastFlatSlabSecProp"
(init-lastrestrain lastrestrain) ;initialize XRecord "LastRestrain"
(init-lastchmatsolidslab lastchmatsolidslab) ;initialize XRecord "LastChMatSolidSlab"
(init-lastchmatflatslab lastchmatflatslab) ;initialize XRecord "LastChMatFlatSlab"

;Conclusion: dicts doesn't accept AddXrecord, it only accepts adding dictionaries, it's not a dictionary, it's a collection object.
;************************

;int-set-frames-distload2
;a utility function that recieves values from commands then applies the actual processing work.
(defun in-set-frames-distload2 (sslines sload eload dir loadpat / i objid dxfgrcd distvals exdict kwdist dloads xrecload)
  ;sslines: the selection set of selected lines (frames)
  ;sload: distributed load at start point
  ;elaod: distributed load at end point
  (setq i 0) ;index for every line within the selection set

  ;fill them with values
  (setq dxfgrcd '(1 2 3 4))
  (setq distvals (list sload eload dir loadpat))

  ;now, apply the actual processing for every line within the selection set
  (repeat (sslength sslines)
    (progn
      (setq objid (ssname sslines i)) ;get ename of the current line
      (setq objid (vlax-ename->vla-object objid)) ;convert it to vla-object
      (setq exdict (vla-GetExtensionDictionary objid)) ;get the extension dictionary of the line
      (setq kwdist "DistLoads") ;the keyword for the dictionary of the distribution loads
      ;add or get "DistLoads" dictionary, then add or get the XRecord for the load pattern and its value
      (setq dloads (dict-in-xdict exdict kwdist))
      (setq xrecload (add-or-getXRecord dloads loadpat))

      (populate-XRecord xrecload 4 dxfgrcd distvals)
      
      (setq i (1+ i)) ;go to the next line (frame)
      );End progn
    );End repeat
  
  (princ) ;clean end
  );End Defun

;****

;get-dir: a utility function that takes load direction and returns correspondant numerical code compatible with ETABS
(defun get-dir (kw / dir)
  (cond
    ((= kw "Local-1") (setq dir 1));local-1
    ((= kw "lOcal-2") (setq dir 2));local-2
    ((= kw "loCal-3") (setq dir 3));local-3
    ((= kw "globalX") (setq dir 4));Global-X
    ((= kw "globalY") (setq dir 5));Global-Y
    ((= kw "Gravity") (setq dir 6));Gravity
    ((= kw "globalXProj") (setq dir 7));Global-XProj
    ((= kw "globalYProj") (setq dir 8));Global-YProj
    ((= kw "gravityProj") (setq dir 9));GravityProj
    );End cond
  (progn dir)
  );End defun

;****

;1- set-frames-distload
(defun C:set-frames-distload (/ sslines1 patpool sload1 eload1 dir loadpat kwlist)
  (setq sslines1 (ssget '((0 . "LINE")))) ;Select Objects
  (setq patpool (all-elem-in-dict loadpatdict)) ;get all defined load patterns
  (setq patpool (vl-remove "Dead" patpool)) ;"Dead" from here on is only used internally in the program
  (setq loadpat (pool-getkword (XRecord-get-element lastloadpat 1) "Specify load pattern" patpool))
  (setq sload1 (getreal "\nLoad Value at start point: "))
  (setq eload1 (getreal "\nLoad Value at end point: "))
  (setq kwlist '("Local-1" "lOcal-2" "loCal-3" "globalX" "globalY" "Gravity" "globalXProj" "globalYProj" "gravityProj"))
  (setq dir (get-dir (pool-getkword "Gravity" "Specify load direction" kwlist))) ;assign correspondant numerical code with load direction
  
  (in-set-frames-distload2 sslines1 sload1 eload1 dir loadpat)
  );End defun

;****

;2- set-frames-udistload
(defun C:set-frames-udistload (/ sslines2 patpool load1 dir loadpat kwlist)
  (setq sslines2 (ssget '((0 . "LINE")))) ;Select Objects
  (setq patpool (all-elem-in-dict loadpatdict)) ;get all defined load pattern
  (setq patpool (vl-remove "Dead" patpool)) ;"Dead" from here on is only used internally in the program
  (setq loadpat (pool-getkword (XRecord-get-element lastloadpat 1) "Specify load pattern" patpool))
  (setq load1 (getreal "\nLoad Value: "))
  (setq kwlist '("Local-1" "lOcal-2" "loCal-3" "globalX" "globalY" "Gravity" "globalXProj" "globalYProj" "gravityProj"))
  (setq dir (get-dir (pool-getkword "Gravity" "Specify load direction" kwlist))) ;assign correspondant numerical code with load direction

  (in-set-frames-distload2 sslines2 load1 load1 dir loadpat)
  );End defun

;****

;int-set-areas-distload
;a utility function that recieves values from commands then applies the actual processing work.
(defun in-set-areas-distload (sslines sload dir loadpat / i objid dxfgrcd distvals exdict kwdist dloads xrecload)
  ;sslines: the selection set of selected lines (frames)
  ;sload: distributed load at start point
  ;elaod: distributed load at end point
  (setq i 0) ;index for every line within the selection set

  ;fill them with values
  (setq dxfgrcd '(1 3 4))
  (setq distvals (list sload dir loadpat))

  ;now, apply the actual processing for every line within the selection set
  (repeat (sslength sslines)
    (progn
      (setq objid (ssname sslines i)) ;get ename of the current line
      (setq objid (vlax-ename->vla-object objid)) ;convert it to vla-object
      (setq exdict (vla-GetExtensionDictionary objid)) ;get the extension dictionary of the line
      (setq kwdist "DistLoads") ;the keyword for the dictionary of the distribution loads
      ;add or get "DistLoads", then add or get the XRecord of the assigned load pattern and its value
      (setq dloads (dict-in-xdict exdict kwdist))
      (setq xrecload (add-or-getXRecord dloads loadpat))

      (populate-XRecord xrecload 3 dxfgrcd distvals)
      
      (setq i (1+ i)) ;go to the next line (frame)
      );End progn
    );End repeat
  
  (princ) ;clean end
  );End defun

;****

;3- set-areas-udistload
(defun C:set-areas-udistload (/ sslines2 patpool load1 dir loadpat)
  (setq sslines2 (ssget '((0 . "3DFACE")))) ;Select Objects
  (setq patpool (all-elem-in-dict loadpatdict)) ;get all defined load pattern
  (setq patpool (vl-remove "Dead" patpool)) ;"Dead" from here on is used internally in the program
  (setq loadpat (pool-getkword (XRecord-get-element lastloadpat 1) "Specify load pattern" patpool))
  (setq load1 (getreal "\nLoad Value: "))
  (setq kwlist '("Local-1" "lOcal-2" "loCal-3" "globalX" "globalY" "Gravity" "globalXProj" "globalYProj" "gravityProj"))
  (setq dir (get-dir (pool-getkword "Gravity" "Specify load direction" kwlist))) ;assign correspondant numerical code with load direction
  
  (in-set-areas-distload sslines2 load1 dir loadpat)
  );End defun

;****


;4- def-conc-mat

(defun C:def-conc-mat (/ label initdxfgrcd initval initval2 lastval fstlflg initflg smbvarlst varlst i elem masg fc me sfc su pr tc uw dprop)
  ;Get the data of "InitConcMat" and "LastconcMat"
  (setq initdxfgrcd (vlax-make-safearray vlax-vbInteger '(0 . 7)))
  (setq initval (vlax-make-safearray vlax-vbVariant '(0 . 7)))
  (setq fstflg "0")
  (vla-GetXRecordData initcprop 'initdxfgrcd 'initval)
  (setq lastval (vlax-make-safearray vlax-vbVariant '(0 . 7)))
  (vla-GetXRecordData lastcprop 'initdxfgrcd 'lastval)

  (setq initflg (vlax-safearray-get-element initval 0))


  ;If the flag isn't set to 1, assign initial values to variables from "InitConcMat", otherwise, assign from "LastConcMat"
  (setq smbvarlist (list "fc" "me" "sfc" "su" "pr" "tc" "uw"))
  (setq varlst (vlax-safearray->list initval))
  (setq lastval (vlax-safearray->list lastval))
  (setq i 0)
  (setq initflg (inVariant-ToString initflg))
  (if (eq fstflg initflg)
    (progn
      (foreach elem (cdr varlst)
	(progn
	  ;(print elem) ;debug line
	  ;(print (nth i smbvarlist)) ;debug line
	  (set (read (nth i smbvarlist)) elem)
	  (setq i (1+ i))
	  );progn, foreach expr
	);End foreach
      ;change the flag of "InitConcMat"
  (setq initval (vlax-safearray->list initval))
  (setq i 0)
  (setq initval (cdr initval)) ;all values without the flag
  ;convert values to real
  (repeat 7
    (progn
      (setq initval (append (cdr initval) (list (atof (inVariant-ToString (car initval)))))) ;cdr returns a list
      );progn, repeat expr
    );End repeat
  ;add the new flag
  (setq initval (append '(1) initval))
  (setq initval2 (vlax-make-safearray vlax-vbVariant '(0 . 7)))
  (vlax-safearray-fill initval2 initval)
  (vla-SetXRecordData initcprop initdxfgrcd initval2)
      );progn [then part]
    (progn
      (foreach elem (cdr lastval)
	(progn
	  (set (read (nth i smbvarlist)) elem)
	  (setq i (1+ i))
	  );progn, foreach expr
	);End foreach
      ;(display-XRecord initcprop 8) ;debug line
      );progn [else part]
    );End If
  ;get information from user
  ;Note: the user will not be asked if he/she wishes to modify an existing property
  (setq label (getstring "\nSpecify property label: "))
  ;/fc/ , the others follow the same procedure
  (setq fc (safe-getreal fc "Specify fc'"))
  ;/uw
  (setq uw (safe-getreal uw "Specify unit weight"))
  ;/me/
  (setq me (safe-getreal me "Specify modulus of elasticity"))
  ;/sfc/
  (setq sfc (safe-getreal sfc "Specify Strain at Fc"))
  ;/su/
  (setq su (safe-getreal su "Specify the Ultimate Strain"))
  ;/pr/
  (setq pr (safe-getreal pr "Specify Poison's Ratio"))
  ;/tc/
  (setq tc (safe-getreal tc "Specify Thermal Coefficient"))

  (setq vals (list label fc me sfc su pr tc uw))

  (populate-XRecord lastcprop 8 (vlax-safearray->list initdxfgrcd) vals) ;assign values to "LastConcMat"
  
  ;assign values to a new XRecord in "ConcMaterial" dictionary, or modify existing one
  (setq dprop (add-or-getXRecord concmat label))
  (populate-XRecord dprop 8 (vlax-safearray->list initdxfgrcd) vals)

  ;(print (all-elem-in-dict concmat)) ;debug line
  ;(print "Defualt Initial Material:")
  ;(print (display-XRecord (vla-GetObject concmat "C35") 8));debug line
  
  (princ) ;clean end
  );End defun

;****

;get-concmat-unitweight: a utility function that gets the unit weight of a concrete material property
(defun get-concmat-unitweight (label / xprop uw)
  ;External variables:-
  ;concmat: the dictionary "ConcMaterial"

  (setq xprop (add-or-getXRecord concmat label)) ;get material's XRecord
  (setq uw (atof (XRecord-get-element xprop 9))) ;get the unit weight from its dxf group code 

  (progn uw)
  );End defun

;****

;5- def-frame-secprop

(defun C:def-frame-secprop (/ matlist label mat sectype sectype1 dim1 dim2 uw xprop)
  ;External variables
  ;frsprop: the dictionary "FrSecProp"
  ;initfrsprop: the XRecord "InitFrSecProp"
  ;lastfrsprop: the XRecord "LastFrSecPropRec"
  ;lastfrsprop1: the XRecord "LastFrSecPropCir"
  ;lastfrstype: the XRecord "LastFrSecType"
  ;sectypes: the XRecord "SecTypes"
  ;***

  (setq matlist (all-elem-in-dict concmat)) ;get defined material properties
  (setq lastmat (XRecord-get-element lastcprop 1)) ;get last defined material property

  ;get the label
  (setq label (getstring "\nSpecify property label: "))
  
  ;see if to use intial values in "InitFrSecProp" or to use last defined section type
  (setq fstflg (XRecord-get-element initfrsprop 1)) ;get the flag
  (if (not (eq fstflg "0"))
    (progn
      (setq sectype (XRecord-get-element lastfrstype 1)) ;get last defined section property shape
      (setq sectype (pool-getkword sectype "Specify section shape" '("Rec" "Circular"))) ;get section shape from the user
      (setq sectype1 (XRecord-get-element lastfrstype 3)) ;get last defined section's element type
      (setq sectype1 (pool-getkword sectype1 "Specify section type" '("Beam" "Column"))) ;get last defined section's element type
      
      ;get user input values for the specified section types {consider to fork this conditional in a utility function if there's
      ;large number of section types}
      (cond
	((eq sectype "Rec")
	 (progn
	   (setq dim1 (atof (XRecord-get-element lastfrsprop 7))) ;get 1st default dimension (7 is a dxf group code)
	   (setq dim2 (atof (XRecord-get-element lastfrsprop 8))) ;get 2nd default dimension (8 is a dxf group code)
	   (setq mat (XRecord-get-element lastfrsprop 2)) ;get default material (2 is a dxf group code)
	   (setq mat (pool-getkword mat "Choose material property" matlist)) ;get material from user
	   (setq dim2 (ord-getreal dim2 "Specify breadth")) ;get 2st dimension from user
	   (setq dim1 (ord-getreal dim1 "Specify depth")) ;get 1st dimension from user
	   (setq uw (get-concmat-unitweight mat)) ;get concrete's unit weight
	   (setq uw (* dim1 dim2 uw)) ;get frame's unit weight
	   );progn {Rectangular}
	 );Rectangular Section
	((eq sectype "Circular")
	 (progn
	   (setq dim1 (atof (XRecord-get-element lastfrsprop1 7))) ;get default diameter (7 is a dxf group code)
	   (setq mat (XRecord-get-element lastfrsprop1 2)) ;get default material (2 is a dxf group code)
	   (setq mat (pool-getkword mat "Choose material property" matlist)) ;get material from user
	   (setq dim1 (ord-getreal dim1 "Specify diameter")) ;get diameter from user
	   (setq dim2 (/ dim1 2)) ;get radius
	   (setq uw (get-concmat-unitweight mat)) ;get concrete's unit weight
	   (setq uw (* pi dim2 dim2 uw)) ;get frame's unit weight
	   );progn {Circular}
	 );Circular Section
	);End Conditional branches of different section types
      );progn, not the first time to define a property [then part]
    (progn
      (setq sectype (XRecord-get-element initfrsprop 6)) ;get the initially defined section property shape
      (setq sectype (pool-getkword sectype "Specify section shape" '("Rec" "Circular"))) ;get section shape from the user
      (setq sectype1 (XRecord-get-element initfrsprop 3)) ;get the initially defined section property element type
      (setq sectype1 (pool-getkword sectype1 "Sepcify section type" '("Beam" "Column"))) ;get the initially defined section's element type

      ;get user input values for the specified section types {consider to fork this conditional in a utility function if there's
      ;large number of section types}
      (cond
	((eq sectype "Rec")
	 (progn
	   (setq dim1 (atof (XRecord-get-element initfrsprop 7))) ;get 1st default dimension (7 is a dxf group code)
	   (setq dim2 (atof (XRecord-get-element initfrsprop 8))) ;get 2nd default dimension (8 is a dxf group code)
	   (setq mat (XRecord-get-element initfrsprop 2)) ;get default material (2 is a dxf group code)
	   (setq mat (pool-getkword mat "Choose material property" matlist)) ;get material from user
	   (setq dim2 (ord-getreal dim2 "Specify breadth")) ;get 2nd dimension from user
	   (setq dim1 (ord-getreal dim1 "Specify depth")) ;get 1st dimension from user
	   (setq uw (get-concmat-unitweight mat)) ;get concrete's unit weight
	   (setq uw (* dim1 dim2 uw)) ;get frame's unit weight
	   );progn {Rectangular}
	 );Rectangular Section
	((eq sectype "Circular")
	 (progn
	   (setq dim1 (atof (XRecord-get-element initfrsprop 7))) ;get default diameter (7 is a dxf group code)
	   (setq mat (XRecord-get-element initfrsprop 2)) ;get default material (2 is a dxf group code)
	   (setq mat (pool-getkword mat "Choose material property" matlist)) ;get material from user
	   (setq dim1 (ord-getreal dim1 "Specify diameter")) ;get diameter from user
	   (setq dim2 (/ dim 2)) ;get radius
	   (setq uw (get-concmat-unitweight mat)) ;get concrete's unit weight
	   (setq uw (* pi dim2 dim2 uw)) ;get frame's unit weight
	   );progn {Circular}
	 );Circular Section
	);End conditional branches of different section types
      );progn, it's the first time to define a property [else part]
    );End If

  ;create a new XRecord, and if it's really exists, get it .. then populate it with the values from user
  (setq xprop (add-or-getXRecord frsprop label))
  (setq dxfgrcd '(1 6 2 3 7 8 9))
  (setq vals (list label sectype mat sectype1 dim1 dim2 uw))
  (populate-XRecord xprop 7 dxfgrcd vals)

  ;populate the proper XRecord of last results
  (cond
    ((eq sectype "Rec")
     (progn
       (populate-XRecord lastfrsprop 7 dxfgrcd vals)
       );progn {Rectangular}
     );Rectangular Section
    ((eq sectype "Circular")
     (progn
       (populate-XRecord lastfrsprop1 7 dxfgrcd vals)
       );progn {Circular}
     );Circular Section
    );End conditional branches of updating last results
  
  
  ;change flag of "InitFrSecProp" to 1
  (setq vals '(1 "Rec" "C35" "Beam" 0.7 0.3 5.04)) ;taken from the first definition of "InitFrSecProp",
  (populate-XRecord initfrsprop 7 dxfgrcd vals)

  (princ) ;clean end
  );End defun

;****

;fillSS-XRecord-inExtDict: a utility function to fill some data to copies of a specified XRecord
;which are located in the extension dictionaries of the objects of a specified selection set
;Note: dxfgrcd & vals are values in list
(defun fillSS-XRecord-inExtDict (sslines label size dxfgrcd vals / ssobj obj i xprop)
  (setq i 0)
  (setq len (sslength sslines))
  (repeat len ;for every element in the selection set
    (setq elem (ssname sslines i))
    (setq elem (vlax-ename->vla-object elem))
    (setq obj (vla-GetExtensionDictionary elem)) ;get its extension dictionary
    (setq xprop (add-or-getXRecord obj label)) ;add or get the specified XRecord
;    (print i) ;debug line
;    (print dxfgrcd) ;debug line
;    (print vals) ;debug line
    (populate-XRecord xprop size dxfgrcd vals)
    ;(display-XRecord xprop size);debug line ;Note: this function alters accidently dxfgrcd and vals resulting in an error
    (setq i (1+ i))
    );End repeat

  (princ) ;clean end
  );defun End

;****

;6- set-frames-secprop

(defun C:set-frames-secprop (/ dxfgrcd initval initflg mysec lastsec kw xprop)
  ;External variables:-
  ;initfrslbl: the XRecord "InitFrSecLbl"
  ;lastfrslbl: the XRecord "LastFrSecLbl"
  ;frsprop: the Dictionary "FrSecProp"

  (setq ssline (ssget '((0 . "LINE"))));Select Objects

  ;get the flag of "InitFrSecLbl"
  ;-remember to convert safe arrays to lists when supplying values to utility functions-
  (setq dxfgrcd (vlax-make-safearray vlax-vbInteger '(0 . 0))) ;define dxf group codes' variable [safe array]
  (setq initval (vlax-make-safearray vlax-vbVariant '(0 . 0))) ;retrieve values' variable [safe array]
  (vla-GetXRecordData initfrslbl dxfgrcd initval) ;retrieve dxf group codes and thier associated values
  (vlax-safearray-fill dxfgrcd '(1)) ;to solve an unknown bug that changes dxfgrcd value to '(0)
  (setq initflg (XRecord-get-element initfrslbl 1)) ;get the flag of "InitFrSecLbl"
  (setq mysec (all-elem-in-dict frsprop));get all section properties

  ;See If the initial value is used or not, if not used then use it, otherwise, use the last used value
  (if (eq initflg "0")
    (progn
      ;get and assign the frame section property
      (setq lastsec (XRecord-get-element lastfrslbl 1))
      (setq kw (pool-getkword lastsec "Specify frame section" mysec))
      ;use a utility function to assign value
      (fillSS-XRecord-inExtDict ssline "SecProp" 1 (vlax-safearray->list dxfgrcd) (list kw))
      ;assign values to "LastFrSecLbl"
      (populate-XRecord lastfrslbl 1 (vlax-safearray->list dxfgrcd) (list kw))
      ;change the flag of "InitFrSecLbl" to 1
      ;(setq initval2 '(1)) ;note that if there's more than one elements the code will expand to meet the requirement
      (populate-XRecord initfrslbl 1 (vlax-safearray->list dxfgrcd) '(1))
      ;assign unit weight to "Dead" load pattern
      (setq xprop (add-or-getXRecord frsprop kw))
      (setq uw (atof (XRecord-get-element xprop 9)))
      (in-set-frames-distload2 ssline uw uw 6 "Dead") ;6 is the Gravity direction value
      );progn, the initial value isn't used [then part]
    (progn
      ;get and assign the frame section property
      (setq lastsec (XRecord-get-element lastfrslbl 1))
      (setq kw (pool-getkword lastsec "Specify frame section" mysec))
      ;use a utility function to assign value
      (fillSS-XRecord-inExtDict ssline "SecProp" 1 (vlax-safearray->list dxfgrcd) (list kw))
      ;assign values to "LastFrSecLbl"
      (populate-XRecord lastfrslbl 1 (vlax-safearray->list dxfgrcd) (list kw))
      ;assign unit weight to "Dead" load pattern
      (setq xprop (add-or-getXRecord frsprop kw))
      (setq uw (atof (XRecord-get-element xprop 9)))
      (in-set-frames-distload2 ssline uw uw 6 "Dead") ;6 is the Gravity direction value
      );progn, the iniial value is used, use the last value [else part]
    );End If

  (princ) ;clean end
  );End defun

;****

;7- def-slab-secprop

(defun C:def-slab-secprop (/ forbidlist name dxfgrcd matlist label lastmat fstflg tk mat initval xprop xprop2 uw)
  ;External Variables:-
  ;concmat: the dictionary "ConcMaterial"
  ;slbsprop: the dictionary "SlabSecProp"
  ;initslbsprop: the XRecord "InitSlabSecProp"
  ;initslbsprop1: the XRecord "InitSlabSecProp1" [the initial definition]
  ;lastslbsprop: the XRecord "LastSlabSecProp"
  ;***

  ;list of reserved section properties
  (setq forbidlist '("SOLID120" "SOLID140" "SOLID150" "SOLID160" "FLAT180" "FLAT200" "FLAT220" "FLAT240" "FLAT250" "FLAT260"))

  (setq dxfgrcd '(1 2 3 4 6)) ;dxf group codes
  
  (setq matlist (all-elem-in-dict concmat)) ;get defined material properties
  (setq lastmat (XRecord-get-element lastcprop 1)) ;get last defined material property

  ;get the label
  (setq label (getstring "\nSpecify property label: "))

  ;forbid the user from redefining a reserved section property
  (foreach name forbidlist
    (progn
      (if (eq name label)
	(progn
	  (print "Reserved Property; Use another name")
	  (alert "Reserved Property; Use another name")
	  (vl-catch-all-apply 'quit)
	  );End progn [of forbiding the user from redifinition of any reserved section property]
	);End if [of forbiding the user from redifinition of any reserved section property]
      );End progn [of forbiding the user from redifinition of any reserved section property]
    );End foreach [of forbiding the user from redefinition of any reserved section property]

  ;get material property
  (setq mat (pool-getkword lastmat "Specify slab's material property" matlist))
  (setq xprop2 (add-or-getXRecord concmat mat)) ;get material property's XRecord
  (setq uw (atof (XRecord-get-element xprop2 9))) ;get material's unit weight
  
  ;see if to use intial values in "InitFrSecProp" or to use last defined section type
  (setq fstflg (XRecord-get-element initslbsprop 1)) ;get the flag

  (if (eq fstflg "0")
    (progn
      ;change the flag of "InitSlabSecProp" to 1
      (setq initval '(1 "C35" 0.16 0.16 3.84))
      (populate-XRecord initslbsprop 5 dxfgrcd initval)
      ;get values from user, then assign them to "LastSlabSecProp" & "SlabSecProp"
      (setq tk (atof (XRecord-get-element initslbsprop1 3))) ;get the initial section thickness
      (setq tk (ord-getreal tk "Specify slab thickness")) ;get slab thickness
      (setq uw (* tk uw)) ;get slab's unit weight
      (setq initval (list label mat tk tk uw))
      (populate-XRecord lastslbsprop 5 dxfgrcd initval) ;assignment to "LastSlabSecProp"
      (setq xprop (add-or-getXRecord slbsprop label))
      (populate-XRecord xprop 5 dxfgrcd initval) ;assignment to "SlabSecProp"
      );progn, it's the 1st time to define a slab section [then part]
    (progn
      ;get values from user, then assign them to "LastSlabSecProp" & "SlabSecProp"
      (setq tk (atof (XRecord-get-element lastslbsprop 3))) ;get the last defined section thickness
      (setq tk (ord-getreal tk "Specify slab thickness")) ;get slab thickness
      (setq uw (* tk uw)) ;get slab's unit weight
      (setq initval (list label mat tk tk uw))
      (populate-XRecord lastslbsprop 5 dxfgrcd initval) ;assignment to "LastSlabSecProp"
      (setq xprop (add-or-getXRecord slbsprop label))
      (populate-XRecord xprop 5 dxfgrcd initval) ;assignment to "SlabSecProp"
      );progn, it's not the 1st time to define a slab section [else part]
    );End If

  ;(print (all-elem-in-dict slbsprop)) ;debug line

  (princ) ;clean end
  );End defun

;****

;8- set-slabs-secprop

(defun C:set-slabs-secprop (/ sslines proplist initflg lastprop label val el xprop uw)
  ;External Variables:-
  ;initslbslbl1: the XRecord "InitSlabSecLbl1" that containes the flag denoting the state of the previous use of the initial value
  ;initslbslbl: the XRecord "InitSlabSecLbl" that contains the 1st default value
  ;lastslbslbl: the XRecord "LastSlabSecLbl" that contains the last assigned value

  (setq sslines (ssget '((0 . "3DFACE")))) ;Select Objects
  
  (setq proplist (all-elem-in-dict slbsprop)) ;get all defined section properties

  ;remove all section properties that are reserved for SAP2000 solid slabs 2D modeling definition
  (foreach el '("SOLID120" "SOLID140" "SOLID150" "SOLID160" "FLAT180" "FLAT200" "FLAT220" "FLAT240" "FLAT250" "FLAT260")
    (progn
      (setq proplist (vl-remove el proplist))
      );End progn [of foreach]
    );End foreach [of removing reserved properties of SAP2000 solid slabs 2D modeling definition]
  
  (setq initflg (XRecord-get-element initslbslbl1 1)) ;get the flag

  (setq dxfgrcd '(1)) ;the dxf group code

  (if (eq initflg "0")
    (progn
      ;change flag in "InitSlabSecLbl1" to 1
      (setq val (list label))
      (populate-XRecord initslbslbl1 1 dxfgrcd val)
      ;get values from user
      (setq lastprop (XRecord-get-element initslbslbl 1)) ;get the initial default value
      (setq label (pool-getkword lastprop "Specify slab property" proplist)) ;get slab property from the user
      (setq val (list label))
      ;assign values to "LastSlabSecLbl"
      (populate-XRecord lastslbslbl 1 dxfgrcd val)
      ;assign values to drawing objects 
      (fillSS-XRecord-inExtDict sslines "SecProp" 1 dxfgrcd val)

      ;make PierID and SpandralID set to "None"
      (setq dxfgrcd '(1) val '("None"))
      (fillSS-XRecord-inExtDict sslines "PierID" 1 dxfgrcd val)
      (fillSS-XRecord-inExtDict sslines "SpandralID" 1 dxfgrcd val)

      ;assign unit weight to "Dead" load pattern
      (setq xprop (add-or-getXRecord slbsprop label))
      (setq uw (atof (XRecord-get-element xprop 6)))
      (in-set-areas-distload sslines uw 6 "Dead")
      );progn, if there's no assignment before [then part]
    (progn
      ;get values from user
      (setq lastprop (XRecord-get-element lastslbslbl 1)) ;get the last assigned value
      (setq label (pool-getkword lastprop "Specify slab property" proplist)) ;get slab property from the user
      (setq val (list label))
      ;assign values to "LastSlabSecLbl"
      (populate-XRecord lastslbslbl 1 dxfgrcd val)
      ;assign values to drawing objects 
      (fillSS-XRecord-inExtDict sslines "SecProp" 1 dxfgrcd val)

      ;make PierID and SpandralID set to "None"
      (setq dxfgrcd '(1) val '("None"))
      (fillSS-XRecord-inExtDict sslines "PierID" 1 dxfgrcd val)
      (fillSS-XRecord-inExtDict sslines "SpandralID" 1 dxfgrcd val)

      ;assign unit weight to "Dead" load pattern
      (setq xprop (add-or-getXRecord slbsprop label))
      (setq uw (atof (XRecord-get-element xprop 6)))
      (in-set-areas-distload sslines uw 6 "Dead")
      );progn, if there's any assignment before [else part]
    );End if

  
  (princ) ;clean end
  );End defun

;****

;9- addloadpattern

(defun C:addloadpattern (/ dxfgrcd initfls ltypes val label ltype xprop)
  ;External Variables:-
  ;initloadpat: the XRecord "InitLoadPattern" of the 1st default and defined load pattern
  ;initloadpat1: the XRecord "InitLoadPattern" of the flag that denotes whether the initially defined load pattern has been used before
  ;or not
  ;loadpatdict: the dictionary "LoadPatterns"
  ;loadpatypes: the XRecord "LoadPatternTypes"
  ;lastloadpat: the XRecord "LastLoadPattern"

  (setq dxfgrcd '(1 2 3)) ;the dxf group codes

  (setq initflg (XRecord-get-element initloadpat1 1)) ;get the flag
  (setq ltypes (safeVariant->strlist loadpatypes 3));get available load types. when new types are added, only modify the size

  ;determain conditions depending on flag's value
  (if (eq initflg "0")
    (progn
      ;alter the flag of "InitLoadPattern1" to 1
      (setq dxfgrcd '(1 2) val '(1 "Dead"))
      (populate-XRecord initloadpat1 2 dxfgrcd val)
      ;get information from user
      (setq label (getstring "Specify Load Pattern Name: ")) ;get load pattern's name
      (setq ltype (pool-getkword (XRecord-get-element initloadpat 2) "Specify Load Pattern Type" ltypes)) ;get load pattern's type
      (setq val (list label ltype))
      ;assign values to "LastLoadPattern"
      (populate-XRecord lastloadpat 2 dxfgrcd val)
      ;assign values in the dictionary "LoadPatterns"
      (setq xprop (add-or-getXRecord loadpatdict label))
      (populate-XRecord xprop 2 dxfgrcd val)
      );progn, if there's no conducted definition before [then part]
    (progn
      ;get information from user
      (setq label (getstring "Specify Load Pattern Name: ")) ;get load pattern's name
      (setq ltype (pool-getkword (XRecord-get-element lastloadpat 2) "Specify Load Pattern Type" ltypes)) ;get load pattern's type
      (setq dxfgrcd '(1 2) val (list label ltype))
      ;assign values to "LastLoadPattern"
      (populate-XRecord lastloadpat 2 dxfgrcd val)
      ;assign values in the dictionary "LoadPatterns"
      (setq xprop (add-or-getXRecord loadpatdict label))
      (populate-XRecord xprop 2 dxfgrcd val)
      );progn, if there's any conducted definition before [else part]
    );End if

  (princ) ;clean end
  );End defun

;****

;10- addpierid

(defun C:addpierid (/ dxfgrcd label val xprop)
  ;External variables:-
  ;pierids: the "PierIDs" dictionary that holds all pier ids
  
  (setq dxfgrcd '(1))
  (setq label (getstring "Specify Pier ID: "))
  (setq val (list label))
  (setq xprop (add-or-getXRecord pierids label))
  (populate-XRecord xprop 1 dxfgrcd val)

  (princ) ;clean end
  );End defun

;****

;11- addspandid

(defun C:addspandid (/ dxfgrcd label val xprop)
  ;External variables:-
  ;spandids: the dictioanry "SpandralIDs"
  
  (setq dxfgrcd '(1))
  (setq label (getstring "Specify Spandral ID: "))
  (setq val (list label))
  (setq xprop (add-or-getXRecord spandids label))
  (populate-XRecord xprop 1 dxfgrcd val)

  (princ) ;clean end
  );End defun

;****

;12- def-wall-secprop

(defun C:def-wall-secprop (/ dxfgrcd label tkns val xprop lstmat mat)
  ;External variables:-
  ;wallsprops: the dictionary "WallSecProps" that holds all defined section properties of walls

  (setq dxfgrcd '(1 2 3))
  (setq label (getstring "Specify wall section property name: "))
  (setq tkns (getreal "Specify Thickness: "))
  (setq lstmat (XRecord-get-element lastcprop 1)) ;get the last defined concrete property
  (setq mat (pool-getkword lstmat "Specify material property" (all-elem-in-dict concmat))) ;get material property
  
  (setq val (list label tkns mat))
  (setq xprop (add-or-getXRecord wallsprops label))
  (populate-XRecord xprop 3 dxfgrcd val)

  (princ) ;clean end
  );End defun

;****

;get-interpts: a utility function that takes start and end points and a distance/step, so as to get a list consisting of
;(startpoint intermediate_points endpoint)
(defun get-interpts (sp ep step / dist dif nseg divline ssdivpts i divpt ldiv ptd copt result)
  (setq dist (distance sp ep)) ;get distance between start and end points
  (if (eq (type 1) (type step)) (setq step (atof (itoa step)))) ;insure that "step" is REAL
  (setq dif (- (/ dist step) (atoi (rtos (/ dist step))))) ;get the residual of dist/step
  ;if dist <= step or dist/step < 1.5, just return (startpoint endpoint), otherwise, proceed getting intermediate points
  (if (or (<= dist step) (< (/ dist step) 1.5))
    (progn
      (setq result (list sp ep))
      );End progn [just return (startpoint endpoint)]
    (progn
      ;get number of segments "nseg" on the line connecting startpoint to endpoint
      (if (>= dif 0.5)
	(progn
	  (setq nseg (1+ (- (/ dist step) dif)))
	  );End progn [dif >= 0.5]
	(progn
	  (setq nseg (- (/ dist step) dif))
	  );End progn [dif < 0.5]
	);End if [of getting no. of segements]
      (command "line" sp ep "") ;draw line between startpoint and endpoint
      (setq divline (entlast)) ;get line's entity name
      (setq nseg (atoi (rtos nseg))) ;insure that "nseg" is integer
      
      ;quit if number of segments > 32767
      (if (> nseg 32767)
	(progn
	  (print "Can't proceed; Number of segments > 32767")
	  (alert "Can't proceed; Number of segments > 32767")
	  (quit)
	  );End progn [of nseg > 32767]
	);End if [of nseg > 32767]
      
      (command "divide" (entlast) nseg) ;create points in division locations on the line
      (setq ssdivpts (ssget "_C" sp ep (list '(0 . "POINT") (assoc 8 (entget (entlast)))))) ;get a selection set of newly created points
      (setq i 0 ldiv (sslength ssdivpts))
      
      ;get division points list
      (repeat ldiv
	(progn
	  (setq divpt (ssname ssdivpts i)) ;get division point's entity name
	  (setq divpt (vlax-ename->vla-object divpt)) ;convert it to a vla-object
	  (setq copt (vlax-safearray->list (vlax-variant-value (vla-get-coordinates divpt)))) ;get division point's coordinates
	  (setq result (append result (list copt))) ;append point's coordinates to "result"
	  (setq divpt (vlax-vla-object->ename divpt)) ;get the point's ename
	  (setq elist (append elist (list divpt))) ;append the point's entity name to elist

	  (setq i (1+ i))
	  );End progn [of division points]
	);End repeat [of division points]
      
      ;delete the previously created line and division points
      (entdel divline) ;delete line
      (foreach e elist (entdel e)) ;delete every division point
      (setq ssdivpts nil) ;delete the selection set of the division points
      
      (setq result (csortpts sp result));sort points according to their distance from startpoint
      (setq result (append (list sp) (append result (list ep)))) ;get the final result
      );End progn [proceed getting intermediate points]
    );End if [of getting intermediate points]

  (progn result)
  );End defun

;****

;in-draw-shw: a utility function that takes input data, then draws the wall's elements and assigns structural data to each one of them
(defun in-draw-shw (ptlist sprop pid sid loadstate sload eload loadpat loaddir elembval dpth / npts dif nelempc dload dstep i pt1 pt2 pairlist pairlen pt1_2 pt2_2 kup kdwn j k pxy1 pxy2 initz p1 p2 p3 p4 elemobj elemxdict xpid xsip xload dxfgrcd val lo e elist cecho xsprop xloadpat)
  ;<<The Plan is to get every adjacent points, measure distance, get intermediate points, then draw meshing elements with
  ;the adjacent points and the intermediate points (if they do exist)>>
  ;<<Drawing an element means to draw its geometry and assign its relevant structural data>>
  ;<<Along every depth (per column), a calculated number of elements will be drawn according to "elembval">>
  ;<<All columns must have equal number of elements, to assure connectivity>>
  ;<<Before Drawing, a list of uniform loads on elements (per column) must be constructed>>

  (setq npts (length ptlist)) ;get number of points on stiffline
  ;ensure that "dpth" is REAL
  (if (eq (type dpth) (type 1)) (setq dpth (atof (itoa dpth))))
  ;get the residual of dpth/elembvar [careful if dpth < elembvar]
  (setq dif (- (/ dpth elembval) (atoi (rtos (/ dpth elembval)))))
  ;let the program warn user if dpth < elembvar, otherwise, complete function execution
  (if (>= dpth elembval)
    (progn
      ;calculate the number of elements per column (in variable "nelempc")
      (if (>= dif 0.5)
	(progn
	  (setq nelempc (+ 1 (- (/ dpth elembval) dif)))
	  );End progn [dif >= 0.5]
	(progn
	  (setq nelempc (- (/ dpth elembval) dif))
	  );End progn [dif < 0.5]
	);End if [of no. of elements per column]
      
      (if (eq (type 1.0) (type nelempc)) (setq nelempc (atoi (rtos nelempc))));ensure that "nelempc" is integer
      (setq cecho (getvar 'CMDECHO)) ;get the command prompt echo state
      (setvar "CMDECHO" 0) ;turn off echoing
      
      ;calculate the load slope [the increase of load per element within the column]
      (setq dload (/ (- eload sload) nelempc)) ;delta load
      (setq dstep (/ dpth nelempc)) ;length of element in depth's direction
      ;for every pair of points in ptlist [externally supplied points list]
      (setq i 0)
      (repeat (1- npts)
	(progn
	  (setq pt1 (nth i ptlist) pt2 (nth (+ 1 i) ptlist)) ;get the pair of points
	  (setq pairlist (get-interpts pt1 pt2 elembval)) ;get intermediate points list + start and end points (all of them are sorted)

	  ;for every pair of points in pairlist [list of intermediate points and their start and end points]
	  (setq pairlen (length pairlist) j 0)
	  (repeat (1- pairlen)
	    (progn
	      (setq pt1_2 (nth j pairlist) pt2_2 (nth (1+ j) pairlist)) ;get pair points [of intermediate points]
	      ;get x & y for both points
	      (setq pxy1 (list (nth 0 pt1_2) (nth 1 pt1_2)))
	      (setq pxy2 (list (nth 0 pt2_2) (nth 1 pt2_2)))
	      (setq initz (nth 2 pt1_2)) ;get level of stiffline (from any point)
	      ;for every element in column
	      (setq k 0)
	      (repeat nelempc
		(progn
		  ;construct the element's coordinates
		  (setq kup (list (- initz (* k dstep))))
		  (setq kdwn (list (- initz (* (1+ k) dstep))))
		  (setq p1 (append pxy1 kup)) ;get 1st coordiante
		  (setq p2 (append pxy1 kdwn)) ;get 2nd coordinate
		  (setq p3 (append pxy2 kdwn)) ;get 3rd coordinate
		  (setq p4 (append pxy2 kup)) ;get 4th coordinate

		  (command "3dface" p1 p2 p3 p4 "")(command) ;draw the element
		  
		  (setq elemobj (entlast)) ;get entity name of the element
		  (setq elemobj (vlax-ename->vla-object elemobj)) ;convert it to a vla-object
		  (setq elemxdict (vla-GetExtensionDictionary elemobj)) ;get element's extension dictionary
		  (setq xpid (add-or-getXRecord elemxdict "PierID")) ;get the "PierID" XRecord
		  (setq xsid (add-or-getXRecord elemxdict "SpandralID")) ;get the "SpandralID" XRecord
		  (setq xload (dict-in-xdict elemxdict "WallDistLoads")) ;get the "WallDistLoads" XRecord
		  (setq xloadpat (add-or-getXRecord xload loadpat)) ;get the XRecord of the load pattern
		  (setq xsprop (add-or-getXRecord elemxdict "WallProp")) ;get the "SecProp" XRecord

		  ;fill "SecProp" XRecord
		  (setq dxfgrcd '(1) val (list sprop))
		  (populate-XRecord xsprop 1 dxfgrcd val)

		  ;fill "PierID" XRecord
		  (setq dxfgrcd '(1) val (list pid))
		  (populate-XRecord xpid 1 dxfgrcd val)

		  ;fill "SpandralID" XRecord
		  (setq dxfgrcd '(1) val (list sid))
		  (populate-XRecord xsid 1 dxfgrcd val)

		  ;fill "DistLoads" XRecord
		  (setq lo (* dload (1+ (* 2 k))));get the increse of load
		  (setq lo (* 0.5 (+ lo (* 2 sload))));get load on element
		  (setq dxfgrcd '(1 3 4) val (list lo loaddir loadpat))
		  (populate-XRecord xloadpat 3 dxfgrcd val)

		  (setq k (1+ k))
		  );End progn [for every element in column]
		);End repeat [for every element in column]

	      (setq j (1+ j) k 0)
	      );End progn [of pairs of intermediate points]
	    );End repeat [of pairs of intermediate points]

	  (setq i (1+ i) j 0)
	  );End progn [of looping on points pairs]
	);End repeat [of looping on points pairs]

      (setvar "CMDECHO" cecho) ;reset echoing state
      (print "See the work!") ;indicate successful drawing
      );End progn [proceed execution]
    (progn
      (print "Can't Proceed; Depth is smaller than meshing size!")
      (alert "Can't Proceed; Depth is smaller than meshing size!")
      );End progn [exit with a warning message]
    );End if [of the function]

  (princ) ;clean end
  );End defun

;****

;rem-dup: removes any point that has a simillar one within a point list
(defun rem-dup (ptlist / elem comp n dist dup result)
  (setq n 0)
  (foreach elem ptlist
    (progn
      (foreach comp ptlist
	(progn
	  (setq dist (distance elem comp));get distance between the 2 points (elem & comp)
	  (cond
	    ((and (eq dist 0) (/= n 2)) (progn (setq n (1+ n))));End 1st cond [elem = comp and n /= 2]
	    ((and (eq dist 0) (= n 2)) (progn (setq ptlist (vl-remove elem ptlist)) (setq dup (append dup (list elem))) (setq n 0)));End 2nd cond [elem = comp and n = 2]
	    );End conditional branch
	  );End progn [of 2nd foreach]
	);End 2nd foreach
      );End progn [of 1st foreach]
    );End 1st foreach
  (setq result (append ptlist dup))
  (print result);debug line
  (quit);debug line
  );End defun

;****

;13- draw-shw

(defun C:draw-shw (/ stiffline0 stiffline stiffline1 beamsonwall spoint epoint i kwb intpt temppt ptlist dxfgrcd val flgpid flgsid flgwsp lpid lsid lwsp pid sid wsp xprop idtype loadstate sload eload flglpat lpat lapat ldir kwlist flgdpth ladpth dpth elembval osmode)
  ;External variables:-
  ;pierids: the dictionary "PierIDs"
  ;spandids: the dictionary "SpandralIDs"
  ;wallsprops: the dictionary "WallSecProp"
  ;loadpatdict: the dictionary "LoadPatterns"
  ;initpierid1: the XRecord of last usage flag for pier ID assignment
  ;initpierid: the XRecord of initial value of pier ID to be assigned
  ;lastpierid: the XRecord of the last assigned pier ID
  ;initspandid1: the XRecord of last usage flag for spandral ID assignment
  ;initspandid: the XRecord of initial value of spandral ID to be assigned
  ;lastspandid: the XRecord of last assigned spandral ID
  ;initwallsprop1: the XRecord of last usage flag for wall section property assignment
  ;initwallsprop: the XRecord of the initial wall section property to be asigned
  ;lastwallsprop: the XRecord of the last assigned wall section property
  ;initwallloadpat1: the XRecord of last usage flag for load pattern assignment
  ;initwallloadpat: the XRecord of the initial value of load pattern to be assigned
  ;lastwallloadpat: the XRecord of the last assigned load pattern on walls
  ;initwalldepth1: the XRecord of the last usage flag for wall depth
  ;initwalldepth: the XRecord of the initial value of wall depth
  ;lastwalldepth: the XRecord of the last assigned wall depth

  ;<<The Plan is to get all intersection points with beams rested on wall, then derive the needed dimensions for meshing>>
  ;Select the stiff line (representing the shearwall) and intersected beams
  (print "[[Select ONLY the wall's stiff line]]")
  (setq stiffline0 (ssget "_:S" '((-4 . "<OR") (0 . "LINE") (0 . "LWPOLYLINE") (0 . "SPLINE") (-4 . "OR>"))))
  (setq stiffline (ssname stiffline0 0));get the line's ename
  (setq stiffline1 stiffline)
  (setq stiffline0 nil);no need for this selection set anymore
  (setq stiffline (vlax-ename->vla-object stiffline)) ;convert stiff line to a vla-object

  ;get stiffline's start and end points
  (setq spoint (vla-get-startpoint stiffline)) ;get stiffline's start point
  (setq spoint (vlax-safearray->list (vlax-variant-value spoint))) ;convert it to list
  (setq epoint (vla-get-endpoint stiffline)) ;get stiffline's end point
  (setq epoint (vlax-safearray->list (vlax-variant-value epoint))) ;convert it to list
  
  (setq kwb (pool-getkword "Yes" "Is the Wall connected with beam(s)/slab(s)?" '("Yes" "No")))
  (if (eq kwb "Yes")
    (progn
  (print "[[Select beam(s)/slab(s) rested on wall. Selection of stiff line has no effect on results]]")
  ;(setq stiffline1 (cdr (assoc 5 (entget stiffline1))))
  ;(setq nostiff (list '(-4 . "<OR") (cons -4 "<NOT") (cons 5 stiffline1) (cons -4 "NOT>") '(0 . "LINE") '(0 . "3DFACE") '(0 . "LWPOLYLINE") '(-4 . "OR>"))) ;stiffline is not selectable
  ;(print nostiff);debug line
  (setq beamsonwall (ssget))
  (ssdel stiffline1 beamsonwall) ;delete the stiffline if selected within the selection set
  
  ;get intersection points
  
  (setq i 0)
  (repeat (sslength beamsonwall)
    (progn
      (setq objid (ssname beamsonwall i)) ;get beam's entity name
      (setq objid (vlax-ename->vla-object objid)) ;convert beam to a vla-object
      (setq intpt (vla-intersectwith stiffline objid acExtendNone)) ;get the variant safearray containing the intersection point
      (setq temppt (vlax-safearray->list (vlax-variant-value intpt)));get beam's intersection point

      ;for lines, things go perfectly, for 3dface with 2-point intersection, arrange them in temppt
      (if (eq (vla-get-objectname objid) "AcDbFace")
	(progn
	  (if (eq 6 (length temppt))
	    (setq temppt (list (list (nth 0 temppt) (nth 1 temppt) (nth 2 temppt)) (list (nth 3 temppt) (nth 4 temppt) (nth 5 temppt))));[then part]
	    ;no else part
	    );End if [to check whether the 3Dface intersects stiffline at one or two point]
	  (if (eq 2 (length temppt))
	    (setq ptlist (append ptlist temppt));then part [2-point intersection]
	    (setq ptlist (append ptlist (list temppt)));else part [single-point intersection]
	    );End if [of appending temppt to ptlist]
	  );End progn [for 3DFaces of 2-point intersection]
	(progn
	  (setq ptlist (append ptlist (list temppt))) ;add intersection point to points list
	  );End progn [for lines]
	);End if [for 3DFaces of 2-point intersection]
      
      (setq i (1+ i))
      );End progn [of repeat]
    );End repeat
  ;(setq ptlist (rem-dup ptlist)) ;remove duplicate points **_new_**
  (setq ptlist (vl-remove spoint ptlist));remove start point from point list
  (setq ptlist (vl-remove epoint ptlist));remove end point from point list
  (setq ptlist (csortpts spoint ptlist)) ;sort points according to their distances from stiffline's start point
  (setq ptlist (append (list spoint) (append ptlist (list epoint)))) ;create the full list of points
  );End progn [then part]
    (progn
      (setq ptlist (list spoint epoint)) ;create the full list of points
      );End progn [else part]
    );End if

  ;get the wall's section property & pier or spandral ID
  (setq flgpid (XRecord-get-element initpierid1 1)) ;get flag of pier ID
  (setq flgsid (XRecord-get-element initspandid1 1)) ;get flag of spandral ID
  (setq flgwsp (XRecord-get-element initwallsprop1 1)) ;get flag of section property

  ;1-get section property (in "wsp" variable)
  (if (eq flgwsp "0")
    (progn
      ;alter the flag to 1
      (setq dxfgrcd '(1) val '(1))
      (populate-XRecord initwallsprop1 1 dxfgrcd val)
      ;get information from user
      (setq lwsp (XRecord-get-element initwallsprop 1)) ;get the inital value
      (setq wsp (pool-getkword lwsp "Specify Section Property" (all-elem-in-dict wallsprops))) ;get section property label
      ;update the "LastWallSecProp" XRecord
      (setq val (list wsp))
      (populate-XRecord lastwallsprop 1 dxfgrcd val)
      );End progn [then part]
    (progn
      ;get information from user
      (setq lwsp (XRecord-get-element lastwallsprop 1)) ;get the last used value
      (setq wsp (pool-getkword lwsp "Specify Section Property" (all-elem-in-dict wallsprops))) ;get section property label
      ;update the "LastWallSecProp" XRecord
      (setq dxfgrcd '(1) val (list wsp))
      (populate-XRecord lastwallsprop 1 dxfgrcd val)
      );End progn [else part]
    );End if

  ;2-get pier & spandral IDs, only one type will be assigned and the other will take the value "None"
  ;(in variables "pid" & "sid")
  (setq idtype (pool-getkword "Pier" "Specify Wall Type" '("Pier" "Spandral"))) ;get wall type
  (if (eq idtype "Pier")
    (progn
      (setq flgpid (XRecord-get-element initpierid1 1)) ;get pier ID's flag
      (setq sid "None") ;set Spandral ID to "None"
      (if (eq flgpid "0")
	(progn
	  ;alter the flag to 1
	  (setq dxfgrcd '(1) val '(1))
	  (populate-XRecord initpierid1 1 dxfgrcd val)
	  ;get information from user
	  (setq lpid (XRecord-get-element initpierid 1)) ;get the initial value
	  (setq pid (pool-getkword lpid "Specify Pier ID" (all-elem-in-dict pierids))) ;get Pier ID
	  ;update "LastPierID" XRecord
	  (setq val (list pid))
	  (populate-XRecord lastpierid 1 dxfgrcd val)
	  );End progn [Pier ID not previously assigned]
	(progn
	  ;get information from user
	  (setq lpid (XRecord-get-element lastpierid 1)) ;get the last assigned value
	  (setq pid (pool-getkword lpid "Specify Pier ID" (all-elem-in-dict pierids))) ;get Pier ID
	  ;update "LastPierID" XRecord
	  (setq dxfgrcd '(1) val (list pid))
	  (populate-XRecord lastpierid 1 dxfgrcd val)
	  );End progn [Pier ID previously assigned]
	);End if [for Pier]
      );End progn [if Pier]
    (progn
      (setq flgsid (XRecord-get-element initspandid1 1)) ;get spandral ID's flag
      (setq pid "None") ;set Pier ID to "None"
      (if (eq flgsid "0")
	(progn
	  ;alter the flag to 1
	  (setq dxfgrcd '(1) val '(1))
	  (populate-XRecord initspandid1 1 dxfgrcd val)
	  ;get information from user
	  (setq lsid (XRecord-get-element initspandid1 1)) ;get the initial value
	  (setq sid (pool-getkword lsid "Specify Spandral ID" (all-elem-in-dict spandids))) ;get Spandral ID
	  ;update "LastSpandralID" XRecord
	  (setq val (list sid))
	  (populate-XRecord lastspandid 1 dxfgrcd val)
	  );End progn [Spandral ID not previously assigned]
	(progn
	  ;get information from user
	  (setq lsid (XRecord-get-element lastspandid 1)) ;get the last assigned value
	  (setq sid (pool-getkword lsid "Specify Spandral ID" (all-elem-in-dict spandids))) ;get Spandral ID
	  ;update "LastSpandralID" XRecord
	  (setq dxfgrcd '(1) val (list sid))
	  (populate-XRecord lastspandid 1 dxfgrcd val)
	  );End progn [Spandral ID previously assigned]
	);End if [for Spandral]
      );End progn [if Spandral]
    );End if
  
  ;Get loading state & its values ("sload" "eload" "lpat" "ldir")
  (setq loadstate (pool-getkword "None" "Specify Loading State" '("None" "Uniform" "Gradual")))
  (setq flglpat (XRecord-get-element initwallloadpat1 1)) ;get usage flag of load pattern
  (setq kwlist '("Local-1" "lOcal-2" "loCal-3" "globalX" "globalY" "Gravity" "globalXProj" "globalYProj" "gravityProj"))
  (cond
    ((eq loadstate "None")
     (progn
       (setq sload 0 eload 0 lpat "Dead" ldir 6)
       );End progn [of "None" Loading State]
     );"None" Loading State
    ((eq loadstate "Uniform")
     (progn
       (if (eq flglpat "0")
	 (progn
	   ;alter flag to 1
	   (setq dxfgrcd '(1) val '(1))
	   (populate-XRecord lastwallloadpat 1 dxfgrcd val)
	   ;get information from user
	   (setq lapat (XRecord-get-element initwallloadpat 1)) ;get the initial value
	   (setq lpat (pool-getkword lapat "Specify Load Pattern" (all-elem-in-dict loadpatdict))) ;get load pattern
	   (setq ldir (get-dir (pool-getkword "loCal-3" "Specify Load Direction" kwlist))) ;get load direction
	   ;update "LastWallLoadPat" XRecord
	   (setq val (list lpat))
	   (populate-XRecord lastwallloadpat 1 dxfgrcd val)
	   );End progn [load pattern not previously assigned]
	 (progn
	   ;get information from user
	   (setq lapat (XRecord-get-element lastwallloadpat 1)) ;get the last assigned load pattern
	   (setq lpat (pool-getkword lapat "Specify Load Pattern" (all-elem-in-dict loadpatdict))) ;get load pattern
	   (setq ldir (get-dir (pool-getkword "loCal-3" "Specify Load Direction" kwlist))) ;get load direction
	   ;update "LastWallLoadPat" XRecord
	   (setq dxfgrcd '(1) val (list lpat))
	   (populate-XRecord lastwallloadpat 1 dxfgrcd val)
	   );End progn [load pattern previously assigned]
	 );End if [of load pattern assignment]
       (setq sload (getreal "Specify Load Value: "))
       (setq eload sload)
       );End progn [of "Uniform" Loading state]
     );"Uniform" Loading State
    ((eq loadstate "Gradual")
     (progn
       (if (eq flglpat "0")
	 (progn
	   ;alter flag to 1
	   (setq dxfgrcd '(1) val '(1))
	   (populate-XRecord initloadpat1 1 dxfgrcd val)
	   ;get information from user
	   (setq lapat (XRecord-get-element initwallloadpat 1)) ;get the initial value
	   (setq lpat (pool-getkword lapat "Specify Load Pattern" (all-elem-in-dict loadpatdict))) ;get load pattern
	   (setq ldir (get-dir (pool-getkword "loCal-3" "Specify Load Direction" kwlist))) ;get load direction
	   ;update "LastWallLoadPat" XRecord
	   (setq val (list lpat))
	   (populate-XRecord lastwallloadpat 1 dxfgrcd val)
	   );End progn [load pattern not previously assigned]
	 (progn
	   ;get information from user
	   (setq lapat (XRecord-get-element lastwallloadpat 1)) ;get the last assigned load pattern
	   (setq lpat (pool-getkword lapat "Specify Load Pattern" (all-elem-in-dict loadpatdict))) ;get load pattern
	   (setq ldir (get-dir (pool-getkword "loCal-3" "Specify Load Direction" kwlist))) ;get load direction
	   ;update "LastWallLoadPat" XRecord
	   (setq dxfgrcd '(1) val (list lpat))
	   (populate-XRecord lastwallloadpat 1 dxfgrcd val)
	   );End progn [load pattern previously assigned]
	 );End if [of "Gradual" Loading State]
       (setq sload (getreal "Specify Start Load Value (at stiffline's level): "))
       (setq eload (getreal "Specify End Load Value (at wall's bottom level): "))
       );End progn [of "Gradual" Loading State]
     );"Gradual" Loading State
    );End conditional branching

  (setq elembval (ord-getreal 0.75 "Specify best dimension for meshing elements")) ;get the best dimension for meshing elements

  ;get wall's depth
  (setq flgdpth (XRecord-get-element initwalldepth1 1)) ;get wall's depth flag
  (if (eq flgdpth "0")
    (progn
      ;alter flag to 1
      (setq dxfgrcd '(1) val '(1))
      (populate-XRecord initwalldepth1 1 dxfgrcd val)
      ;get information from user
      (setq ladpth (XRecord-get-element initwalldepth 1)) ;get initial value
      (setq dpth (ord-getreal (atof ladpth) "Specify Depth")) ;get wall depth
      ;update "LastWallDepth" XRecord
      (setq val (list dpth))
      (populate-XRecord lastwalldepth 1 dxfgrcd val)
      );End progn [depth not previously assigned]
    (progn
      ;get information from user
      (setq ladpth (XRecord-get-element lastwalldepth 1)) ;get last assigned wall depth
      (setq dpth (ord-getreal (atof ladpth) "Specify Depth")) ;get wall depth
      ;update "LastWallDepth" XRecord
      (setq dxfgrcd '(1) val (list dpth))
      (populate-XRecord lastwalldepth 1 dxfgrcd val)
      );End progn [depth previously assigned]
    );End if [of wall depth assignment]
  
  ;Now, let's draw the wall elements based on:-
  ;1- element's best chosen dimension value for meshing (from user)
  ;2- depth of wall from the stiffline (from user)
  ;3- distance between every adjacent pair of points (from drawing)
  ;4- assign section property & pier/spandral ID for every element
  (setq osmode (getvar 'OSNAPCOORD)) ;get object snap overwriting system variable
  (setvar "OSNAPCOORD" 1)
  (in-draw-shw ptlist wsp pid sid loadstate sload eload lpat ldir elembval dpth)
  (setvar "OSNAPCOORD" osmode)

  (princ) ;clean end
  );End defun

;****

;assign-mat-solidflatslabs: a utility function that "really" works for the redefinition of solid and flat slab material properties
(defun assign-mat-solidflatslabs (ty lastchmatslab lastmatslabs / kwlist sprop ls lmat xmat uw xsprop tksap tketabs dxfgrcd val)
  ;External variables:-
  ;concmat: the dictionary "ConcMaterial"

  ;lastchmatslab: the lastly changed section property
  ;lastmatslab: the lastly assigned material property

  (if (eq ty "solid")
    (setq kwlist (list "SOLID120" "SOLID140" "SOLID150" "SOLID160")) ;list of solid slab section properties [then part]
    (setq kwlist (list "FLAT180" "FLAT200" "FLAT220" "FLAT240" "FLAT250" "FLAT260")) ;list of flat slab section properties [else part]
    );End if [of kwlist]
  
  (setq ls (XRecord-get-element lastchmatslab 1)) ;get the last changed section property
  (setq lmat (XRecord-get-element lastmatslabs 1)) ;get the last used material property
  (setq sprop (pool-getkword ls "Specify Section Property" kwlist)) ;get the section property

  (setq lmat (pool-getkword lmat "Specify Material Property" (all-elem-in-dict concmat)))
  (setq xmat (add-or-getXRecord concmat lmat))
  (setq uw (atof (XRecord-get-element xmat 9))) ;get material's unit weight

  (setq xsprop (add-or-getXRecord slbsprop sprop)) ;get XRecord of section property
  (setq tksap (atof (XRecord-get-element xsprop 3))) ;get SAP2000 thickness
  (setq tketabs (atof (XRecord-get-element xsprop 4))) ;get ETABS thickness
  (setq uw (* tketabs uw)) ;get slab's unit weight
  (setq val (list sprop lmat tksap tketabs uw) dxfgrcd '(1 2 3 4 6))
  (populate-XRecord xsprop 5 dxfgrcd val) ;redefine section peoperty

  ;update "LastChMatSolidSlab"
  (setq dxfgrcd '(1) val (list sprop))
  (populate-XRecord lastchmatslab 1 dxfgrcd val)

  ;update "LastMatSolidSlabs"
  (setq val (list lmat))
  (populate-XRecord lastmatslabs 1 dxfgrcd val)

  (princ) ;clean end
  
  );End defun

;****

;14- assign-mat-solidslab

(defun C:assign-mat-solidslab (/)
  ;External variables:-
  ;lastmatsolidslabs: the last assigned material property
  ;lastchmatsolidslab: the last changed solid slab

  (assign-mat-solidflatslabs "solid" lastchmatsolidslab lastmatsolidslabs)
  );End defun

;****

;15- assign-mat-flatslab

(defun C:assign-mat-flatslab (/)
  ;External variable:-
  ;lastmatflatslabs: the last assigned material property
  ;lastchmatflatslab: the last changed flat slab

  (assign-mat-solidflatslabs "flat" lastchmatflatslab lastmatflatslabs)
  );End defun

;****

;set-fixedslabs: a utility function that assigns a reserved slab property to a set of slabs
(defun set-fixedslabs (kwlist lastslab / sslines lprop xprop dxfgrcd val)
  (setq sslines (ssget '((0 . "3DFACE"))))
  (setq lprop (XRecord-get-element lastslab 1)) ;get last used slab property
  (setq sprop (pool-getkword lprop "Specify Section Property" kwlist)) ;get section property
  (setq xprop (add-or-getXRecord slbsprop sprop))
  (setq uw (atof (XRecord-get-element xprop 6))) ;get slab's unit weight
  (setq dxfgrcd '(1) val (list sprop))
  (fillSS-XRecord-inExtDict sslines "SecProp" 1 dxfgrcd val) ;assign section property
  ;assign unit weight to "Dead" load pattern
  (in-set-areas-distload sslines uw 6 "Dead") ;6 is the Gravity load direction value for SAP2000 and ETABS

  (princ) ;clean end
  );End defun

;****

;16- set-solidslabs

(defun C:set-solidslabs (/ kwlist)
  ;External variables:-
  ;lastsolidslabsprop: the last assigned solid slab

  (setq kwlist '("SOLID120" "SOLID140" "SOLID150" "SOLID160"))
  (set-fixedslabs kwlist lastsolidslabsprop)
  
  );End defun

;****

;17- set-flatslabs

(defun C:set-flatslabs (/ kwlist)
  ;Externalvariables:-
  ;lastflatslabsprop: the last assigned flat slab

  (setq kwlist '("FLAT180" "FLAT200" "FLAT220" "FLAT240" "FLAT250" "FLAT260"))
  (set-fixedslabs kwlist lastflatslabsprop)
  
  );End defun

;****

;18- set-restrains

(defun C:set-restrains (/ sslines lr restype kwlist dxfgrcd val)
  ;External variables:-
  ;lastrestrain: the last used restrain type

  (setq sslines (ssget '((0 . "POINT"))))
  (setq lr (XRecord-get-element lastrestrain 1)) ;get last used restrain type
  (setq restype (pool-getkword lr "Specify Restrain Type" '("Hinged" "Fixed"))) ;get restrain type

  (setq dxfgrcd '(1) val (list restype))
  (populate-XRecord lastrestrain 1 dxfgrcd val) ;update lastrestrain
  (fillSS-XRecord-inExtDict sslines "Restrain" 1 dxfgrcd val) ;assign restrain type
  );End defun

;****

;19-delines

(defun C:delines (/ sslines len elem entlist elem2)
  (setq sslines (ssget '((0 . "LINE"))))
  (setq len (sslength sslines) i 0) ;number of elements
  (repeat len
    (progn
      (setq elem (ssname sslines i))
      (setq entlist (append entlist (list elem)))
      (setq i (1+ i))
      );End progn [of repeat]
    );End repeat
  (foreach elem2 entlist
    (progn
      (entdel elem2)
      );End progn [of foreach]
    );End foreach
  (princ) ;clean end
  );End defun

;****