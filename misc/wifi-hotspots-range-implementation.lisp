(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn 
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
					   (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
	(load quicklisp-init)))
    (require :cl-ppcre)))

(defpackage :wifi-classifier-prototype
  (:use :cl :cl-ppcre))

(in-package :wifi-classifier-prototype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Basic loading of files ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-wifi-formatted-file (file)
  "Loads the file passed in, returns an ALIST of WIFI mac addresses, and the abs signal strength"
  (let ((mac-addr-db '()))
    (with-open-file (f file :direction :input) 
      (loop for l = (read-line f nil nil)
	    for l-split = (split "\\\t" l)
	    for mac-addr = (first l-split)
	    for signal-str = (second l-split)
	    while l
	    do
	       (if (null (cdr (assoc mac-addr mac-addr-db :test #'string-equal)))
		   (push (cons mac-addr (list (abs (read-from-string signal-str))))
			 mac-addr-db)
		   (push (abs (read-from-string signal-str))
			 (cdr (assoc mac-addr mac-addr-db :test #'string-equal))))))
    mac-addr-db))

(defun load-assoc-files (filelist)
  "Given a list of files, in the format of:
ROOM_NAME . FILE_NAME
we'll return an ALIST such that it's:
 (ROOM_NAME . ((MAC_ADDR . '(LIST_OF_ADDRS))
               (...)))
"
  (loop for f in filelist
	for room_label = (car f)
	for file = (cdr f)
	for loaded-file = (load-wifi-formatted-file file)
	while f
	collect (cons room_label loaded-file)))


(defun rangeify-range (strengths)
  (let ((smin (apply #'min strengths))
	(smax (apply #'max strengths)))
    `((min . ,smin)
      (max . ,smax))))

(defun assoc-files-to-room-range (loaded-afiles)
  "Given the output from load-assoc-files, we simply transform the file to be something like:
 (ROOM_NAME . ((MAC_ADDR_1 MIN_RANGE MAX_RANGE)
               (MAC_ADDR_2 MIN_RANGE ...)
               ...)
  ..)"
  (let* ((new-room-alist '()))
    (loop for room-alist in loaded-afiles
	  for room = (car room-alist)
	  for mac-addr-list = (cdr room-alist)
	  do
	     (push (cons room
			 (loop for m in mac-addr-list
			       for mac-addr = (car m)
			       for strengths = (cdr m)
			       collect (cons mac-addr (rangeify-range strengths))))
		   new-room-alist))
    new-room-alist))

;;;; MBA
;; (defvar *room-alist-data-store*
;;   '(("4505" . "/Users/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4505.reformatted.csv")
;;     ("4511" . "/Users/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4511.reformatted.csv)
;;     ("4th-floor-hallway" . "/Users/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_hallwayreformatted.csv")))
;; 

;;;; Laptop
(defvar *room-alist-data-store*
    '(("4505" . "/home/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4505.reformatted.csv")
      ("4511" . "/home/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4511.reformatted.csv")
      ("4th-floor-hallway" . "/home/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_hallway.reformatted.csv")))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Classification Options ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *room-ranges* '())

(defun in-bucketp (sig mac-range)
  (let ((mr-min (cdr (assoc 'min mac-range)))
	(mr-max (cdr (assoc 'max mac-range))))
    (and (<= sig mr-max)
	 (>= sig mr-min))))

(defun eager-load-ranges ()
  "Simply laods our ranges into the *room-ranges* variable, which is
used for everything in this block"
  (setf *room-ranges* 
	(assoc-files-to-room-range (load-assoc-files *room-alist-data-store*))))

(defun intersect-overlaps (signal-list room-ranges)
  "Given a listing of signals in the form of:
 MAC_ADDR . <signal_strength> as a list, and room ranges in the form of:
 MAC_ADDR . <range>
 we will loop through the signal-list and apply it to the room-ranges
 and will return an ALIST of pruned elements in the form of:
 MAC_ADDR . <signal>"
  (let ((subset-signals '()))
    (loop for s in signal-list
	  for smac = (car s)
	  for sstr = (cdr s)
	  do
	     (loop for sr in room-ranges
		   for mac-addr = (car sr)
		   for range-struct = (cdr sr)
		   when
		   (and (string-equal smac mac-addr)
			(in-bucketp sstr range-struct))
		   do
		      (push (cons smac sstr) subset-signals)))
    subset-signals))

(defun determine-overlaps (signal-list)
  "Loop through the signal-list"
  (loop for room in *room-ranges*
	for room-name = (car room)
	for room-ranges = (cdr room)
	for overlaps = (intersect-overlaps signal-list room-ranges)
	collect
	(cons room-name (length overlaps)) into overlap-struct
	finally (return (nreverse overlap-struct))))

(defun classify-location (signal-list &key (force-reload-p 'nil))
  "Given a signal-list in the format of:
 (MAC_ADDR_1 <signal_strength>
  MAC_ADDR_2 <signal_strength_2)"
  (when (or (null *room-ranges*)
	    (not (null force-reload-p)))
    (eager-load-ranges))
  (determine-overlaps signal-list))

;;;p;;;;;;;;

(defun output-java-csv-ranges (file &key (force-reload-p 'nil))
  "Outputs a CSV file in the format of:
ROOM,MAC_ADDR,RANGE_LOW,RANGE_HIGH"
  (when (or (null *room-ranges*)
	    (not (null force-reload-p)))
    (eager-load-ranges))
  (with-open-file (f file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for room in *room-ranges*
	  for room-name = (car room)
	  for room-ranges = (cdr room)
	  do
	     (loop for rr in room-ranges
		   for mac-addr = (car rr)
		   for ranges-struct = (cdr rr)
		   for min = (cdr (assoc 'min ranges-struct))
		   for max = (cdr (assoc 'max ranges-struct))
		   do
		      (format f "~a,~a,~a,~a~%" room-name mac-addr min max)))))
