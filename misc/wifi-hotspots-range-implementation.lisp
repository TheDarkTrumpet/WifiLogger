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
	    for l-split = (split "," l)
	    for ssid = (first l-split)
	    for mac-addr = (second l-split)
	    for signal-str = (third l-split)
	    while l
	    when (not (scan "^SSID,Mac_Addr,Strength.*" l))
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

(setf *room-alist-data-store*
  '(("219JH" . "/mnt/data.temporary/WifiLogger_Trace_Files/219jh.2012.12.03_07.50.27.csv")
    ("221JH" . "/mnt/data.temporary/WifiLogger_Trace_Files/221jh.2012.12.03_07.48.51.csv")
    ("248JH" . "/mnt/data.temporary/WifiLogger_Trace_Files/248jh.2012.12.03_07.44.35.csv")
    ("JH-2-HALL" . "/mnt/data.temporary/WifiLogger_Trace_Files/jh2ndhall.2012.12.03_07.46.49.csv")
    ("4517SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/4517sc.2012.12.07_16.44.27.csv")
    ("5309SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/5309SC.2012.12.08_15.37.22.csv")
    ("5315SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/5315SC.2012.12.08_15.39.15.csv")
    ("5317SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/5317SC.2012.12.08_15.33.27.csv")
    ("5309SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/5309SC.2012.12.08_15.37.22.csv")
    ("SC-5-HALL" . "/mnt/data.temporary/WifiLogger_Trace_Files/5thHallSC.2012.12.08_15.35.06.csv")
    ("5013SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/5013SC.2012.12.08_16.07.53.csv")
    ("5020SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/5020SC.2012.12.08_16.03.47.csv")
    ("5312SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/5312SC.2012.12.08_16.02.16.csv")
    ("5316SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/5316SC.2012.12.08_16.01.11.csv")
    ("5318SC" . "/mnt/data.temporary/WifiLogger_Trace_Files/5318SC.2012.12.08_16.00.04.csv")))

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
