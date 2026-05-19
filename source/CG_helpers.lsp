;;#################################################################################
;;#################################################################################
;;## CG HELPER ROUTINES (defined below)
;;## 
;;## Are used by the main function(s) (separate files) to determine the 
;;## Center of Gravity of one or more AutoCAD objects
;;##
;;## Thrown together haphazardly by AKH
;;## VERSION / DATE: MAY 2026
;;#################################################################################
;;#################################################################################
;; This gives you access to symbolp, vl-princ-to-string, and other VL functions in any LISP environment.
;; it is loaded as soon as appload or startup script opens this LSP file
;; autocad still couldn't find the "vl-stringp" command for some reason
;;
(vl-load-com)
;;
;; ================================================================================
;; DEBUGGING CODE SYNTAX FOR LATER REFERENCE
;; ================================================================================
;; (prompt "\nC[DEBUG_POINT] A reached.") ;; DEBUGGING CHECKPOINT
;; (print hdrObjProp)  ;; DEBUGGING CHECKPOINT
;; (TypeSize hdrObjProp "hdrObjProp") ;; DEBUGGING
;; (prompt (strcat "\nVariable i = " (vl-prin1-to-string i))) ;; DEBUGGING
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; Helper function definition to browse for folder with dialog box
;; ================================================================================
(defun hlphlpBrowseFolder ( / objShell objFolder strPath)
  (vl-load-com)
  (setq objShell  (vlax-create-object "Shell.Application"))
  (setq objFolder (vlax-invoke-method objShell 'BrowseForFolder 0 "Select Folder" 0))
  (if objFolder
    (progn
      (setq strPath (vlax-get-property (vlax-get-property objFolder 'Self) 'Path))
      (vlax-release-object objShell)
      strPath
    )
    (progn
      (vlax-release-object objShell)
      nil
    )
  )
)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; Helper Function 
;; to get the name of the active drawing units.
;; ================================================================================
;; ================================================================================

(defun hlpUnitName ( / intUnitsSys arrUnits )
	(setq intUnitsSys (getvar "INSUNITS"))

	(setq arrUnits
		'("Unspecified" "Inches" "Feet" "Miles" "Millimeters" "Centimeters"
		"Meters" "Kilometers" "Microinches" "Mils" "Yards" "Angstroms"
		"Nanometers" "Microns" "Decimeters" "Decameters" "Hectometers"
		"Gigameters" "Astronomical Units" "Light Years" "Parsecs"
		"US Survey Feet" "US Survey Inch" "US Survey Yard" "US Survey Mile")
	)

	(if (and (>= intUnitsSys 0) (< intUnitsSys (length arrUnits)))
		(nth intUnitsSys arrUnits)
		"Unknown"
	)
)
;; ================================================================================
;; ================================================================================





;; ================================================================================
;; ================================================================================
;; Save Array as text / csv file
;;
;; sample use:
;; (ArrayToTxt arrWeight)
;; ================================================================================
;; ================================================================================
(defun hlpArrayToTxt (arrData / strFilePath fileHandle row line)
	;; Show Save As dialog
	(setq strFilePath (getfiled "Save CSV As" "" "csv" 1))

	(if strFilePath
		(progn
			;; Open file for writing
			(setq fileHandle (open strFilePath "w"))

			;; Loop through each row in 2D array
			(foreach row arrData
				(setq line "")
				
				(foreach item row
				  
				  (setq line (strcat line (vl-prin1-to-string item) ","))
				  
				)
				
				;; Trim trailing comma and write line
				(write-line (vl-string-trim "," line) fileHandle)
				
			)

			;; Close file and return path
			(close fileHandle)
			strFilePath
			)
		;; ELSE statement. User canceled dialog
		nil
	)
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Debugging Helper Function that will get the size and type of an array 
;; and print it to the autocad console
;; sample use:
;; (TypeSize arrObjProp "arrObjProp") ;; [DEBUG TypeSize]
;; type out the string "arrObjProp" (or whatever) so it prints out while debugging
;; ================================================================================
;; ================================================================================
(defun TypeSize (thing name / typeName rowCount colCount)
  (cond
    ;; Not A List or Array
    ((not (listp thing))
     (prompt (strcat "\n[DEBUGTypeSize] " name " is not a list: " (vl-prin1-to-string thing)))
    )

    ;; Flat 1D List
    ((not (listp (car thing)))
     (setq typeName "list")
     (setq rowCount (length thing))
     (prompt (strcat "\n[DEBUGTypeSize] " name " is a " typeName " with length " (itoa rowCount)))
    )

    ;; 2D array 
    ((listp (car thing))
     (setq typeName "array")

     ;; Fallback in case arrOut is nil or malformed
     (if thing
       (setq rowCount (length thing))
       (setq rowCount 0)
     )

     ;; Fallback for malformed first row
     (if (and (car thing) (listp (car thing)))
       (setq colCount (length (car thing)))
       (setq colCount 0)
     )

     (prompt
       (strcat
         "\n[DEBUGTypeSize] "
         name " is an " typeName
         " with size "
         (itoa rowCount)
         " x "
         (itoa colCount)
       )
     )
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to convert a 1D list/array to a 2D definition
;; that can be used as a wrapper
;; in case a calling function is expecting a 2D result
;; ================================================================================
;; ================================================================================
(defun To2D (arr1D)
  (mapcar '(lambda (x) (list x)) arr1D)
)


;; ================================================================================
;; ================================================================================
(defun To1D (arr2D / result)
  (cond
    ;; If it's a 1-row 2D list (like: (("A" "B" "C")))
    ((and (listp arr2D) (= (length arr2D) 1))
     (car arr2D)
    )

    ;; If it's a 1-column 2D list (like: (("A") ("B") ("C")))
    ((and (listp arr2D) (= (length (car arr2D)) 1))
     (mapcar 'car arr2D)
    )

    ;; If it's not 1-row or 1-column, return as-is or nil
    (T
     (prompt "\n[DEBUGARRAY] To1D: Input is not a 1-row or 1-column 2D array.")
     nil
    )
  )
)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; Helper Function to search a list and return the index value of the matching element
;;
;; SAMPLE USE:
;; (setq hdrObjProp (list "ObjectID" "ObjectName" "Layer" "Density" "Volume" "Centroid X" "Centroid Y" "Centroid Z"))
;; (setq i (hlpIndexOfMatch hdrObjProp "*DENS*")) ; case-insensitive search
;; ================================================================================
;; ================================================================================
(defun hlpIndexOfMatch (arr pattern / i result)
	
	(setq i 0)
	(setq result nil)
  
	(cond
		;; it's not a list at all
		((not (listp arr))
			(setq typeName nil)
		)

		;; it's a 1D array or list
		;; this is okay, we don't need to do anything...continue...
		((not (listp (car arr)))
			 (setq typeName "list")
		)

		;; it's a 2D array
		;; need (at least temporarily) to be a 1D array or list
		((listp (car arr)) ; 2D array
			(setq typeName "array")
		)
	)
	
	(if (= "array" typeName)
		(setq arr (To1D arr))
	)
  
	(while (and arr (not result))
		(if (wcmatch (strcase (car arr)) (strcase pattern))
			(setq result i)
			(setq i (1+ i))
		)
		(setq arr (cdr arr))
	)
	result
)

;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; hlpTranspose function to effectively conver a 2x7 array to a 7x2 array
;; should work for either 1D or 2D arrays
;; ================================================================================
;; ================================================================================
(defun hlpTranspose (arr / rowCount colCount result)
  (cond
    ;; If 1D list, convert to vertical 2D
    ((not (listp (car arr)))
     (mapcar '(lambda (x) (list x)) arr)
    )

    ;; If 2D, hlpTranspose rows and columns
    ((listp (car arr))
     (setq colCount (length (car arr)))
     (setq result '())
     (repeat colCount
       (setq result
         (append result
           (list (mapcar '(lambda (row) (car row)) arr))
         )
       )
       ;; Strip first item off each row for next column
       (setq arr (mapcar 'cdr arr))
     )
     result
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to Extract
;; from a 2D array, copy/extract a single column into a new array
;;
;; sample user
;; (setq arrNew (hlpExtractCol arrData 1)) ; colIndex is 0-based
;; ================================================================================
;; ================================================================================
(defun hlpExtractCol (arr2D colIndex / arrResult row)
  (setq arrResult '()) ; initialize result list
  (foreach row arr2D
    (setq arrResult (append arrResult (list (nth colIndex row))))
  )
  arrResult
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to filter out non-unique values from a list / array
;; sample use:
;; (setq arrLayers (hlpUniqueVals arrLayers))
;; ================================================================================
;; ================================================================================
(defun hlpUniqueVals (arr / item arrUnique)
  (setq arrUnique '())
  (foreach item arr
    (if (not (member item arrUnique))
      (setq arrUnique (append arrUnique (list item)))
    )
  )
  arrUnique
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; Helper Function to filter out empty values from a list / array
;; intent is to use array as space holders
;;
;; sample use:
;; (setq arrBlank3x3 (hlpBlankArr 3 3))
;; Result: (("" "" "") ("" "" "") ("" "" ""))
;;
;; ================================================================================
;; ================================================================================
(defun hlpRemEmpty (arr / item arrNoEmpty)
  (setq arrNoEmpty '()) ; initialize as empty list
  (foreach item arr
    (if (/= item "") ; skip empty strings
      (setq arrNoEmpty (append arrNoEmpty (list item)))
    )
  )
  arrNoEmpty ; return cleaned array
)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; Helper Function that will create a blank array
;; Blank arrays can then be inserted into other arrays to change the size/dimensions.
;; sample use:
;; (setq arrClean (hlpRemEmpty arrStuffed))
;; ================================================================================
;; ================================================================================
(defun hlpBlankArr (r c / row arr i)
  ;; Create a single row with c blank entries
  (setq row (repeat c (setq arr (cons "" arr))) arr (reverse arr))
  
  ;; Create r copies of that row
  (setq arr '())
  (repeat r
    (setq arr (cons row arr))
  )
  (reverse arr)
)


;; ================================================================================
;; ================================================================================
;; Helper Function to Append one array (1D or 2D) onto the end of another array (1D or 2D)
;; sample use:
;; (setq arrNew (hlpAppendCol arrBig arrLittle))
;; arrBig → a 2D array (e.g., 6×4)
;; arrLittle → a 1D array (e.g., 6×1)
;; ================================================================================
;; ================================================================================
(defun hlpAppendCol (arr2D arr1D / arrNew rowA rowB isA2D isB2D)
  ;; Convert to 2D if either input is 1D
  (if (not (listp (car arr2D)))
    (setq arr2D (To2D arr2D))
  )
  (if (not (listp (car arr1D)))
    (setq arr1D (To2D arr1D))
  )

  ;; Dimension check
  (if (/= (length arr2D) (length arr1D))
    (progn
      (prompt "\nError: Appended Columns must have the same number of rows.")
      nil
    )
    (progn
      (setq arrNew '())
      (while (and arr2D arr1D)
        (setq rowA (car arr2D))
        (setq rowB (car arr1D))
        (setq arrNew (append arrNew (list (append rowA rowB))))
        (setq arr2D (cdr arr2D))
        (setq arr1D (cdr arr1D))
      )
      arrNew
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to STACK / append a 2D array below another 2D array
;;
;; supposedly, LISP allows jagged arrays
;; ================================================================================
;; ================================================================================
(defun hlpAppendRow (arrTop arrBottom / arrNew isTop2D isBot2D)
  ;; Ensure both inputs are 2D arrays
  (if (not (listp (car arrTop)))
    (setq arrTop (To2D arrTop))
  )
  (if (not (listp (car arrBottom)))
    (setq arrBottom (To2D arrBottom))
  )

  ;; Append arrBottom rows below arrTop
  (setq arrNew (append arrTop arrBottom))

  arrNew
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function definition to insert the columns of one Array
;; into / in between the columns of another array.
;; arrA is inserted into arrB
;; after columnIndex of arrB
;; sample use:
;; 			(setq arrObjProp (hlpInsertCol arrObjProp arrObjDens 2))
;; ================================================================================
;; ================================================================================
(defun nthcdr (n lst)
  (if (or (null lst) (<= n 0))
    lst
    (nthcdr (1- n) (cdr lst))
  )
)

;; Helper: range of integers from start to end inclusive
(defun range (start end / result)
  (if (> start end)
    nil
    (cons start (range (1+ start) end))
  )
)

(defun hlpInsertCol (arrA arrB indexB / rA cA rB cB arrNew rowA rowB left right rowNew i)

  ;; Coerce arrA if it's a scalar (atom) or a 1D list
  (cond
    ((atom arrA)                       ; atom → ((arrA))
     (setq arrA (list (list arrA))))
    
    ((and (listp arrA)
          (not (listp (car arrA))))    ; 1D list → ((val))
     (setq arrA (list arrA)))
  )

  ;; Get array dimensions
  (setq rA (length arrA))
  (setq rB (length arrB))

  ;; Assume rectangular data
  (setq cA (length (car arrA)))
  (setq cB (length (car arrB)))

  ;; Error if row counts don't match
  (if (/= rA rB)
    (progn
      (prompt "\nError: Inserted Columns must have the same number of rows.")
      nil
    )
    
    ;; Build new array
    (progn
      (setq arrNew '())
      (setq i 0)
      (repeat rB
        (setq rowA (nth i arrA))
        (setq rowB (nth i arrB))

        ;; Split rowB into left and right parts
        (setq left  (mapcar '(lambda (j) (nth j rowB)) (range 0 indexB)))
        (setq right (nthcdr (1+ indexB) rowB))

        ;; Build new row and append
        (setq rowNew (append left rowA right))
        (setq arrNew (append arrNew (list rowNew)))

        (setq i (1+ i))
      )
      arrNew
    )
  )
)

;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function definition to multiply two columns from arrays together
;; sample use:
;; (setq arrProduct (arrA 2 arrB 3))
;; ================================================================================
;; ================================================================================
(defun hlpMultCols (arrA colA arrB colB / arrProduct rowA rowB valA valB)
  ;; Coerce to 2D arrays
  (if (not (listp (car arrA))) (setq arrA (To2D arrA)))
  (if (not (listp (car arrB))) (setq arrB (To2D arrB)))

  ;; Dimension check
  (if (/= (length arrA) (length arrB))
    (progn
      (prompt "\nError: Multiplied Columns must have the same number of rows.")
      nil
    )
    (progn
      (setq arrProduct '())
      (while (and arrA arrB)
        (setq rowA (car arrA))
        (setq rowB (car arrB))

        (setq valA (nth colA rowA))
        (setq valB (nth colB rowB))

        ;; convert strings if necessary
        (setq valA (if (numberp valA) valA (read valA)))
        (setq valB (if (numberp valB) valB (read valB)))

        (setq arrProduct (append arrProduct (list (* valA valB))))

        (setq arrA (cdr arrA))
        (setq arrB (cdr arrB))
      )
      arrProduct
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function definition to sum columns of an array
;; sample use:
;; (setq totalMass (hlpSumCol arrWeight 0))
;; ================================================================================
;; ================================================================================
(defun hlpSumCol (arr colIndex / total row val)
  ;; If it's a 1D list, wrap it
  (if (not (listp (car arr)))
    (setq arr (To2D arr))
  )

  (setq total 0.0)
  (foreach row arr
    (setq val (nth colIndex row))
    (if (numberp val)
      (setq total (+ total val))
      (setq total (+ total (read val))) ; just in case
    )
  )
  total
)

;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; Helper Function to Make sure layer exists
;; sample use:
;; (hlpLayExists "CGsphere")
;; ================================================================================
;; ================================================================================
(defun hlpLayExists (layerName / layObj)
  (if (not (tblsearch "LAYER" layerName))
    (progn
      (setq layObj
        (vla-add
          (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
          layerName
        )
      )
      (vla-put-Color layObj 1) ; 1 = red
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to draw a sphere
;; sample use:
;; (hlpDrawSphere centX centY centZ 12 "CGsphere")
;; ================================================================================
;; ================================================================================
(defun hlpDrawSphere (cenX cenY cenZ radius layerName / acadDoc modelSpace sphere ptCenter)

	(setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
	(setq modelSpace (vla-get-ModelSpace acadDoc))

	;; Create center point
	(setq ptCenter (vlax-3d-point (list cenX cenY cenZ)))

	;; Create the sphere
	(setq sphere (vla-AddSphere modelSpace ptCenter radius))

	;; Set layer
	(vla-put-Layer sphere layerName)
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to draw a circle & cross hair (benchmark)
;; sample use:
;; (hlpDraw2Dmark centX centY radius layerName)
;; ================================================================================
;; ================================================================================
(defun hlpDrawCG2d (cenX cenY radius layerName / acadDoc modelSpace ptCenter pt1 pt2 pt3 pt4 circ lineH lineV)

	(setq acadDoc    (vla-get-ActiveDocument (vlax-get-acad-object)))
	(setq modelSpace (vla-get-ModelSpace acadDoc))

	;; center point
	(setq ptCenter (vlax-3d-point (list cenX cenY 0.0)))

	;; circle
	(setq circ (vla-AddCircle modelSpace ptCenter radius))
	(vla-put-Layer circ layerName)

	;; horizontal crosshair
	(setq pt1 (vlax-3d-point (list (- cenX radius) cenY 0.0)))
	(setq pt2 (vlax-3d-point (list (+ cenX radius) cenY 0.0)))
	(setq lineH (vla-AddLine modelSpace pt1 pt2))
	(vla-put-Layer lineH layerName)

	;; vertical crosshair
	(setq pt3 (vlax-3d-point (list cenX (- cenY radius) 0.0)))
	(setq pt4 (vlax-3d-point (list cenX (+ cenY radius) 0.0)))
	(setq lineV (vla-AddLine modelSpace pt3 pt4))
	(vla-put-Layer lineV layerName)

	(princ)
)




;; ================================================================================
;; ================================================================================
;; Helper Function definition to check the format of user input
;; ================================================================================
;; ================================================================================
(defun hlpFormatNum (arr / result)
	(setq result '())

	(foreach item arr
		(cond
			;; If it's already a number (int or real), keep it
			((or (= (type item) 'INT) (= (type item) 'REAL))
				(setq result (append result (list item)))
			)

			;; IF ITEM IS A STRING
			((= (type item) 'STR)
			 (cond
			   ;; see if it can be read ast a number
			   ((numberp (read item))
				(setq result (append result (list (read item))))
			   )
			   ;; if can't read string, see if it's a fraction
			   ((hlpParseFracStr item)
				(setq result (append result (list (hlpParseFracStr item))))
			   )
			   (T
				(prompt (strcat "\n[DEBUGFORMAT] Could not convert string: " item))
				(setq result (append result (list "STR")))
			   )
			 )
			)

      ;; If it's a symbol, try to convert from its name
      ((= (type item) 'SYM)
       (if (numberp (read (symbol-name item)))
         (setq result (append result (list (read (symbol-name item)))))
         (progn
           ;; conversion failed
           (prompt (strcat "\n[DEBUG] Could not convert symbol: " (symbol-name item)))
           (setq result (append result (list "SYM")))
         )
       )
      )

      ;; Fallback for unknown types
      (T
       (prompt "\n[DEBUGFORMAT] Unknown type encountered.")
       (setq result (append result (list 0)))
      )
    )
  )

  result
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Helper Function to parse a fraction stored in a string
;; into a REAL number (..."3/4" --> 0.750)
;; ================================================================================
;; ================================================================================
(defun hlpParseFracStr (s / idx num den)
	;; Make sure s is a string before continuing
	(if (/= (type s) 'STR)
		nil ; exit early — not a string
	(progn
		;; Now we know s is a string, check if it's a valid fraction
		(if (and (> (strlen s) 2) (/= (vl-string-search "/" s) nil))
			(progn
				(setq idx (vl-string-search "/" s))
				(setq num (substr s 1 idx))
				(setq den (substr s (+ idx 2)))

			(if (and (numberp (read num)) (numberp (read den)))
				;; LISP does "integer devision", 3/4 = 1
				;; convert integers to floating point numbers
				(/ (float (read num)) (read den))
				nil ; failed to convert numerator or denominator
			)
        )
        nil ; no "/" found or string too short
      )
    )
  )
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; Popup  -  Lee Mac
;; A wrapper for the WSH popup method to display a message box prompting the user.
;; ttl - [str] Text to be displayed in the pop-up title bar
;; msg - [str] Text content of the message box
;; bit - [int] Bit-coded integer indicating icon & button appearance
;; Returns: [int] Integer indicating the button pressed to exit
;;
;; BUTTONS
;; 0	Display OK button
;; 1	Display OK and Cancel buttons
;; 2	Display Abort, Retry, and Ignore buttons.
;; 3	Display Yes, No, and Cancel buttons.
;; 4	Display Yes and No buttons.
;; 5	Display Retry and Cancel buttons.
;; 6	Display Cancel, Try Again, and Continue buttons.
;; 
;; RETURN VALUES
;; 1	OK button
;; 2	Cancel button
;; 3	Abort button
;; 4	Retry button
;; 5	Ignore button
;; 6	Yes button
;; 7	No button
;; 10	Try Again button
;; 11	Continue button
;;
;; http://lee-mac.com/popup.html
;; see webpage for bit-codes and return definitions
;;
;; Example Function Call
;; (LM:popup "Title Text" "This is a test message." (+ 2 48 4096))
;; ================================================================================
;; ================================================================================
(defun LM:popup ( ttl msg bit / wsh rtn )
    (if (setq wsh (vlax-create-object "wscript.shell"))
        (progn
            (setq rtn (vl-catch-all-apply 'vlax-invoke-method (list wsh 'popup msg 0 ttl bit)))
            (vlax-release-object wsh)
            (if (not (vl-catch-all-error-p rtn)) rtn)
        )
    )
)
;; ================================================================================
;; ================================================================================
