;;#################################################################################
;;#################################################################################
;;## 
;;## CG3D (DEFINED AT THE BOTTOM OF THIS FILE)
;;## is the main function to determin the Center of Gravity of one or more
;;## AutoCAD solid elements.
;;## 
;;## This file is reliant on a seperate file of helper routeines
;;## 
;;## Thrown together haphazardly by AKH
;;## VERSION / DATE: MAY 2026
;;#################################################################################
;;#################################################################################
;;
;; ================================================================================
;; VL Load
;; ================================================================================
;; This gives you access to symbolp, vl-princ-to-string, and other VL functions in any LISP environment.
;; it is loaded as soon as appload or startup script opens this LSP file
;; autocad still couldn't find the "vl-stringp" command for some reason

(vl-load-com)

;; ================================================================================
;; ================================================================================


;; ================================================================================
;; Load HELPER FILE
;; REQUIRED HELPER function file
;; ================================================================================

(load "CG_helpers.lsp")

;; ================================================================================
;; ================================================================================

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
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;;
;; subroutine to build array of selected element properties
;;
;; Loop through selected 3D solids, extract properties, and save them to an array
;; Each element in the selection set becomes its own row,
;; and each row contains values for 7 columns as noted
;; ================================================================================
;; ================================================================================
(defun subGet3dProps (ss / ent solAcad arr)

	;; create empty array to be filled in
	(setq arr '())
		
	(while (> (sslength ss) 0)
		(setq ent (ssname ss 0))
		(setq solAcad (vlax-ename->vla-object ent))

		;; Add this object's properties to the array
		(setq arr
			(append arr
				(list (list
					(vlax-get solAcad 'ObjectID)
					(vlax-get solAcad 'ObjectName)
					(vlax-get solAcad 'Layer)
					(vlax-get solAcad 'Volume)
					(car   (vlax-get solAcad 'Centroid))
					(cadr  (vlax-get solAcad 'Centroid))
					(caddr (vlax-get solAcad 'Centroid))
					)
				)
			)
		)

		;; Remove that entity from the selection set
		(setq ss (ssdel ent setSolids))
	)
	
	arr ;; return the 2D array of properties to the main routine

)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;;
;; Function to call up a dialog box definition.
;; calls a seperate dialog box file (*.DCL)
;;
;; This defun declaration defines local variables that won't go outside of this function...NO GOOD
;; (defun inputMaterial ( / dcl_id mat1 mat2 mat3 mat4 mat5 mat6 result)
;; Remove those declarations to allow global variable definitions elsewhere...
;; ================================================================================
;; ================================================================================
(defun subMatDialog ( / dcl_id result)

	;; Load DCL
	;; DCL should be saved in the same folder as this LISP
	;; May need to hardcode the folder path when the DCL is not in an autocad support folder
	;; double-backslashes in filepath strings

	(setq dcl_id
		(load_dialog (findfile "CG_dialogs.dcl"))
	)	
	(if (not (new_dialog "CG3d_MatDlg" dcl_id))
		(progn
			(alert "DCL failed to load.")
			(exit)
		)
	)
	
	(if arrLayers
		(progn
			;; disable (1) input all boxes 
			(mode_tile "mat1" 1)
			(mode_tile "mat2" 1)
			(mode_tile "mat3" 1)
			(mode_tile "mat4" 1)
			(mode_tile "mat5" 1)
			(mode_tile "mat6" 1)
			
			;; enable (change mode_tile to 0) input boxes IF material / layer exists
			(if (nth 0 arrLayers) (set_tile "label1" (nth 0 arrLayers)) )
			(if (nth 0 arrLayers) (mode_tile "mat1" 0) )
			(if (nth 1 arrLayers) (set_tile "label2" (nth 1 arrLayers)) )
			(if (nth 1 arrLayers) (mode_tile "mat2" 0) )
			(if (nth 2 arrLayers) (set_tile "label3" (nth 2 arrLayers)) )
			(if (nth 2 arrLayers) (mode_tile "mat3" 0) )
			(if (nth 3 arrLayers) (set_tile "label4" (nth 3 arrLayers)) )
			(if (nth 3 arrLayers) (mode_tile "mat4" 0) )
			(if (nth 4 arrLayers) (set_tile "label5" (nth 4 arrLayers)) )
			(if (nth 4 arrLayers) (mode_tile "mat5" 0) )
			(if (nth 5 arrLayers) (set_tile "label6" (nth 5 arrLayers)) )
			(if (nth 5 arrLayers) (mode_tile "mat6" 0) )
		)
	)

	;; Accept button
	;; set global variables, so they're sent to the calling method/routine
	;; UPDATE this list if the dialog ever changes
	(setq arrMatKeys '("mat1" "mat2" "mat3" "mat4" "mat5" "mat6" "SphLay" "SphDiam"))
	
	;; this is generating a string to send to the DCL engine, which will be turned INTO
	;; an action when the accept button is clicked. And the arrOut array is created.
	(action_tile "accept"
		(strcat
			"(progn "
				"(setq arrOut (mapcar 'get_tile arrMatKeys)) "
				"(done_dialog 1))"
		)
	)

	;; Cancel button
	(action_tile "cancel" "(done_dialog 0)")

	;; Start the dialog
	(setq result (start_dialog))
	(unload_dialog dcl_id)

	;; remove empty values from the array
	(setq arrOut (hlpRemEmpty arrOut))

	;; assign last two elements of arrOut to sphere variables.
	;; and delete the last two rows/elements from the array
	;; (reverse arrOut) → reverses the list so the last two elements are now at the front.
	;; (cdr (cdr ...)) → removes the first two elements of the reversed list (i.e., the original last two).
	;; reverse again to restore original order minus the last two items.	
	(setq i (length arrOut)); i = 4
	(setq strCGLayer (nth (- i 2) arrOut))    ; (nth 2 arrOut) = "CGsphere" (a layer name)
	(setq diaSphere  (nth (- i 1) arrOut))    ; (nth 3 arrOut) = "4" (a number)
	(setq arrOut (reverse (cdr (cdr (reverse arrOut)))))
	
	;; make sure all of the input values are valid
	;; this is sent back to the calling routine
	(setq arrOut (hlpFormatNum arrOut))
	
	;; Bundle multiple values to be returned to calling routine
	(list arrOut strCGLayer diaSphere)

)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;; this creates a new 1D list/array of material desity
;; that matches up to the selected objects (array) 
;; ================================================================================
;; ================================================================================
(defun subObjDensity (arrP hdr arrD / arrOut rowObj i objLayer objDens arrScan)

	;; create empty array...will be 1D array of density values
	(setq arrOut '())
	
	;; check which column "Layer" is in
	(setq i (hlpIndexOfMatch hdr "*Layer*"))
	
	;; Loop Through each object row
	(foreach rowObj arrP
		
		(setq objLayer (nth i rowObj))
		(setq objDens "") ;; default if not found
		
		;; nested loop through arrD to find match
		;; save to local arrScan array for scratch work
		(setq arrScan arrD)
		
		(while arrScan
			;; caar operator shouild get the first element of a list (array?)
			(if (= objLayer (caar arrScan))  ;; match layer name
				(progn
					(setq objDens (cadar arrScan)) ;; get matching density
					(setq arrScan nil) ; this breaks the loop
				)
				(setq arrScan (cdr arrScan))
			)
		)
		;; append the new row with density
		(setq arrOut (append arrOut (list (list objDens))))
	)
	
	;; send object dnsity array back to main routine
	arrOut

)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; MATH SUBROUTINE
;; called by the main function / routine (below)
;;
;; (setq arrCG (sub3dMath hdrObjProp hdrPropAdd arrObjProp))
;; 
;; ================================================================================
;; ================================================================================
(defun sub3dMath (arrDwg hdrP hdrPA arr / i j k arrWeight
							arrWX arrWY arrWZ totWt
							totWX totWY totWZ
                            hdrTotals arrTot
                            centX centY centZ
                            hdrCent arrCent arrOut)
  
	(setq i (hlpIndexOfMatch hdrP "*DENS*"))
	(setq j (hlpIndexOfMatch hdrP "*VOL*"))
  
	(setq arrV (hlpExtractCol arr j))
	(setq arrWeight (hlpMultCols arr i arr j))
  
	(setq k (hlpIndexOfMatch hdrP "*Cent*X*"))
	(setq arrWX (hlpMultCols arrWeight 0 arr k))
  
  	(setq k (hlpIndexOfMatch hdrP "*Cent*Y*"))
	(setq arrWY (hlpMultCols arrWeight 0 arr k))
  
	(setq k (hlpIndexOfMatch hdrP "*Cent*Z*"))
	(setq arrWZ (hlpMultCols arrWeight 0 arr k))
  
	(setq arr (hlpAppendCol arr arrWeight))
	(setq arr (hlpAppendCol arr arrWX))
	(setq arr (hlpAppendCol arr arrWY))
	(setq arr (hlpAppendCol arr arrWZ))
  
	(setq totV (hlpSumCol arrV 0)) ;; NEED TO INSERT 3 BLANK COLUMNS AFTER VOLUME
	(setq totWt (hlpSumCol arrWeight 0))
	(setq totWX (hlpSumCol arrWX 0))
	(setq totWY (hlpSumCol arrWY 0))
	(setq totWZ (hlpSumCol arrWZ 0))
  
	;; column headers for the totals
	(setq titlTot "SUM TOTALS")
	(setq hdrTotals (hlpTranspose(To2D (list "Tot. Vol." "Tot. Weight" "Tot. WX" "Tot. WY" "Tot. WZ"))))
	(setq arrTot (hlpTranspose(To2D (list totV totWt totWX totWY totWZ))))
	
	(setq centX (/ totWX totWt))
	(setq centY (/ totWY totWt))
	(setq centZ (/ totWZ totWt))
  
	;; column headers for the centroid coordinates
	(setq titlCent "CG COORDINATES")
	(setq hdrCent (To2D (list "Centroid X" "Centroid Y" "Centroid Z")))
	(setq hdrCent (hlpTranspose hdrCent))
	(setq arrCent (To2D (list centX centY centZ)))
	(setq arrCent (hlpTranspose arrCent))
  
	;; titlTot is just a STRING
	;; wrap it in a list to turn it into a 2D array that can be appended to other arrays
	(setq TitlTot (To2D (list titlTot)))
	(setq titlCent (To2D (list titlCent)))
  
	;; NEED TO APPEND/INSERT hdrPA after hdrA.
	(setq hdrPA (hlpTranspose (To2D hdrPA))) ;; make the additional header list into an array and hlpTranspose it
	(setq hdrP (hlpAppendCol hdrP hdrPA))  ; insert hdrPA at the end
  
	;; STACK / append Total and Center rows for later insertion into arrOut
	(setq arrTot (hlpAppendRow hdrTotals arrTot))
	(setq arrCent (hlpAppendRow hdrCent arrCent))
	
	;; Define blank arrays to cushion Totals and Centers
	(setq Blank2x4 (hlpBlankArr 2 4))
	(setq Spacer2x3 (hlpBlankArr 2 3))
	(setq Blank2x9 (hlpBlankArr 2 9))
  
	(setq arrTot (hlpInsertCol Spacer2x3 arrTot  0)) ;; after colIndex 0
	(setq arrTot (hlpAppendCol Blank2x4 arrTot))
	(setq arrCent (hlpAppendCol Blank2x9 arrCent))
	
	(setq arrOut arrDwg) ;; insert drawing info into arrOut
  
	;; STACK / append arrays to be output to CSV
	(setq arrOut (hlpAppendRow arrOut hdrP))
	(setq arrOut (hlpAppendRow arrOut arr))
	(setq arrOut (hlpAppendRow arrOut titlTot))	
	(setq arrOut (hlpAppendRow arrOut arrTot))
	(setq arrOut (hlpAppendRow arrOut titlCent))
	(setq arrOut (hlpAppendRow arrOut arrCent))
  
	;; send output array back to main ROUTINE
	arrOut
  
)
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;; This gets a list of the unique layers in the selection set
;; Unique layers names are then sent to the dialog box as labels
;; 
;; ================================================================================
;; ================================================================================
(defun subGet3dLay (num arr hdr)
	;; Count number of 3D solids in selection
	;; and display a popup message with the 
	;; total number of objects selected and the number of solid objects selected
	(setq j 0 i 0)
	(while (< i num)
		(if (equal (cadr (nth i arr)) "AcDb3dSolid")
		(setq j (1+ j)))
		(setq i (1+ i))
	)

	(setq strMsg (strcat "Total Objects Selected: " (itoa num)
					"\n\nNumber of 3D Solid Objects: " (itoa j)
					"\n\nDo you wish to continue?"
				)
	)	

	(setq intResult (LM:popup "Confirm Selection" strMsg (+ 1 32 4096))) ; OK-Cancel, Question Mark
	(if (= intResult 1)
		(progn
			;; OK was pressed - continue with rest of routine
			(princ "\nUser pressed OK. Continuing...")
			;; your code here / continue with program
		)
		;;ELSE
		(progn
			;; Cancel was pressed - exit the function
			(princ "\nUser pressed Cancel. Aborting.")
			(exit) ; or (quit) if you want to fully bail out
		)
	)
		
	;; Extract (unique) layers from arrObjProp and eliminate non-unique layer names
	(setq i (hlpIndexOfMatch hdr "*Layer*"))
	
	(setq arrOut (hlpExtractCol arr i)) ; colIndex is 0-based
	(setq arrOut (hlpUniqueVals arrOut))			
	
	;; send arrOut back to calling routine
	arrOut
)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;; This gets the general material density from user input
;; 
;; ================================================================================
;; ================================================================================
(defun subGetDens (arrL arrInput)

	;; Check if user clicked Cancel (returns nil) on the dialog box
	(if arrInput
		(progn					
			;; Use arrInput values here
			;; (e.g., (nth 0 arrInput) = first input box value)
			(princ "\nUser dialog box input collected successfully.")
		)
		;;ELSE
		(progn
				(princ "\nUser canceled input dialog box. Aborting.")
				(exit) ; or (quit) or just skip the rest of the function
		)
	)
	
	;; get the dialog box values for the material densities
	;; append the layer name and the layer density into a single 2D array
	;; arrD is created via the dialog box and subroutine
	(setq arrOut (hlpAppendCol arrL arrInput))

	;;send data to a text CSV file
	;;(setq strFile (ArrayToTxt arrOut)) ; [DEBUG Write Array]

	;; return density array to main routine
	arrOut	
)
;; ================================================================================
;; ================================================================================


;;#################################################################################
;;#################################################################################
;;
;; THIS IS THE MAIN ROUTINE THAT CALLS THE ABOVE SUBROUTINES 
;; 
;; AND THE HELPER FUNCTIONS STORED IN A SEPERATE HELPER FILE
;;
;;#################################################################################
;;#################################################################################
(defun c:CG3D ()
  
	;; Clear any existing selection set
  	(if (ssget "_P") (setq setSolids nil))
	
	(prompt "\n[DEBUG_POINT] 1 reached")
  
	;; Capture current date and time
  	(setq dateNow (rtos (getvar "CDATE") 2 6))
  
	;; Get current drawing name
	(setq strDwgName (getvar "DWGNAME"))

	;; Convert the unit system number to a string
	(setq strUnitSys (hlpUnitName))
  
	;; --------------------------------------------------------------------------------
	;; can consolidate these notices with LeeMac messages or something
	;; uncomment when all other debugging is completed
	;; --------------------------------------------------------------------------------
	;; Display drawing info in pop up message box
	;;(alert (strcat "Date: " dateNow "\nDwg. Name: " strDwgName "\nUnits: " strUnitSys))
	;; Prompt user to select objects in pop up message box
	;;(alert "Select Objects! Hit Enter to Finish!")
	(setq strMsg (strcat "Dwg. Name: " strDwgName
					"\n\nDate: " dateNow
					"\n\nDwg. Units: " strUnitSys
					"\n\nSelect Solid Objects."
					"\nPress ENTER when selection complete.")
	)
	
	(setq arrDwg (list
               (list "Dwg. Name:" strDwgName)
               (list "Date:"      dateNow)
               (list "Units:"     strUnitSys)
             ))

	
	;; general notification message to let user know calculator is starting
	;; allow to OK or CANCEL
	(setq intResult (LM:popup "Center of Gravity Calculator" strMsg (+ 1 32 4096))) ; OK-Cancel, Question Mark
	(if (= intResult 1)
		(progn
			;; OK was pressed - continue with rest of routine
		)
		;;ELSE
		(progn
			;; Cancel was pressed - exit the function
			(princ "\nUser pressed Cancel. Aborting.")
			(exit) ; or (quit) if you want to fully bail out
		)
	)
	
	(prompt "\n[DEBUG_POINT] 2 reached")
	
	;; request the user to make a selection
	;; (setq setSolids (ssget))
	(setq setSolids (ssget '((0 . "3DSOLID"))))

  
	;; --------------------------------------------------------------------------------
	;; Main "IF" block that will call and process subroutines
	;; --------------------------------------------------------------------------------
	(if setSolids
		
   		;; --------------------------------------------------------------------------------
		;; TRUE condition fo IF statement
		;; --------------------------------------------------------------------------------
   		(progn

			(prompt "\n[DEBUG_POINT] 3 reached")

			(setq numObj (sslength setSolids))
			
			;;(princ (strcat "\nNumber of 3D Solids selected: " (itoa numObj)))

			;; Define a header row for later of object properties for later use
			;; these should match the append of arrObjProp, below.
			(setq hdrObjProp (list "ObjectID" "ObjectName" "Layer" "Volume" "Centroid X" "Centroid Y" "Centroid Z"))
			
			;; additional column headers to append to the existing properties array(s)
			(setq hdrPropAdd (list "Weight" "W * X" "W * Y" "W * Z"))
	
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to create array of object properties ;;
			;; create arrObjProp                                    ;;
			;; (setq variable (subroutine argument1 argument2))
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq arrObjProp (subGet3dProps setSolids))

			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to create an array of Layers        ;;
			;; of selected solids                                  ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq arrLayers (subGet3dLay numObj arrObjProp hdrObjProp))
			
			(prompt "\n[DEBUG_POINT] 4 reached")
			
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call the input dialog box via a subroutine   ;;
			;; and then unpack the multiple returned values ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;(setq arrUserInput (subMatDialog))
			(progn
				(setq tmpReturn (subMatDialog))
				(setq arrUserInput (nth 0 tmpReturn)
						strCGLayer (nth 1 tmpReturn)
						diaSphere  (nth 2 tmpReturn))
			)
			
			;;(prompt (strcat "\n[DEBUG] strCGLayer = " (vl-prin1-to-string strCGLayer)))
			;;(prompt (strcat "\n[DEBUG] diaSphere = " (vl-prin1-to-string diaSphere)))
			
			(prompt "\n[DEBUG_POINT] 5 reached")
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to create an array of user Input    ;;
			;; material densities                                  ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq arrDensity (subGetDens arrLayers arrUserInput))
			
			(prompt "\n[DEBUG_POINT] 6 reached")
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to match element layers & densities ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			
			(setq arrObjDens (subObjDensity arrObjProp hdrObjProp arrDensity))
			
			;; Header of Object Properties is still a 1D list at this point, convert to array
			(setq hdrObjProp (To2D hdrObjProp))
	
			;; Header of Objects is not a 2D array with (multiple) rows x 1 column
			;; need to hlpTranspose those into 1 row x (multiple) column
			(setq hdrObjProp (hlpTranspose hdrObjProp))
	
			;; insert object densities array (column) after the "Layer" column
			(setq i (hlpIndexOfMatch hdrObjProp "*Layer*"))
			(setq j (To2D (list "Density")))

			(setq hdrObjProp (hlpInsertCol j hdrObjProp i)) ;; insert j after hdr
			(setq arrObjProp (hlpInsertCol arrObjDens arrObjProp  2)) ;; insert arrObDens after the 2nd col index, 3rd column
	
			(prompt "\n[DEBUG_POINT] 7 reached")
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to do math stuff ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq arrCG (sub3dMath arrDwg hdrObjProp hdrPropAdd arrObjProp))

			(LM:Popup "Object Property Array Contents" (vl-prin1-to-string arrCG) (+ 1 32 4096)) ; OK-Cancel, Question Mark
			
			;; Get last row of arrCG
			;; Extract X, Y, Z from that row
			(setq arrCent (last arrCG))
			(setq centX (nth 9 arrCent))  ; column 10
			(setq centY (nth 10 arrCent)) ; column 11
			(setq centZ (nth 11 arrCent)) ; column 12
			
			(prompt "\n[DEBUG_POINT] 8 reached")
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to hlpDrawSphere    ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(hlpLayExists strCGLayer)
			
			(hlpDrawSphere centX centY centZ diaSphere strCGLayer)
	
			;; LEAVE THIS HERE...this is the final "production" output
			;;send data to a text CSV file
			(setq strFile (hlpArrayToTxt arrCG))
			
			(prompt "\n[DEBUG_POINT] 9 reached")
			
		)
      	;; ----- END OF PROGRN -------------------------------------------------------------
   
   		;; --------------------------------------------------------------------------------
		;; FALSE/ELSE condition fo IF statement
		;; --------------------------------------------------------------------------------
		(progn
		
			(alert "No 3D Solids selected.
				\nRoute required as least one solid to be selected.
				\nRoutine will now be aborted."
			)
	   
			(prompt "\n[DEBUG_POINT] CGz reached")
		
		)
	
	)
	;; --------------------------------------------------------------------------------
	;; END OF Main "IF" block started above
	;; --------------------------------------------------------------------------------
	
  (princ)
)
;; ================================================================================
;; ================================================================================

