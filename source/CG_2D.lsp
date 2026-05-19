;;#################################################################################
;;#################################################################################
;;## 
;;## CG2D (DEFINED AT THE BOTTOM OF THIS FILE)
;;## is the main function to determine the general section properties
;;## of one or more 2D, closed AutoCAD elements (regions) that works
;;## similarly to the native MASSPROPS command.
;;## 
;;## 
;;## Thrown together haphazardly by AKH
;;## VERSION / DATE: MAY 2026
;;#################################################################################
;;#################################################################################
;;
;; ================================================================================
;; VL Load
;; ================================================================================
;; This gives you access to symbolp, vl-princ-to-string, and other VL functions
;; in any LISP environment. It is loaded as soon as appload or startup script
;; opens this LSP file

(vl-load-com)

;; ================================================================================
;; ================================================================================


;; ================================================================================
;; Load HELPER FILE
;; REQUIRED REFERENCE FILE
;; ================================================================================

(load "CG_helpers.lsp")

;; (princ "\n[LOAD] Current CG2Dw.lsp loaded - 2026-05-18")

;; ================================================================================
;; ================================================================================

;; ================================================================================
;; DEBUGGING CODE SYNTAX FOR LATER REFERENCE
;; ================================================================================
;; (prompt "\nC[DEBUG_POINT] A reached.") ;; DEBUGGING CHECKPOINT
;; (print hdrPropObj)  ;; DEBUGGING CHECKPOINT
;; (TypeSize hdrPropObj "hdrPropObj") ;; DEBUGGING
;; (prompt (strcat "\nVariable i = " (vl-prin1-to-string i))) ;; DEBUGGING
;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;;
;; Filter out the user selection set to check if all objects are region-able
;; ================================================================================
;; ================================================================================

(defun subClosedPoly (ent / entData intFlag)
	(setq entData (entget ent))
	(setq intFlag (cdr (assoc 70 entData)))
	(= 1 (logand 1 intFlag))
)

(defun subFilterRegions (setIn / setOut ent objType objAcad)
	(setq setOut (ssadd))

	(while (> (sslength setIn) 0)
		(setq ent (ssname setIn 0))
		(setq objType (cdr (assoc 0 (entget ent))))
		(setq objAcad (vlax-ename->vla-object ent))

		(cond

			;; --------------------
			;; already a region
			;; --------------------
			(
			(= objType "REGION")
			(ssadd ent setOut)
			)

			;; --------------------
			;; closed polyline candidate
			;; --------------------
			(
			(and
				(member objType '("LWPOLYLINE" "POLYLINE"))
				(subClosedPoly ent)
			)
			(ssadd ent setOut)
			)
			
			;; --------------------
			;; anything else = fail
			;; --------------------
			(T
				(alert
					(strcat
					"Selected object is not a REGION or closed polyline.\n\n"
					"Object type: " objType
					)
				)
				(setq setOut nil)
				(setq setIn nil)
			)
		)

		(if setIn
			(setq setIn (ssdel ent setIn))
		)
	)

  setOut
)
;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;;
;; Converts polylines to regions on a temporary layer
;; ================================================================================
;; ================================================================================

(defun subConvToReg (setIn / setOut i ent obj objType entTemp objTemp entPre entReg strTmpLay)

	(setq setOut (ssadd))
	
	;; Store current active layer
	(setq strLayerOld (getvar "CLAYER"))
	
	;; enter name of new temporary layer
	(setq strTmpLay "CG_TEMP_REGION")

	;; Make temp layer if needed
	(if (not (tblsearch "LAYER" strTmpLay))
		;; TRUE action
		(command
			"_.-LAYER"
			"_N" strTmpLay  ;; New Layer name
			;;"_M" strTmpLay ;; Make Layer current
			"_C" "6" strTmpLay ;; 6 = magenta
			"_LT" "HIDDEN" strTmpLay
			""
		)
		;; FALSE action
		;; -----
	)

	(setvar "CLAYER" strTmpLay)

	(setq i 0)  ;; set counter to 0

	(while (< i (sslength setIn))

		(setq ent (ssname setIn i))
		(setq obj (vlax-ename->vla-object ent))
		(setq objType (vla-get-ObjectName obj))

		(cond

			;; Already a region
			((= objType "AcDbRegion")
				(setq setOut (ssadd ent setOut))
			)

			;; Lightweight polyline
			((= objType "AcDbPolyline")

				;; Must be closed
				(if (/= (vla-get-Closed obj) :vlax-true)
					(progn
						(alert "Selected polyline is not closed. Routine aborted.")
						(exit)
					)
				)

				;; Copy polyline to temp layer
				(setq objTemp (vla-copy obj))
				(vla-put-Layer objTemp strTmpLay)
				(setq entTemp (vlax-vla-object->ename objTemp))

				;; Convert copied polyline to region
				(setq entReg entTemp)
				(vl-cmdf "_.REGION" entReg "")

				;; Make Sure entity is actually being assigned. Use "entlast" function.
				;; NOTE that the function call has to be wrapped in "()".
				(setq entReg (entlast))

				;; If region was created, place in selection set
				(if
					;; TEST condition
					(and entReg
						(/= entReg entTemp)
						(= (cdr (assoc 0 (entget entReg))) "REGION")
					)
					;; TRUE action
					(progn
						(setq setOut (ssadd entReg setOut))
					)
					;; FALSE action
					(progn
						(alert "Polyline could not be converted to a region. Routine aborted.")
						(exit)
					)
				)
			)

			;; Unsupported object
			(T
				(alert
					(strcat
					"Unsupported object selected:\n"
					objType
					"\n\nRoutine aborted."
					)
				)
				(exit)
			)
		) ;; end of conditiion expression

		(setq i (1+ i)) ;; move counter up 1
	
	)   ;; end of while loop
	
	;; Restore previous active layer
	(setvar "CLAYER" strLayerOld)

  ;; return selection set of regions (only).
  setOut
)

;; ================================================================================
;; ================================================================================


;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;;
;; subroutine to build array of selected element properties
;;
;; Loop through selected 2D solids, extract properties, and save them to an array
;; Each element in the selection set becomes its own row,
;; and each row contains values for 'X' columns as noted
;; ================================================================================
;; ================================================================================

(defun subGet2DProps (ss / ent objAcad arr)   ;; (defun FUNCNAME (inputs / local variables) 

	;; create empty array to be filled in
	(setq arr '())

	;;(prompt "\n[DEBUG_POINT] Start subGet2DProps Loop.")
	
	(princ (sslength ss))
		
	(while (> (sslength ss) 0)
		(setq ent (ssname ss 0))
		(setq objAcad (vlax-ename->vla-object ent))
		
		;; (princ "\n[DEBUG] ent = ")
		;; (princ ent)

		;; (princ "\n[DEBUG] ObjectID = ")
		;; (princ (vlax-get objAcad 'ObjectID))

		;; (princ "\n[DEBUG] Area = ")
		;; (princ (vlax-get objAcad 'Area))

		;; Add this object's properties to the array
		(setq arr
			(append arr
				(list (list
					;;
					;; this list assumes all REGIONS (no polylines, etc)
					;;
					
					;; debug check to see what properties are vlax-gettable
					;; (vlax-dump-object objAcad T)
					
					(vlax-get objAcad 'ObjectID)
					(vlax-get objAcad 'ObjectName)
					(vlax-get objAcad 'Layer)
					(vlax-get objAcad 'Area)
					(vlax-get objAcad 'Perimeter)   ; regions have a perimeter, polylines do not.
					;;
					;; car / cadr are extracting a double value property into a single value
					;;
					(car   (vlax-get objAcad 'Centroid))	;; X-axis of region
					(cadr  (vlax-get objAcad 'Centroid))   ;; Y-axis of region
					(car   (vlax-get objAcad 'MomentOfInertia))   ;; X-axis of region
					(cadr  (vlax-get objAcad 'MomentOfInertia))   ;; Y-axis of region
					(car   (vlax-get objAcad 'PrincipalDirections))   ;; X-axis of region
					(cadr  (vlax-get objAcad 'PrincipalDirections))   ;; Y-axis of region
					(car   (vlax-get objAcad 'PrincipalMoments))   ;; X-axis of region
					(cadr  (vlax-get objAcad 'PrincipalMoments))   ;; Y-axis of region
					(vlax-get objAcad 'ProductOfInertia)
					(car   (vlax-get objAcad 'RadiiOfGyration))   ;; X-axis of region
					(cadr  (vlax-get objAcad 'RadiiOfGyration))   ;; Y-axis of region
					)
				)
			)
		)

		;; Remove that entity from the selection set
		;; this variable name is set / called in the main routine
		(setq ss (ssdel ent ss))
		
	)
		
	;; debug print arr
	;; (prompt "\n[DEBUG] arr(subGet2DProps) = ")
	;; (princ arr)
	
	arr ;; return the 2D array of properties to the main routine

)
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;;
;; Define a header row of object properties for later use
;; these should match the "get properties" routine above.
;; ================================================================================
;; ================================================================================

(defun subObjHdr ( / )

	(setq lst (list
		"ID"
		"Name"
		"Layer"
		"Area"
		"Perimeter"
		"Cent X (global)"
		"Cent Y (global)"
		"Inert X (global)"
		"Inert Y (global)"
		"Prin Dir Vect X"
		"Prin Dir Vect Y"
		"Prin Mom X (prin)"
		"Prin Mom Y (prin)" 
		"Prod Inert (global)"
		"Rad Gyr X (global)"
		"Rad Gyr Y (global)"
		)
	)
	
	lst ;; return list to the main routine
	
)
	
;; ================================================================================
;; ================================================================================

;; ================================================================================
;; ================================================================================
;; SUBROUTINE OF THE MAIN ROUTINE
;;
;; Define a header row of additional object properties for later use
;; these should be the properties that will be calculated internally and appended
;; to the extracted properties.
;; ================================================================================
;; ================================================================================

(defun subAddlHdr ( / )

	;; calculate these values in the math subroutine

	(setq lst (list
		"Inert X (Ix, cg)"
		"Inert Y (Iy, cg)"
		"Prod Inert (Ixy, cg)"
		"Rad Gyr X (rx, cg)"
		"Rad Gyr Y (ry, cg)"
		"Polar Inert (J, cg)"
		)
	)

	lst ;; return list to the main routine

)

;; ================================================================================
;; ================================================================================



;; ================================================================================
;; ================================================================================
;; MATH SUBROUTINE
;; called by the main function / routine (below)
;;
;; subroute to calculate
;; "Inert X (Ix, cg)"
;; "Inert Y (Iy, cg)"
;; "Prod Inert (Ixy, cg)"
;; "Rad Gyr X (rx, cg)"
;; "Rad Gyr Y (ry, cg)"
;; "Polar Inert (J, cg)"
;;
;; and prepare the values to be appended to the object property array in the main routine
;; 
;; ================================================================================
;; ================================================================================
	
(defun subMath2D (hdrIn arrIn / arrOut row
                         idxA idxCxg idxCyg idxIxg idxIyg idxIxyg
                         A Cxg Cyg Ixg Iyg Ixyg
                         Ixcg Iycg Ixycg rxcg rycg Jcg)

	;; Find needed column indexes from header row
	(setq idxA    (hlpIndexOfMatch hdrIn "*Area*"))
	(setq idxCxg  (hlpIndexOfMatch hdrIn "*Cent*X*"))
	(setq idxCyg  (hlpIndexOfMatch hdrIn "*Cent*Y*"))
	(setq idxIxg  (hlpIndexOfMatch hdrIn "*Inert*X*global*"))
	(setq idxIyg  (hlpIndexOfMatch hdrIn "*Inert*Y*global*"))
	(setq idxIxyg (hlpIndexOfMatch hdrIn "*Prod*Inert*global*"))

	;; Initialize output array
	(setq arrOut '())

	;; Loop through each object property row
	(foreach row arrIn

		;; Extract values
		(setq A    (nth idxA row))
		(setq Cxg  (nth idxCxg row))
		(setq Cyg  (nth idxCyg row))
		(setq Ixg  (nth idxIxg row))
		(setq Iyg  (nth idxIyg row))
		(setq Ixyg (nth idxIxyg row))

		;; Calculate centroidal / local properties
		(setq Ixcg  (- Ixg (* A Cyg Cyg)))
		(setq Iycg  (- Iyg (* A Cxg Cxg)))
		(setq Ixycg (- Ixyg (* A Cxg Cyg)))

		(setq rxcg (sqrt (/ Ixcg A)))
		(setq rycg (sqrt (/ Iycg A)))

		;; Polar moment of inertia about centroid
		;; Not true Saint-Venant torsional constant except for circular sections.
		(setq Jcg (+ Ixcg Iycg))

		;; Append calculated values to the current row
		(setq row
			(append row
				(list
					Ixcg
					Iycg
					Ixycg
					rxcg
					rycg
					Jcg
				)
			)
		)

    ;; Add completed row to output array
    (setq arrOut (append arrOut (list row)))
	
	)

  ;; Return completed array
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
(defun c:CG2D ()
  
	;; Clear any existing selection set
  	(if (ssget "_P") (setq setRegions nil))
  
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
					"\n\nSelect Closed PolyLine or Region Objects."
					"\nPress ENTER when selection complete.")
	)
	
	(setq arrDwg (list
               (list "Dwg. Name:" strDwgName)
               (list "Date:"      dateNow)
               (list "Units:"     strUnitSys)
             ))

	
	;; general notification message to let user know calculator is starting
	;; allow to OK or CANCEL
	(setq intResult (LM:popup "Area Properties Calculator" strMsg (+ 1 32 4096))) ; OK-Cancel, Question Mark
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
	
	;; request the user to make a selection
	;; limit selections to noted types
	(setq setRegions
		(ssget
			'(
				(0 . "REGION,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
			)
		)
	)

	;; --------------------------------------------------------------------------------
	;; Main "IF" block that will call and process subroutines
	;; --------------------------------------------------------------------------------
	(if setRegions
		
	
   		;; --------------------------------------------------------------------------------
		;; TRUE condition for IF statement
		;; --------------------------------------------------------------------------------
   		(progn

			(princ "\n[RUN] Current CG2DW definition running.")

			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine 
			;; to make polylines into regions
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; (setq setRegions (subConvToReg setRegions))

			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine 
			;; to filter out non-regionable selections
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq setRegions (subFilterRegions setRegions))

			(if setRegions
				;; TRUE Action
				(progn
									
					(setq setRegions (subConvToReg setRegions))
					
					(setq numObj (sslength setRegions))
				
					(setq arrPropObj (subGet2DProps setRegions))
					
				)
				;; FALSE Action
				(progn
					(prompt "\nRoutine canceled: invalid object selected.")
					(exit)
				)
			)
			
			;;(princ (strcat "\nNumber of 2D Regions selected: " (itoa numObj)))
			
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine 
			;; to set project headers
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq hdrPropObj (subObjHdr))

			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine 
			;; additional column headers to append to the existing properties array(s)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq hdrPropAdd (subAddlHdr))
			
			;; Header of Object Properties is still a 1D list at this point, convert to array
			(setq hdrPropObj (To2D hdrPropObj))
	
			;; Header of Objects is not a 2D array with (multiple) rows x 1 column
			;; need to hlpTranspose those into 1 row x (multiple) column
			(setq hdrPropObj (hlpTranspose hdrPropObj))
	
			;; insert object densities array (column) after the "Layer" column
			(setq i (hlpIndexOfMatch hdrPropObj "*Layer*"))
			(setq j (To2D (list "Density")))

			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to do math stuff ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

			;; send header row to math for indexing purposes
			(setq arrCG (subMath2D hdrPropObj arrPropObj))
			
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; LEAVE THIS HERE...this is the final "production" output
			;; append the header columns and 
			;; send data to a text CSV file
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq hdrCSV
				(append
					(car hdrPropObj)
					hdrPropAdd
				)
			)
			
			;; create a blank spacer row
			(setq rowBlank (list ""))
			
			;; prepend header row to calculated property rows
			(setq arrCSV (cons hdrCSV arrCG))
			
			;; assemble cinal CSV export arra with drawing information
			(setq arrCSV
				(append
					arrDwg
					(list rowBlank)
					arrCSV
				)
			)	
			
			(setq strFile (hlpArrayToTxt arrCSV))

			(LM:Popup "Object Property Array Contents" (vl-prin1-to-string arrCG) (+ 1 32 4096)) ; OK-Cancel, Question Mark
			
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;; Call subroutine to hlpDrawSphere    ;;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
			(setq strCGLayer "CGmarker")
			
			(hlpLayExists strCGLayer)
			
			(setq idxCentX (hlpIndexOfMatch hdrPropObj "*Cent*X*"))
			(setq idxCentY (hlpIndexOfMatch hdrPropObj "*Cent*Y*"))
			
			(foreach row arrCG

				(setq centX (nth idxCentX row))  ; Cent X column
				(setq centY (nth idxCentY row)) ; Cent Y column
			
				(hlpDrawCG2d centX centY 1.0 strCGLayer)
			)

			
		)
      	;; ----- END OF PROGRN -------------------------------------------------------------
   
   		;; --------------------------------------------------------------------------------
		;; FALSE/ELSE condition fo IF statement
		;; --------------------------------------------------------------------------------
		(progn
		
			(alert "No 2D Closed Polylines/Regions selected.
				\nRoute required as least one solid to be selected.
				\nRoutine will now be aborted."
			)		
		)
	
	)
	;; --------------------------------------------------------------------------------
	;; END OF Main "IF" block started above
	;; --------------------------------------------------------------------------------
	
  (princ)
)
;; ================================================================================
;; ================================================================================

