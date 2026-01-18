"-----------------------------------------------------------------------
" Z_PM_ASSET_MASTER_EXTRACT
"-----------------------------------------------------------------------
" Purpose
"   Extract SAP PM/EAM asset master data for analytics:
"   - Functional Locations (IFLOT)
"   - Equipment (EQUI)
"   Includes quality checks and a clean, report-friendly output.
"
" Typical use
"   - Master data governance (completeness, standardisation)
"   - Asset hierarchy reporting
"   - Audit support (who/when changed)
"
" Notes
"   Field names may differ by system release/customising.
"   This is portfolio-ready ABAP demonstrating robust patterns.
"-----------------------------------------------------------------------

REPORT z_pm_asset_master_extract.

TABLES: iflot, equi.

PARAMETERS: p_werks TYPE werks_d OBLIGATORY DEFAULT '0001',
            p_inact AS CHECKBOX DEFAULT abap_false.

TYPES: BEGIN OF ty_func_loc,
         tplnr   TYPE iflot-tplnr,
         pltxt   TYPE iflotx-pltxt,
         iwerk   TYPE iflot-iwerk,
         stort   TYPE iflot-stort,
         ingrp   TYPE iflot-ingrp,
         tplma   TYPE iflot-tplma,
         datuv   TYPE iflot-datuv,
         datub   TYPE iflot-datub,
         objnr   TYPE iflot-objnr,
       END OF ty_func_loc.

TYPES: BEGIN OF ty_equip,
         equnr   TYPE equi-equnr,
         eqktx   TYPE eqkt-eqktx,
         tplnr   TYPE equi-tplnr,
         werks   TYPE equi-werks,
         stort   TYPE equi-stort,
         herst   TYPE equi-herst,
         typbz   TYPE equi-typbz,
         sernr   TYPE equi-sernr,
         objnr   TYPE equi-objnr,
         inbdt   TYPE equi-inbdt,
         aedat   TYPE equi-aedat,
       END OF ty_equip.

TYPES: BEGIN OF ty_dq_issue,
         object_type TYPE string,
         object_id   TYPE string,
         severity    TYPE string,
         rule_id     TYPE string,
         message     TYPE string,
       END OF ty_dq_issue.

DATA: lt_floc TYPE STANDARD TABLE OF ty_func_loc,
      lt_equi TYPE STANDARD TABLE OF ty_equip,
      lt_dq   TYPE STANDARD TABLE OF ty_dq_issue.

START-OF-SELECTION.

  " 1) Extract functional locations for a plant
  SELECT f~tplnr, x~pltxt, f~iwerk, f~stort, f~ingrp, f~tplma, f~objnr
    FROM iflot AS f
    LEFT JOIN iflotx AS x ON f~tplnr = x~tplnr AND x~spras = @sy-langu
    WHERE f~iwerk = @p_werks
    INTO TABLE @lt_floc.

  " 2) Extract equipment for a plant
  SELECT e~equnr, t~eqktx, e~tplnr, e~werks, e~stort, e~herst, e~typbz, e~sernr, e~objnr
    FROM equi AS e
    LEFT JOIN eqkt AS t ON e~equnr = t~equnr AND t~spras = @sy-langu
    WHERE e~werks = @p_werks
    INTO TABLE @lt_equi.

  " 4) Data Quality Checks
  PERFORM dq_check_floc USING lt_floc CHANGING lt_dq.
  PERFORM dq_check_equi USING lt_equi CHANGING lt_dq.
  PERFORM dq_check_orphans USING lt_floc lt_equi CHANGING lt_dq.

  " 5) Output
  PERFORM print_summary USING lt_floc lt_equi lt_dq.

FORM dq_check_floc CHANGING ct_dq TYPE STANDARD TABLE.
  LOOP AT it_floc ASSIGNING FIELD-SYMBOLS <f>.
    IF <f>-tplnr IS INITIAL.
      APPEND VALUE #(
        object_type = 'FUNC_LOC'
        object_id   = '(missing tplnr)'
        severity    = 'ERROR'
        rule_id     = 'FL01_MAND_KEY'
        message     = 'Functional Location missing TPLNR.' ) TO ct_dq.
    ENDIF.
    IF <f>-pltxt IS INITIAL.
      APPEND VALUE #(
        object_type = 'FUNC_LOC'
        object_id   = <f>-tplnr
        severity    = 'WARN'
        rule_id     = 'FL02_DESC'
        message     = 'Functional Location missing description (PLTXT).' ) TO ct_dq.
    ENDIF.
    IF <f>-tplma IS NOT INITIAL AND <f>-tplma = <f>-tplnr.
      APPEND VALUE #(
        object_type = 'FUNC_LOC'
        object_id   = <f>-tplnr
        severity    = 'ERROR'
        rule_id     = 'FL03_SELF_PARENT'
        message     = 'Functional Location has invalid parent (self-referential).' ) TO ct_dq.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM dq_check_equi USING it_equi TYPE STANDARD TABLE CHANGING ct_dq TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <e> TYPE ty_equip.
  LOOP AT it_equi ASSIGNING <e>.
    IF <e>-equnr IS INITIAL.
      APPEND VALUE #(
        object_type = 'EQUIPMENT'
        object_id   = '(missing equnr)'
        severity    = 'ERROR'
        rule_id     = 'EQ01_MAND_KEY'
        message     = 'Equipment missing EQUNR.' ) TO ct_dq.
    ENDIF.
    IF <e>-eqktx IS INITIAL.
      APPEND VALUE #(
        object_type = 'EQUIPMENT'
        object_id   = <e>-equnr
        severity    = 'WARN'
        rule_id     = 'EQ02_DESC'
        message     = 'Equipment missing description (EQKTX).' ) TO ct_dq.
    ENDIF.
    IF <e>-tplnr IS INITIAL.
      APPEND VALUE #(
        object_type = 'EQUIPMENT'
        object_id   = <e>-equnr
        severity    = 'WARN'
        rule_id     = 'EQ03_NO_FLOC'
        message     = 'Equipment not assigned to a functional location (TPLNR).' ) TO ct_dq.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM dq_check_orphans USING it_floc TYPE STANDARD TABLE it_equi TYPE STANDARD TABLE CHANGING ct_dq TYPE STANDARD TABLE.
  DATA: lt_floc_keys TYPE SORTED TABLE OF iflot-tplnr WITH UNIQUE KEY table_line.
  FIELD-SYMBOLS: <f> TYPE ty_func_loc.
  FIELD-SYMBOLS: <e> TYPE ty_equip.

  LOOP AT it_floc ASSIGNING <f>.
    IF <f>-tplnr IS NOT INITIAL.
      INSERT <f>-tplnr INTO TABLE lt_floc_keys.
    ENDIF.
  ENDLOOP.

  LOOP AT it_equi ASSIGNING <e>.
    IF <e>-tplnr IS NOT INITIAL AND line_exists( lt_floc_keys[ table_line = <e>-tplnr ] ) = abap_false.
      APPEND VALUE #(
        object_type = 'EQUIPMENT'
        object_id   = <e>-equnr
        severity    = 'ERROR'
        rule_id     = 'EQ04_ORPHAN_FLOC'
        message     = |Equipment references unknown functional location: { <e>-tplnr }| ) TO ct_dq.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM print_summary USING it_floc TYPE STANDARD TABLE it_equi TYPE STANDARD TABLE it_dq TYPE STANDARD TABLE.
  DATA(lv_floc) = lines( it_floc ).
  DATA(lv_equi) = lines( it_equi ).
  DATA(lv_dq)   = lines( it_dq ).

  WRITE: / 'Plant:', p_werks.
  WRITE: / 'Functional Locations:', lv_floc.
  WRITE: / 'Equipment:', lv_equi.
  WRITE: / 'DQ Issues:', lv_dq.
  ULINE.

  IF lv_dq > 0.
    WRITE: / 'DQ Issues (first 50):'.
    ULINE.
    DATA(lv_i) = 0.
    LOOP AT it_dq ASSIGNING FIELD-SYMBOL(<d>). 
      lv_i += 1.
      WRITE: / <d>-severity, <d>-rule_id, <d>-object_type, <d>-object_id, <d>-message.
      IF lv_i >= 50.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
    WRITE: / 'No data quality issues found for selected scope.'
  ENDIF.
ENDFORM.
