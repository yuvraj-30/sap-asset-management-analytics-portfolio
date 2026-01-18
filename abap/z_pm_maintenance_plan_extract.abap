"-----------------------------------------------------------------------
" Z_PM_MAINTENANCE_PLAN_EXTRACT
"-----------------------------------------------------------------------
" Purpose
"   Extract preventive maintenance plan/schedule information for
"   compliance reporting.
"
" Data (typical):
"   - Plans: MPLA
"   - Items: MPOS
"
" Output use cases:
"   - Overdue planned maintenance
"   - Upcoming maintenance windows
"   - Plan completeness checks
"-----------------------------------------------------------------------

REPORT z_pm_maintenance_plan_extract.

TABLES: mpla, mpos.

PARAMETERS: p_werks TYPE werks_d OBLIGATORY DEFAULT '0001',
            p_days  TYPE i DEFAULT 30.

TYPES: BEGIN OF ty_plan,
         warpl TYPE mpla-warpl,
         plnty TYPE mpla-plnty,
         wptxt TYPE mpla-wptxt,
         werks TYPE mpla-werks,
         abnum TYPE mpla-abnum,
         andat TYPE mpla-andat,
         aedat TYPE mpla-aedat,
       END OF ty_plan.

TYPES: BEGIN OF ty_item,
         warpl TYPE mpos-warpl,
         abnum TYPE mpos-abnum,
         iwerk TYPE mpos-iwerk,
         tplnr TYPE mpos-tplnr,
         equnr TYPE mpos-equnr,
         plnkn TYPE mpos-plnkn,
         gstrp TYPE mpos-gstrp,
         gltrp TYPE mpos-gltrp,
       END OF ty_item.

TYPES: BEGIN OF ty_dq_issue,
         object_type TYPE string,
         object_id   TYPE string,
         severity    TYPE string,
         rule_id     TYPE string,
         message     TYPE string,
       END OF ty_dq_issue.

DATA: lt_plan TYPE STANDARD TABLE OF ty_plan,
      lt_item TYPE STANDARD TABLE OF ty_item,
      lt_dq   TYPE STANDARD TABLE OF ty_dq_issue.

START-OF-SELECTION.

  " 1) Extract maintenance plans
  SELECT warpl, plnty, wptxt, werks, abnum, andat, aedat
    FROM mpla
    INTO TABLE @lt_plan
    WHERE werks = @p_werks.

  " 2) Extract plan items (scope to plant)
  SELECT warpl, abnum, iwerk, tplnr, equnr, plnkn, gstrp, gltrp
    FROM mpos
    INTO TABLE @lt_item
    WHERE iwerk = @p_werks.

  " 3) Data quality checks
  PERFORM dq_plans USING lt_plan CHANGING lt_dq.
  PERFORM dq_items USING lt_item CHANGING lt_dq.
  PERFORM dq_plan_item_link USING lt_plan lt_item CHANGING lt_dq.

  " 4) Compliance: identify upcoming and overdue based on finish date
  PERFORM print_compliance USING lt_plan lt_item lt_dq.

FORM dq_plans USING it_plan TYPE STANDARD TABLE CHANGING ct_dq TYPE STANDARD TABLE.
  LOOP AT it_plan ASSIGNING FIELD-SYMBOL(<p>) .
    IF <p>-warpl IS INITIAL.
      APPEND VALUE ty_dq_issue( object_type = 'PLAN' object_id = '(missing warpl)' severity = 'ERROR'
        rule_id = 'PM01_MAND_KEY' message = 'Maintenance plan missing WARPL.' ) TO ct_dq.
    ENDIF.
    IF <p>-wptxt IS INITIAL.
      APPEND VALUE ty_dq_issue( object_type = 'PLAN' object_id = <p>-warpl severity = 'WARN'
        rule_id = 'PM02_DESC' message = 'Maintenance plan missing description (WPTXT).' ) TO ct_dq.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM dq_items USING it_item TYPE STANDARD TABLE CHANGING ct_dq TYPE STANDARD TABLE.
  LOOP AT it_item ASSIGNING FIELD-SYMBOL(<i>) .
    IF <i>-warpl IS INITIAL.
      APPEND VALUE ty_dq_issue( object_type = 'PLAN_ITEM' object_id = '(missing warpl)' severity = 'ERROR'
        rule_id = 'PMI01_MAND_KEY' message = 'Maintenance plan item missing WARPL.' ) TO ct_dq.
    ENDIF.
    IF <i>-tplnr IS INITIAL AND <i>-equnr IS INITIAL.
      APPEND VALUE ty_dq_issue( object_type = 'PLAN_ITEM' object_id = <i>-warpl severity = 'ERROR'
        rule_id = 'PMI02_NO_OBJ' message = 'Plan item has neither functional location nor equipment.' ) TO ct_dq.
    ENDIF.
    IF <i>-gltrp IS INITIAL.
      APPEND VALUE ty_dq_issue( object_type = 'PLAN_ITEM' object_id = <i>-warpl severity = 'WARN'
        rule_id = 'PMI03_NO_END' message = 'Plan item missing planned finish date (GLTRP) - cannot assess overdue.' ) TO ct_dq.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM dq_plan_item_link USING it_plan TYPE STANDARD TABLE it_item TYPE STANDARD TABLE CHANGING ct_dq TYPE STANDARD TABLE.
  DATA lt_keys TYPE SORTED TABLE OF mpla-warpl WITH UNIQUE KEY table_line.
  LOOP AT it_plan ASSIGNING FIELD-SYMBOL(<p>).
    IF <p>-warpl IS NOT INITIAL.
      INSERT <p>-warpl INTO TABLE lt_keys.
    ENDIF.
  ENDLOOP.

  LOOP AT it_item ASSIGNING FIELD-SYMBOL(<i>).
    IF <i>-warpl IS NOT INITIAL AND line_exists( lt_keys[ table_line = <i>-warpl ] ) = abap_false.
      APPEND VALUE ty_dq_issue( object_type = 'PLAN_ITEM' object_id = <i>-warpl severity = 'ERROR'
        rule_id = 'PMI04_ORPHAN_PLAN' message = 'Plan item references missing maintenance plan (WARPL).' ) TO ct_dq.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM print_compliance USING it_plan TYPE STANDARD TABLE it_item TYPE STANDARD TABLE it_dq TYPE STANDARD TABLE.
  DATA: lv_today TYPE sy-datum.
  lv_today = sy-datum.

  WRITE: / 'Plant:', p_werks, 'Window (days):', p_days.
  ULINE.
  WRITE: / 'Upcoming / Overdue planned maintenance (first 50 items):'.
  ULINE.

  DATA(lv_shown) = 0.
  LOOP AT it_item ASSIGNING FIELD-SYMBOL(<i>).
    IF <i>-gltrp IS INITIAL.
      CONTINUE.
    ENDIF.

    " Overdue
    IF <i>-gltrp < lv_today.
      lv_shown += 1.
      WRITE: / 'OVERDUE', <i>-warpl, <i>-tplnr, <i>-equnr, 'Finish:', <i>-gltrp.
    " Due soon
    ELSEIF <i>-gltrp <= lv_today + p_days.
      lv_shown += 1.
      WRITE: / 'DUE_SOON', <i>-warpl, <i>-tplnr, <i>-equnr, 'Finish:', <i>-gltrp.
    ENDIF.

    IF lv_shown >= 50.
      EXIT.
    ENDIF.
  ENDLOOP.

  ULINE.
  WRITE: / 'DQ issues:', lines( it_dq ).
ENDFORM.
