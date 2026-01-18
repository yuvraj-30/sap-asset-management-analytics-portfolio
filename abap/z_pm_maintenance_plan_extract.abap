"-----------------------------------------------------------------------
" Z_PM_MAINTENANCE_PLAN_EXTRACT
"-----------------------------------------------------------------------
" Purpose: Extract Maintenance Plans and perform Data Quality Audits
" as defined in data_quality_checks.md (Rules PM01-PMI04).
"-----------------------------------------------------------------------
REPORT z_pm_maintenance_plan_extract.

TABLES: mpla, mpos.

PARAMETERS: p_werks TYPE werks_d OBLIGATORY DEFAULT '0001',
            p_days  TYPE i DEFAULT 30.

TYPES: BEGIN OF ty_plan,
         warpl TYPE mpla-warpl,
         wptxt TYPE mpla-wptxt,
         werks TYPE mpla-werks,
       END OF ty_plan.

TYPES: BEGIN OF ty_item,
         warpl TYPE mpos-warpl,
         wapos TYPE mpos-wapos,
         tplnr TYPE mpos-tplnr,
         equnr TYPE mpos-equnr,
         gltrp TYPE mpos-gltrp, " Planned finish date
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

  " 1) Extract Plans
  SELECT warpl, wptxt, werks
    FROM mpla
    INTO CORRESPONDING FIELDS OF TABLE @lt_plan
    WHERE werks = @p_werks.

  " 2) Extract Items
  SELECT warpl, wapos, tplnr, equnr, gltrp
    FROM mpos
    INTO CORRESPONDING FIELDS OF TABLE @lt_item
    WHERE iwerk = @p_werks.

  " 3) Execute Data Quality Rules (Matches data_quality_checks.md)
  PERFORM check_dq USING lt_plan lt_item CHANGING lt_dq.

  " 4) Output Results
  PERFORM display_results USING lt_dq.

FORM check_dq USING it_plan TYPE STANDARD TABLE it_item TYPE STANDARD TABLE CHANGING ct_dq TYPE STANDARD TABLE.
  
  " Rule PM01 & PM02: Check Plan Header
  LOOP AT it_plan ASSIGNING FIELD-SYMBOL(<p>).
    IF <p>-wptxt IS INITIAL.
      APPEND VALUE #( object_type = 'PLAN' object_id = <p>-warpl severity = 'WARN' 
                      rule_id = 'PM02_DESC' message = 'Plan missing description' ) TO ct_dq.
    ENDIF.
  ENDLOOP.

  " Rule PMI02, PMI03, PMI04: Check Plan Items
  LOOP AT it_item ASSIGNING FIELD-SYMBOL(<i>).
    
    " PMI03: Missing planned finish date
    IF <i>-gltrp IS INITIAL.
      APPEND VALUE #( object_type = 'ITEM' object_id = <i>-warpl severity = 'WARN' 
                      rule_id = 'PMI03_NO_END' message = 'Item missing Planned Finish Date (GLTRP)' ) TO ct_dq.
    ENDIF.

    " PMI02: No Object Assigned
    IF <i>-tplnr IS INITIAL AND <i>-equnr IS INITIAL.
      APPEND VALUE #( object_type = 'ITEM' object_id = <i>-warpl severity = 'ERROR' 
                      rule_id = 'PMI02_NO_OBJ' message = 'Item has no Equipment or FuncLoc assigned' ) TO ct_dq.
    ENDIF.

    " PMI04: Orphan Plan Item (Plan header missing)
    IF NOT line_exists( it_plan[ warpl = <i>-warpl ] ).
      APPEND VALUE #( object_type = 'ITEM' object_id = <i>-warpl severity = 'ERROR' 
                      rule_id = 'PMI04_ORPHAN_PLAN' message = 'Item references non-existent Plan Header' ) TO ct_dq.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM display_results USING it_dq TYPE STANDARD TABLE.
  cl_demo_output=>new_section( 'Data Quality Issues (Maintenance Plans)' ).
  cl_demo_output=>write_data( it_dq ).
  cl_demo_output=>display( ).
ENDFORM.
