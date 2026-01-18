"-----------------------------------------------------------------------
" Z_PM_ASSET_MASTER_EXTRACT (DOC Aligned)
"-----------------------------------------------------------------------
" Purpose
"   Extract & Validate Master Data for Asset Management (SAP-PM/AMIS).
"   Supports 'Asset Data Coordinator' duties:
"   - Master Data Maintenance (Functional Locations/Equipment)
"   - Identifying Safety Risks (Aging Infrastructure)
"   - Data Quality Triage for decision making
"-----------------------------------------------------------------------

REPORT z_pm_asset_master_extract.

TABLES: iflot, equi.

" Selection Screen: Standard filters for Asset Coordinators
PARAMETERS: p_werks TYPE werks_d OBLIGATORY DEFAULT '0001',
            p_risky AS CHECKBOX DEFAULT abap_true. " Focus on risk items

TYPES: BEGIN OF ty_asset_row,
         asset_id    TYPE string,
         asset_type  TYPE string,
         description TYPE string,
         parent_id   TYPE string,
         startup_date TYPE datum,
         planner_grp TYPE ingrp,
       END OF ty_asset_row.

TYPES: BEGIN OF ty_dq_issue,
         severity TYPE c LENGTH 1, " E=Error, W=Warning
         rule_id  TYPE c LENGTH 20,
         asset_id TYPE string,
         message  TYPE string,
       END OF ty_dq_issue.

DATA: gt_assets TYPE STANDARD TABLE OF ty_asset_row,
      gt_dq     TYPE STANDARD TABLE OF ty_dq_issue.

START-OF-SELECTION.

  " 1. Extract Equipment (The physical assets: Huts, Bridges, Vehicles)
  SELECT
    equi~equnr AS asset_id,
    'EQUIPMENT' AS asset_type,
    eqkt~eqktx AS description,
    equi~tplnr AS parent_id,
    equi~inbdt AS startup_date,
    equi~ingrp AS planner_grp
  FROM equi
  LEFT JOIN eqkt ON equi~equnr = eqkt~equnr AND eqkt~spras = @sy-langu
  WHERE equi~werks = @p_werks
  INTO CORRESPONDING FIELDS OF TABLE @gt_assets.

  " 2. Extract Functional Locations (The spatial hierarchy)
  SELECT
    iflot~tplnr AS asset_id,
    'FUNC_LOC'  AS asset_type,
    iflotx~pltxt AS description,
    iflot~tplma AS parent_id,
    iflot~inbdt AS startup_date,
    iflot~ingrp AS planner_grp
  FROM iflot
  LEFT JOIN iflotx ON iflot~tplnr = iflotx~tplnr AND iflotx~spras = @sy-langu
  WHERE iflot~iwerk = @p_werks
  APPENDING CORRESPONDING FIELDS OF TABLE @gt_assets.

  " 3. Run Data Quality Triage (Automated Triage)
  PERFORM run_dq_checks.

  " 4. Output Results (Simulating Export to Excel for Coordinators)
  PERFORM display_results.

FORM run_dq_checks.
  LOOP AT gt_assets INTO DATA(ls_row).

    " Rule 1: Master Data Completeness (Job Requirement: Maintain Master Data)
    IF ls_row-description IS INITIAL.
      APPEND VALUE #( severity = 'E' rule_id = 'MD_MISSING_DESC' asset_id = ls_row-asset_id
                      message = 'Asset missing description (Critical Master Data Gap)' ) TO gt_dq.
    ENDIF.

    " Rule 2: Hierarchy Integrity (Job Requirement: Functional Locations)
    IF ls_row-asset_type = 'EQUIPMENT' AND ls_row-parent_id IS INITIAL.
      APPEND VALUE #( severity = 'W' rule_id = 'MD_ORPHAN_ASSET' asset_id = ls_row-asset_id
                      message = 'Equipment not installed in a Functional Location' ) TO gt_dq.
    ENDIF.

    " Rule 3: Safety & Risk (Job Requirement: Health & Safety / Threat Management)
    " Flag assets older than 20 years for inspection
    IF ls_row-startup_date IS NOT INITIAL AND ls_row-startup_date < '20040101'.
      APPEND VALUE #( severity = 'I' rule_id = 'SAFETY_AGING' asset_id = ls_row-asset_id
                      message = 'Aging Asset (>20 years): Review for safety/compliance' ) TO gt_dq.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM display_results.
  WRITE: / 'Asset Data Coordinator - Validation Report'.
  WRITE: / 'Total Assets Scanned:', lines( gt_assets ).
  WRITE: / 'Data Issues Found:', lines( gt_dq ).
  ULINE.

  LOOP AT gt_dq INTO DATA(ls_dq).
    WRITE: / ls_dq-severity, ls_dq-asset_id, ls_dq-rule_id, ls_dq-message.
  ENDLOOP.
ENDFORM.
