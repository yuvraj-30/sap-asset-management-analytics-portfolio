"-----------------------------------------------------------------------
" Z_PM_COMPLIANCE_KPI_RECON
"-----------------------------------------------------------------------
" Purpose: Reconcile planned maintenance items against actual execution.
" Validates 'Dark Data' (orphaned plans) and execution compliance.
"-----------------------------------------------------------------------
REPORT z_pm_compliance_kpi_recon.

PARAMETERS: p_werks TYPE werks_d OBLIGATORY DEFAULT '0001',
            p_from  TYPE sy-datum DEFAULT sy-datum-30,
            p_to    TYPE sy-datum DEFAULT sy-datum.

TYPES: BEGIN OF ty_kpi,
         plant             TYPE werks_d,
         period_from       TYPE sy-datum,
         period_to         TYPE sy-datum,
         overdue_pm_items  TYPE i,
         due_soon_pm_items TYPE i,
         open_notif_crit   TYPE i,
         orders_created    TYPE i,
         orders_closed     TYPE i,
       END OF ty_kpi.

DATA: ls_kpi TYPE ty_kpi.

START-OF-SELECTION.
  ls_kpi-plant       = p_werks.
  ls_kpi-period_from = p_from.
  ls_kpi-period_to   = p_to.

  " 1) Overdue / Due Soon (Source: Maintenance Plan History - MHIS)
  SELECT 
    COUNT( CASE WHEN npldo < @sy-datum THEN 1 END ) AS overdue,
    COUNT( CASE WHEN npldo BETWEEN @sy-datum AND @( sy-datum + 30 ) THEN 1 END ) AS due_soon
    FROM mhis
    WHERE iwerk = @p_werks
      AND tstat = ' ' " Status: Scheduled/Called (not skipped)
    INTO (@ls_kpi-overdue_pm_items, @ls_kpi-due_soon_pm_items).

  " 2) Open Critical Notifications (Source: QMEL)
  SELECT COUNT(*)
    FROM qmel
    WHERE iwerk = @p_werks
      AND priok = '1' " Priority 1 (Critical)
      AND qmstat = '1' " Status: Outstanding
    INTO @ls_kpi-open_notif_crit.

  " 3) & 4) Orders Created vs Closed (Source: AUFK Header)
  " CRITICAL FIX: Used IDAT2 (Technical Completion) instead of ISDD (Start Date)
  SELECT 
    COUNT( CASE WHEN a~erdat BETWEEN @p_from AND @p_to THEN 1 END ) AS created,
    COUNT( CASE WHEN a~idat2 BETWEEN @p_from AND @p_to THEN 1 END ) AS closed
    FROM aufk AS a
    INNER JOIN afih AS i ON a~aufnr = i~aufnr
    WHERE i~iwerk = @p_werks
    INTO (@ls_kpi-orders_created, @ls_kpi-orders_closed).

  PERFORM print_kpi USING ls_kpi.

FORM print_kpi USING is_kpi TYPE ty_kpi.
  cl_demo_output=>begin_section( 'PM Compliance KPI Summary' ).
  cl_demo_output=>write_data( is_kpi ).
  cl_demo_output=>display( ).
ENDFORM.
