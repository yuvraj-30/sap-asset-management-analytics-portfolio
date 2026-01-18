"-----------------------------------------------------------------------
" Z_PM_COMPLIANCE_KPI_RECON
"-----------------------------------------------------------------------
" Purpose
"   Compute basic PM compliance KPIs by reconciling plan schedule items
"   against execution evidence (notifications/orders).
"
" Typical tables (portfolio selection):
"   - Maintenance plans/items: MPLA, MPOS
"   - Notifications: QMEL
"   - Orders (header): AUFK
"   - Orders (PM-specific): AFIH
"
" KPIs (illustrative):
"   - Overdue planned maintenance count
"   - Open critical notifications (priority)
"   - Work orders created/completed in period
"
" Notes
"   Status handling is usually via JEST/JCDS; here we use simple fields
"   where possible to demonstrate the reporting pattern.
"-----------------------------------------------------------------------

REPORT z_pm_compliance_kpi_recon.

PARAMETERS: p_werks TYPE werks_d OBLIGATORY DEFAULT '0001',
            p_from  TYPE sy-datum DEFAULT sy-datum-30,
            p_to    TYPE sy-datum DEFAULT sy-datum.

TYPES: BEGIN OF ty_kpi,
         plant                TYPE werks_d,
         period_from          TYPE sy-datum,
         period_to            TYPE sy-datum,
         overdue_pm_items     TYPE i,
         due_soon_pm_items    TYPE i,
         open_notif_crit      TYPE i,
         orders_created       TYPE i,
         orders_closed        TYPE i,
       END OF ty_kpi.

DATA: ls_kpi TYPE ty_kpi.

START-OF-SELECTION.
  ls_kpi-plant       = p_werks.
  ls_kpi-period_from = p_from.
  ls_kpi-period_to   = p_to.

  " 1) Overdue / due soon based on MPOS planned finish
  SELECT COUNT(*)
    FROM mpos
    INTO @ls_kpi-overdue_pm_items
    WHERE iwerk = @p_werks
      AND gltrp IS NOT NULL
      AND gltrp < @sy-datum.

  SELECT COUNT(*)
    FROM mpos
    INTO @ls_kpi-due_soon_pm_items
    WHERE iwerk = @p_werks
      AND gltrp IS NOT NULL
      AND gltrp BETWEEN @sy-datum AND @sy-datum + 30.

  " 2) Open critical notifications (illustrative priority field)
  "    In real systems, use status tables and custom priority definitions.
  SELECT COUNT(*)
    FROM qmel
    INTO @ls_kpi-open_notif_crit
    WHERE iwerk = @p_werks
      AND priok = '1'.

  " 3) Orders created in period
  SELECT COUNT(*)
    FROM aufk
    INTO @ls_kpi-orders_created
    WHERE erdat BETWEEN @p_from AND @p_to.

  " 4) Orders closed in period (illustrative: basic completion date)
  "    Many systems track TECO/CLSD via JEST/JCDS; this is a portfolio simplification.
  SELECT COUNT(*)
    FROM afih
    INTO @ls_kpi-orders_closed
    WHERE iwerk = @p_werks
      AND isdd BETWEEN @p_from AND @p_to.

  PERFORM print_kpi USING ls_kpi.

FORM print_kpi USING is_kpi TYPE ty_kpi.
  ULINE.
  WRITE: / 'PM Compliance KPI Summary'.
  ULINE.
  WRITE: / 'Plant:', is_kpi-plant.
  WRITE: / 'Period:', is_kpi-period_from, 'to', is_kpi-period_to.
  ULINE.
  WRITE: / 'Overdue PM items:', is_kpi-overdue_pm_items.
  WRITE: / 'Due soon PM items (next 30 days):', is_kpi-due_soon_pm_items.
  WRITE: / 'Open critical notifications:', is_kpi-open_notif_crit.
  WRITE: / 'Orders created:', is_kpi-orders_created.
  WRITE: / 'Orders closed:', is_kpi-orders_closed.
  ULINE.
ENDFORM.
