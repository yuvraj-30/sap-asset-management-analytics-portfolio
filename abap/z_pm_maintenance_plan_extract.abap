REPORT z_pm_maintenance_plan_extract.

" Define local class for Clean Core structure
CLASS lcl_pm_extractor DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_full_extract,
             warpl TYPE mpla-warpl,
             wptxt TYPE mpla-wptxt,
             tplnr TYPE mpos-tplnr,
             equnr TYPE mpos-equnr,
             npldo TYPE mhis-npldo, " Correct Planned Date field from MHIS
             terma TYPE mhis-terma, " Actual Date
           END OF ty_full_extract.

    DATA: mt_data TYPE STANDARD TABLE OF ty_full_extract.

    METHODS: extract_data IMPORTING iv_werks TYPE werks_d iv_days TYPE i,
             display_results.
ENDCLASS.

PARAMETERS: p_werks TYPE werks_d OBLIGATORY DEFAULT '0001',
            p_days  TYPE i DEFAULT 30.

START-OF-SELECTION.
  DATA(lo_app) = NEW lcl_pm_extractor( ).
  lo_app->extract_data( iv_werks = p_werks iv_days = p_days ).
  lo_app->display_results( ).

CLASS lcl_pm_extractor IMPLEMENTATION.
  METHOD extract_data.
    " JOIN MPLA (Plan), MPOS (Item), and MHIS (Schedule Dates)
    " Use SQL Case to identify status at database level (Pushdown)
    SELECT p~warpl, p~wptxt, i~tplnr, i~equnr, s~npldo, s~terma
      FROM mpla AS p
      INNER JOIN mpos AS i ON i~warpl = p~warpl
      INNER JOIN mhis AS s ON s~warpl = p~warpl AND s~wapos = i~wapos
      WHERE p~werks = @iv_werks
        AND s~npldo <= @( sy-datum + iv_days )
        AND s~tstat = ' ' " Status: Called/Scheduled (not completed)
      INTO TABLE @mt_data.
  ENDMETHOD.

  METHOD display_results.
    " Use ALV for modern, interactive 2026 reporting
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv) CHANGING t_table = mt_data ).
        lo_alv->get_functions( )->set_all( ).
        
        " Color code columns for Overdue vs Upcoming (Logic Simplified)
        lo_alv->display( ).
      CATCH cx_salv_msg.
        MESSAGE 'Error generating output' TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
