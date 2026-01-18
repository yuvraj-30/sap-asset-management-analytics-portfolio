"-----------------------------------------------------------------------
" RAP Read-Only Query Exposure for Asset Compliance KPIs
"-----------------------------------------------------------------------
" This folder provides a pragmatic RAP pattern for exposing read-only
" analytics (CDS query) via OData.
"
" Object names (suggested):
"   - CDS consumption view: Z_C_PM_COMPLIANCE_KPIS
"   - Query provider class: ZCL_PM_COMPLIANCE_QP
"
" In ABAP, read-only analytic queries are often exposed directly via
" service definition; however, a RAP query provider helps if you need
" custom filtering/paging logic or multi-source joins.
"-----------------------------------------------------------------------

" 1) Behavior definition (read-only)
" Note: For a pure CDS query, behavior is minimal.

define behavior for Z_C_PM_COMPLIANCE_KPIS alias PMComplianceKpis
implementation in class ZCL_PM_COMPLIANCE_QP unique
{
  read;
}

" 2) Behavior implementation: Query provider
" Implements IF_RAP_QUERY_PROVIDER to support RAP read operations.

CLASS zcl_pm_compliance_qp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.

  PRIVATE SECTION.
    METHODS select_kpis
      IMPORTING
        io_request  TYPE REF TO if_rap_query_request
        io_response TYPE REF TO if_rap_query_response.
ENDCLASS.

CLASS zcl_pm_compliance_qp IMPLEMENTATION.

  METHOD if_rap_query_provider~select.
    me->select_kpis( io_request = io_request io_response = io_response ).
  ENDMETHOD.

  METHOD select_kpis.
    " Basic pattern: read from CDS consumption view.
    " In real implementations, apply filters from io_request.

    DATA: lt_result TYPE STANDARD TABLE OF z_c_pm_compliance_kpis.

    SELECT *
      FROM z_c_pm_compliance_kpis
      INTO TABLE @lt_result.

    " Paging (simple): respect requested page size if provided
    DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
    IF lv_page_size IS NOT INITIAL AND lines( lt_result ) > lv_page_size.
      DELETE lt_result FROM lv_page_size + 1 TO lines( lt_result ).
    ENDIF.

    io_response->set_total_number_of_records( lines( lt_result ) ).
    io_response->set_data( lt_result ).

  ENDMETHOD.

ENDCLASS.
