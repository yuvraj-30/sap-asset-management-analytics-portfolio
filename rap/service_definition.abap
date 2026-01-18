"-----------------------------------------------------------------------
" RAP Service Definition
"-----------------------------------------------------------------------
" Exposes the PM compliance KPIs consumption view as an OData service.
"
" Create in ADT: Service Definition
" Name: ZUI_PM_COMPLIANCE
" Source below is valid service definition syntax.
"-----------------------------------------------------------------------

service definition ZUI_PM_COMPLIANCE {
  expose Z_C_PM_COMPLIANCE_KPIS as PMComplianceKpis;
}
