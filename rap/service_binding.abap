"-----------------------------------------------------------------------
" RAP Service Binding (Guidance + Configuration)
"-----------------------------------------------------------------------
" In ABAP development tools (ADT), Service Binding is created as a
" repository object (not purely code). This file provides exact, runnable
" configuration steps that make the portfolio end-to-end.
"
" 1) In ADT: New -> Service Binding
"    - Name: ZUI_PM_COMPLIANCE_BIND
"    - Binding Type: OData V4 (recommended)
"    - Service Definition: ZUI_PM_COMPLIANCE
"
" 2) Publish the binding.
"    - Right-click binding -> Publish
"
" 3) Verify
"    - Use /IWFND/MAINT_SERVICE or ADT preview
"    - Confirm entity set: PMComplianceKpis
"
" 4) Security
"    - In production, define DCL for CDS authorization.
"    - For portfolio, @AccessControl.authorizationCheck: #NOT_REQUIRED
"      is used to keep focus on modelling.
"-----------------------------------------------------------------------
