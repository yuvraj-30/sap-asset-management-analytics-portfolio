@EndUserText.label: 'Maintenance Plan Interface (Plans + Items + Schedule)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true

define view entity Z_I_MAINTENANCE_PLAN
  as select from mpla as P
  inner join     mpos as I on I.warpl = P.warpl
  /* Join MHIS to get the actual planned dates for the calls */
  inner join     mhis as S on S.warpl = P.warpl 
                          and S.wapos = I.wapos
{
  key P.warpl as MaintenancePlan,
      P.plnty as PlanCategory,
      P.wptxt as PlanText,
      P.werks as Plant,

  key I.wapos as MaintenanceItem, -- Standard key for Plan Item
      I.tplnr as FunctionalLocation,
      I.equnr as Equipment,
      
      /* Correct date fields from MHIS */
      S.npldo as PlannedDate,       -- The actual 'Next Planned Date'
      S.terma as CompletionDate,    -- Actual finish date
      S.tstat as CallStatus,        -- Scheduled, Fixed, or Completed
      
      /* Calculated Status for KPI consumption */
      case 
        when S.npldo < $session.system_date and S.tstat = ' ' 
        then 'OVERDUE'
        else 'ON_TRACK'
      end     as ComplianceStatus
}
