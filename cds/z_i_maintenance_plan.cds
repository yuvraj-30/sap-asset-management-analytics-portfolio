@EndUserText.label: 'Maintenance Plan Interface (Plans + Items)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true

define view entity Z_I_MAINTENANCE_PLAN
  as select from mpla as P
  inner join mpos as I
    on I.warpl = P.warpl
{
  key P.warpl as MaintenancePlan,
      P.plnty as PlanCategory,
      P.wptxt as PlanText,
      P.werks as Plant,

  key I.plnkn as PlanItem,
      I.tplnr as FunctionalLocation,
      I.equnr as Equipment,
      I.gstrp as PlannedStartDate,
      I.gltrp as PlannedFinishDate
}
