@EndUserText.label: 'PM Compliance KPIs (Consumption View)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Analytics.query: true

define view entity Z_C_PM_COMPLIANCE_KPIS
  as select from Z_I_MAINTENANCE_PLAN
{
  Plant,

  /* KPI: Overdue items */
  count( * ) as TotalPlanItems,

  sum(
    case
      when PlannedFinishDate is not null and PlannedFinishDate < $session.system_date then 1
      else 0
    end
  ) as OverdueItems,

  sum(
    case
      when PlannedFinishDate is not null
       and PlannedFinishDate between $session.system_date and add_days( $session.system_date, 30 ) then 1
      else 0
    end
  ) as DueSoonItems
}

group by Plant
