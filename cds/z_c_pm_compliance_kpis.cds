@EndUserText.label: 'PM Compliance KPIs (Consumption View)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@Analytics.query: true

define view entity Z_C_PM_COMPLIANCE_KPIS
  as select from Z_I_MAINTENANCE_PLAN
{
  @AnalyticsDetails.query.axis: #ROWS
  key Plant,

  /* KPI: Total Items */
  @DefaultAggregation: #SUM
  @EndUserText.label: 'Total Plan Items'
  count( * ) as TotalPlanItems,

  /* KPI: Overdue items */
  @DefaultAggregation: #SUM
  @EndUserText.label: 'Overdue Items'
  sum(
    case
      when PlannedFinishDate is not null 
       and PlannedFinishDate < $session.system_date 
       and PlannedFinishDate <> '00000000' then 1
      else 0
    end
  ) as OverdueItems,

  /* KPI: Due in next 30 days */
  @DefaultAggregation: #SUM
  @EndUserText.label: 'Due Soon (30d)'
  sum(
    case
      /* CRITICAL FIX: Safe date math. Returns NULL if calculation fails. */
      when PlannedFinishDate between $session.system_date 
                             and dats_add_days($session.system_date, 30, 'NULL') then 1
      else 0
    end
  ) as DueSoonItems
}
group by 
  Plant
