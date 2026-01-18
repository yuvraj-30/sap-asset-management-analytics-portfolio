@EndUserText.label: 'Asset Hierarchy Interface (Functional Locations + Equipment)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true

define view entity Z_I_ASSET_HIERARCHY
  as select from iflot  as F
  left outer join iflotx as FX on  FX.tplnr = F.tplnr 
                               and FX.spras = $session.system_language
  left outer join equi   as E  on  E.tplnr  = F.tplnr
  left outer join eqkt   as ET on  ET.equnr = E.equnr
                               and ET.spras = $session.system_language
{
  /* Functional Location Data */
  key F.tplnr  as FunctionalLocation,
      FX.pltxt as FunctionalLocationText,
      F.iwerk  as Plant,
      F.tplma  as ParentFunctionalLocation,
      F.stort  as Location,
      F.ingrp  as PlannerGroup,

  /* Equipment Data (Nested within F-Loc) */
      E.equnr  as Equipment,
      ET.eqktx as EquipmentText,
      E.herst  as Manufacturer,
      E.typbz  as Model,
      E.sernr  as SerialNumber,
      
  /* Dates and Metadata */
      E.inbdt  as StartupDate,
      E.aedat  as ChangedOn
}
