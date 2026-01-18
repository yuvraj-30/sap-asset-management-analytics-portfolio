@EndUserText.label: 'Asset Hierarchy Interface (Functional Locations + Equipment)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true

define view entity Z_I_ASSET_HIERARCHY
  as select from iflot as F
  left outer join equi as E
    on E.tplnr = F.tplnr
{
  /* Functional Location */
  key F.tplnr  as FunctionalLocation,
      F.pltxt  as FunctionalLocationText,
      F.iwerk  as Plant,
      F.tplma  as ParentFunctionalLocation,
      F.stort  as Location,
      F.ingrp  as PlannerGroup,

  /* Equipment (optional) */
      E.equnr  as Equipment,
      E.eqktx  as EquipmentText,
      E.herst  as Manufacturer,
      E.typbz  as Model,
      E.sernr  as SerialNumber,
      E.inbdt  as StartupDate,
      E.aedat  as ChangedOn
}
