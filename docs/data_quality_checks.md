# Data Quality Checks (SAP PM/EAM Portfolio)

## Objectives
Asset and maintenance compliance reporting depends on **trusted master data** and **reliable schedule data**.
This portfolio implements checks that mirror what regulated organisations expect.

## Checks implemented

### Functional locations (IFLOT)
- **FL01_MAND_KEY (ERROR):** Functional location key (TPLNR) must be present
- **FL02_DESC (WARN):** Description (PLTXT) should be present
- **FL03_SELF_PARENT (ERROR):** Parent functional location cannot equal self

### Equipment (EQUI)
- **EQ01_MAND_KEY (ERROR):** Equipment number (EQUNR) must be present
- **EQ02_DESC (WARN):** Equipment text (EQKTX) should be present
- **EQ03_NO_FLOC (WARN):** Equipment should be assigned to a functional location (TPLNR)
- **EQ04_ORPHAN_FLOC (ERROR):** Equipment TPLNR must exist in IFLOT

### Maintenance plans (MPLA) and items (MPOS)
- **PM01_MAND_KEY (ERROR):** Plan key (WARPL) must be present
- **PM02_DESC (WARN):** Plan text (WPTXT) should be present
- **PMI02_NO_OBJ (ERROR):** Plan item must reference a functional location or equipment
- **PMI03_NO_END (WARN):** Plan item should have planned finish date (GLTRP)
- **PMI04_ORPHAN_PLAN (ERROR):** Plan item WARPL must exist in MPLA

## Escalation approach
- **ERROR:** blocks trusted reporting; must be corrected or explicitly accepted with documented rationale
- **WARN:** may be reported with caveats; prioritised for cleanup

## Evidence
- ABAP reports print DQ issues and sample compliance outputs
- `sql/hana_validation_queries.sql` provides matching SQL checks
