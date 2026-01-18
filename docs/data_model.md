# Data Model (SAP PM/EAM Portfolio)

## Scope
This portfolio models **asset hierarchy** and **preventive maintenance** at a reporting-ready layer.

## Core tables (typical)

### Asset hierarchy
- `IFLOT` — Functional locations
- `EQUI`  — Equipment

Relationship:
- `EQUI-TPLNR` → `IFLOT-TPLNR` (equipment installed at a functional location)

### Preventive maintenance
- `MPLA` — Maintenance plans
- `MPOS` — Maintenance plan items

Relationship:
- `MPOS-WARPL` → `MPLA-WARPL`

### Execution (illustrative)
- `QMEL` — Notifications
- `AUFK` — Order header
- `AFIH` — PM order header data

## Analytical views
- `Z_I_ASSET_HIERARCHY` — Interface view combining functional locations and equipment
- `Z_I_MAINTENANCE_PLAN` — Interface view combining plans and items
- `Z_C_PM_COMPLIANCE_KPIS` — Consumption query computing overdue/due-soon KPIs per plant
