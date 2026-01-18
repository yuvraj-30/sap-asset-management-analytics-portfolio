# SAP Asset Management (EAM/PM) Data Portfolio (ABAP + CDS + RAP)

## Purpose
This repository demonstrates an **end-to-end SAP Enterprise Asset Management (EAM) / Plant Maintenance (PM)**
portfolio designed for roles such as:
- Asset Data Coordinator / EAM Master Data Specialist
- SAP PM / EAM Analyst
- SAP Data / Analytics Engineer (asset domain)

The focus is **asset master data quality**, **maintenance compliance reporting**, and **governed analytics** built from SAP.

---

## Business Scenario
A regulated organisation (utilities / local government / public sector) needs trusted asset data and compliance reporting across:
- **Asset hierarchy** (Functional Locations + Equipment)
- **Preventive maintenance** plans and schedules
- **Work execution** (Notifications + Orders)
- **Operational compliance indicators** (overdue PM, completion rates, open critical notifications)

---

## Repository Structure

```
sap-asset-management-analytics-portfolio/
├── abap/
│   ├── z_pm_asset_master_extract.abap
│   ├── z_pm_maintenance_plan_extract.abap
│   └── z_pm_compliance_kpi_recon.abap
│
├── cds/
│   ├── z_i_asset_hierarchy.cds
│   ├── z_i_maintenance_plan.cds
│   ├── z_c_pm_compliance_kpis.cds
│
├── rap/
│   ├── behavior_definition.abap
│   ├── service_definition.abap
│   └── service_binding.abap
│
├── sql/
│   └── hana_validation_queries.sql
│
├── docs/
│   ├── data_model.md
│   └── data_quality_checks.md
│
└── README.md
```

---

## Notes on realism
- Table names used are common in SAP PM/EAM (e.g., `IFLOT`, `EQUI`, `MPLA`, `MPOS`, `QMEL`, `AUFK`, `AFIH`).
- Exact fields can differ between ECC and S/4HANA and by industry add-ons.
- This is a **portfolio**: the objective is to show correct *patterns* (master data governance, compliance reporting, RAP exposure), not to claim production access.

---

## How to present this portfolio
In interviews, walk through:
1. Asset hierarchy extraction (functional locations + equipment)
2. Preventive maintenance schedule extraction (plans + next due)
3. Compliance KPI calculation (overdue %, completion %, open critical notifications)
4. Quality checks (mandatory keys, orphan relationships, invalid statuses)
5. CDS modelling and RAP exposure for reporting tools

---

## Author
Yuvraj Singh
