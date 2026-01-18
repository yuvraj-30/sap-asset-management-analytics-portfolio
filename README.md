# SAP Asset Management (EAM) Analytics Portfolio

![SAP ABAP](https://img.shields.io/badge/SAP-ABAP%207.5+-blue?style=flat&logo=sap) ![RAP](https://img.shields.io/badge/Model-RAP%20(Restful%20ABAP)-green) ![Status](https://img.shields.io/badge/Status-Portfolio%20Ready-success)

## 📊 Project Overview
This project demonstrates an **End-to-End Analytics & Data Governance** solution for SAP Enterprise Asset Management (PM/EAM).

In regulated industries (Utilities, Public Sector, Manufacturing), "Dark Data"—such as assets missing location data or maintenance plans without future dates—leads to compliance failures. This solution solves that by providing:
1.  **Automated Data Quality Audits:** Identifies orphan assets and incomplete master data.
2.  **Compliance KPIs:** Tracks overdue maintenance items and schedule compliance.
3.  **Modern API Layer:** Exposes these insights via **SAP RAP (RESTful ABAP Programming)** to external dashboards like SAP Analytics Cloud.

## 🛠️ Tech Stack
* **Backend Logic:** ABAP 7.5+ (OO-ABAP, New Syntax)
* **Data Modeling:** ABAP CDS Views (Core Data Services)
* **API Exposure:** SAP RAP (Service Definition, Service Binding)
* **Database:** SAP HANA (SQLScript for validation)

## 📂 Repository Structure

### 1. Data Extraction & Logic (ABAP)
* `z_pm_asset_master_extract.abap`: Extracts Functional Locations (`IFLOT`) and Equipment (`EQUI`). Implements a rules engine to flag "orphan" equipment.
* `z_pm_maintenance_plan_extract.abap`: Extracts Maintenance Plans (`MPLA`) and Items (`MPOS`), calculating dynamic "Overdue" status based on system date.
* `z_pm_compliance_kpi_recon.abap`: A reconciliation report comparing planned maintenance dates against actual Work Orders (`AUFK`) and Notifications (`QMEL`).

### 2. Modern Data Modeling (CDS & RAP)
* `z_i_asset_hierarchy.cds`: Interface view joining Equipment to Functional Locations.
* `z_c_pm_compliance_kpis.cds`: **Analytics Consumption View**. Aggregates data for:
    * *TotalPlanItems*
    * *OverdueItems* (Planned Date < System Date)
    * *DueSoonItems* (Next 30 Days)
* `service_definition.abap`: Exposes the CDS views as an OData V4 service.

## 🚀 Key Features: Data Quality Engine
This project implements specific rules to ensure data trust:

| Severity | Rule ID | Description |
| :--- | :--- | :--- |
| 🔴 **ERROR** | `EQ04_ORPHAN_FLOC` | Equipment exists but is not installed in a Functional Location. |
| 🔴 **ERROR** | `FL03_SELF_PARENT` | Functional Location lists itself as its own parent (hierarchy loop). |
| 🟡 **WARN** | `PMI03_NO_END` | Maintenance Item missing `GLTRP` (Planned Finish Date). |
| 🔴 **ERROR** | `PMI04_ORPHAN_PLAN` | Plan item references a missing maintenance plan header. |

## 📊 Data Model
The solution is built on standard SAP PM tables:
* **Master Data:** `IFLOT` (Func Loc) ← `EQUI` (Equipment).
* **Planning:** `MPLA` (Header) ← `MPOS` (Item) ← `MHIS` (Cycles/Dates).
* **Execution:** `QMEL` (Notifications) and `AUFK` / `AFIH` (Orders).

## 💻 How to Run
1.  **Clone & Import:** Copy the `.abap` and `.cds` source codes into your SAP ADT (ABAP Development Tools) package.
2.  **Activate:** Activate Dictionary objects first, then Classes and Reports.
3.  **Execute Reports:**
    * Run `z_pm_asset_master_extract` to generate the Data Quality Scorecard.
    * Run `z_pm_compliance_kpi_recon` to view the KPI summary.
4.  **Test API:** Publish `ZUI_PM_COMPLIANCE_BIND` and test the OData stream in the browser or Postman.

## 👤 Author
**Yuvraj Singh**
*SAP Data & Analytics Engineer*
