# SAP Asset Management (AMIS) Data Portfolio

**Prepared for:** Asset Data Coordinator Role (Department of Conservation)
**Focus:** Master Data Integrity, Process Support, and Asset Safety

## 🎯 Alignment with Job Requirements
This repository demonstrates technical competency in **SAP Enterprise Asset Management (EAM)**, directly supporting the accountabilities outlined in the DOC position description:

| DOC Accountability Area  | My Solution / Demonstration |
| :--- | :--- |
| **EAM Master Data Coordinator** | **`z_pm_asset_master_extract.abap`**<br>Automated validation of Functional Locations (`IFLOT`) and Equipment (`EQUI`) to ensure hierarchy completeness. |
| **System and Process Improvement** | **Data Quality Rules Engine**<br>Replaces manual error-checking with automated scripts that "triage" data faults instantly (e.g., finding orphan assets). |
| **Safety and Wellbeing** | **Risk-Based Flags**<br>Implemented logic to flag aging infrastructure (>20 years) to support safety audits and maintenance planning. |
| **Asset Management Co-ordination** | **Reporting Ready**<br>Extracts are structured for immediate export to Excel, facilitating easier reporting to stakeholders and management. |

## 🛠️ Technical Context
* **System:** SAP S/4HANA (Plant Maintenance / AMIS context)
* **Key Objects:**
    * **Functional Locations:** Spatial hierarchy (e.g., Parks, Tracks).
    * **Equipment:** Physical assets (e.g., Bridges, Huts, Traps).
    * **Maintenance Plans:** Scheduling of inspections.

## 📂 Project Structure

### 1. Master Data Validator (`z_pm_asset_master_extract.abap`)
* **Purpose:** Ensures the "Source of Truth" in SAP is accurate.
* **Key Feature:** Detects "Orphan Assets" (Equipment not installed in a Functional Location), which causes maintenance gaps.
* **Safety Check:** Flags assets with missing startup dates or extreme age, prioritizing them for review.

### 2. Compliance Reconciliation (`z_pm_compliance_kpi_recon.abap`)
* **Purpose:** Monitor if maintenance is actually happening on time.
* **Logic:** Compares **Planned Dates** (from Maintenance Plans) vs. **Actual Completion Dates** (from Work Orders).
* **Value:** Helps the Asset Specialist Manager identify teams that are falling behind on critical inspections.

### 3. Analytics Dashboard (CDS Views)
* **File:** `z_c_pm_compliance_kpis.cds`
* **Purpose:** A modern reporting layer that aggregates data by **Plant** (Region) or **Planner Group**.
* **Use Case:** Allows management to see a high-level "Health Check" of the asset register without running complex T-Codes.

## 🚀 How this supports the team
By automating the "boring" parts of data coordination (finding errors, checking fields), I can focus on:
1.  **Supporting field staff** with accurate data.
2.  **Triaging complex issues** rather than fixing typos.
3.  **Improving processes** to stop bad data from entering the system in the first place.
