-- ----------------------------------------------------------------------
-- HANA Validation Queries - SAP PM/EAM (Portfolio)
-- ----------------------------------------------------------------------
-- These queries mirror the data quality rules described in docs.
-- Adjust schema/table names to your system (S/4HANA vs ECC).

-- 1) Functional locations missing description
SELECT COUNT(*) AS missing_floc_desc
FROM IFLOT
WHERE PLTXT IS NULL OR PLTXT = '';

-- 2) Equipment missing description
SELECT COUNT(*) AS missing_equipment_desc
FROM EQUI
WHERE EQKTX IS NULL OR EQKTX = '';

-- 3) Equipment referencing unknown functional location
SELECT COUNT(*) AS orphan_equipment_floc
FROM EQUI e
LEFT JOIN IFLOT f
  ON e.TPLNR = f.TPLNR
WHERE e.TPLNR IS NOT NULL
  AND f.TPLNR IS NULL;

-- 4) Plan items missing both FL and EQ
SELECT COUNT(*) AS plan_items_missing_object
FROM MPOS
WHERE (TPLNR IS NULL OR TPLNR = '')
  AND (EQUNR IS NULL OR EQUNR = '');

-- 5) Overdue maintenance items
SELECT COUNT(*) AS overdue_plan_items
FROM MPOS
WHERE GLTRP IS NOT NULL
  AND GLTRP < CURRENT_DATE;
