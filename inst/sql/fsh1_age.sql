SELECT YEAR,
FMP_AREA,
SPECIES,
LENGTH,
WEIGHT,
AGE,
SPECIMEN_TYPE,
PERFORMANCE
FROM NORPAC.DEBRIEFED_AGE_MV
WHERE AREA
        -- insert region
AND SPECIES
        -- insert species
