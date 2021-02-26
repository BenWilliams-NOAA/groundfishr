SELECT NORPAC.DEBRIEFED_LENGTH_MV.YEAR,
NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA,
NORPAC.DEBRIEFED_LENGTH_MV.SPECIES,
NORPAC.DEBRIEFED_LENGTH_MV.LENGTH,
NORPAC.DEBRIEFED_LENGTH_MV.FREQUENCY,
NORPAC.DEBRIEFED_LENGTH_MV.PERFORMANCE
FROM NORPAC.DEBRIEFED_LENGTH_MV
WHERE NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA
        -- insert region
AND NORPAC.DEBRIEFED_LENGTH_MV.SPECIES
        -- insert species
