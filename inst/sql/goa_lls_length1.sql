SELECT AFSC.LENGTH_FREQUENCIES.YEAR,
AFSC.LENGTH_FREQUENCIES.COUNTRY,
AFSC.LENGTH_FREQUENCIES.VESSEL_NUMBER,
AFSC.LENGTH_FREQUENCIES.VESSEL_NAME,
AFSC.LENGTH_FREQUENCIES.CRUISE_NUMBER,
AFSC.LENGTH_FREQUENCIES.AREA_CODE,
AFSC.LENGTH_FREQUENCIES.GEOGRAPHIC_AREA_NAME,
AFSC.LENGTH_FREQUENCIES.COUNCIL_SABLEFISH_MGMT_AREA,
AFSC.LENGTH_FREQUENCIES.SPECIES_CODE,
AFSC.LENGTH_FREQUENCIES.COMMON_NAME,
AFSC.LENGTH_FREQUENCIES.SEX,
AFSC.LENGTH_FREQUENCIES.LENGTH,
AFSC.LENGTH_FREQUENCIES.LENGTH_FREQUENCY,
AFSC.LENGTH_FREQUENCIES.AKFIN_LOAD_DATE
FROM AFSC.LENGTH_FREQUENCIES
WHERE AFSC.LENGTH_FREQUENCIES.SPECIES_CODE
-- insert species
AND AFSC.LENGTH_FREQUENCIES.COUNTRY != 'Japan'
