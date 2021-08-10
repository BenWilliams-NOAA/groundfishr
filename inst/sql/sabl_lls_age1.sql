SELECT
  year,
  station_number,
  startlat,
  startlong,
  sex,
  age,
  length,
  weight,
  age_notes
FROM afsc.age_view
WHERE country != 'Japan'
AND length > 0
AND length > 0
AND year = 2020
AND age_notes is null
