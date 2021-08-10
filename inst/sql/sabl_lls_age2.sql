SELECT
  active,
  region,
  station_number,
  habitat_type
FROM afsc.stations_view
WHERE active = 1
