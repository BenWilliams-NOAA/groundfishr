"SELECT
  year,
  haul_join,
  vessel,
  cruise,
  ifq,
  gear_description,
  performance,
  fmp_area,
  fmp_subarea,
  catch_203,
  total_hook_pots,
  bottom_depth_fathoms,
  haul_date,
  latdd_end,
  londd_end
FROM AFSC.NORPAC_CATCH_HOOK_MAMMAL_V
WHERE gear_description = 'LONGLINER'
AND ifq = 'Y'
AND performance IN (1, 8, 10)
AND year >= 1995
AND fmp_area IS NOT NULL
AND fmp_subarea NOT IN ('SEI', 'PWSI')
AND catch_203 IS NOT NULL
AND bottom_depth_fathoms >= 25
        AND bottom_depth_fathoms <= 1200
