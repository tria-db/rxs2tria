# example QWAmetadata files read and check against schemas

    Code
      check_QWAmetadata(x)
    Condition
      Warning:
      ! related missing optional columns:
      i description, url, and rel_ds_label
      Warning:
      ! sites missing optional columns:
      i aspect, slope, soil_water_capacity, soil_moisture, stand_age_str, and stand_age_class
      Warning:
      ! trees missing optional columns:
      i tree_sex, tree_latitude, tree_longitude, and tree_comment
      Warning:
      ! woodpieces missing optional columns:
      i sampling_year, sample_apex_distance, and pith_year
      Warning:
      ! slides missing optional columns:
      i pith_offset
    Message
      i supplementary: not provided, skipping.
    Condition
      Warning:
      ! roxas missing optional columns:
      i img_created_at, img_software, and rw_reference_file
    Message
      v All checks completed.

---

    Code
      check_QWAmetadata(x)
    Condition
      Warning:
      ! related missing optional columns:
      i description, url, and rel_ds_label
      Warning:
      ! sites missing optional columns:
      i aspect, slope, soil_water_capacity, soil_moisture, stand_age_str, and stand_age_class
      Warning:
      ! trees missing optional columns:
      i tree_sex, tree_latitude, tree_longitude, and tree_comment
      Warning:
      ! woodpieces missing optional columns:
      i sampling_year, sample_apex_distance, and pith_year
      Warning:
      ! slides missing optional columns:
      i pith_offset
    Message
      i supplementary: not provided, skipping.
      v All checks completed.

