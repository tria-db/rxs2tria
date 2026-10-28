# example QWAmetadata files read and check against schemas

    Code
      check_QWAmetadata(x)
    Message
      i related missing optional columns:
      i description, url, and rel_ds_label
      i sites missing optional columns:
      i aspect, slope, soil_water_capacity, soil_moisture, stand_age_str, and stand_age_class
      i trees missing optional columns:
      i tree_sex, tree_latitude, tree_longitude, and tree_comment
      i woodpieces missing optional columns:
      i sampling_year, sample_apex_distance, and pith_year
      i slides missing optional columns:
      i pith_offset
      i roxas missing optional columns:
      i only_ew, img_created_at, img_software, img_comment, and rw_reference_file
      v <QWAimages>: all checks passed.
      v <QWAmetadata>: all checks passed.

---

    Code
      check_QWAmetadata(x)
    Message
      i related missing optional columns:
      i description, url, and rel_ds_label
      i sites missing optional columns:
      i aspect, slope, soil_water_capacity, soil_moisture, stand_age_str, and stand_age_class
      i trees missing optional columns:
      i tree_sex, tree_latitude, tree_longitude, and tree_comment
      i woodpieces missing optional columns:
      i sampling_year, sample_apex_distance, and pith_year
      i slides missing optional columns:
      i pith_offset
      i roxas_ai missing optional columns:
      i only_ew and img_comment
      i roxas_ai columns with empty values:
        `rings_segmentation_model` (5 rows)
        `rings_segmentation_datetime` (5 rows)
        `cells_segmentation_model` (5 rows)
        `cells_segmentation_datetime` (5 rows)
        `reference_series` (33 rows)
      v <QWAimages>: all checks passed.
      v <QWAmetadata>: all checks passed.

