INTERFACE zif_semver_definitions PUBLIC.

  TYPES:
    BEGIN OF ty_options,
      loose  TYPE abap_bool,
      incpre TYPE abap_bool,
      rtl    TYPE abap_bool,
    END OF ty_options.

ENDINTERFACE.
