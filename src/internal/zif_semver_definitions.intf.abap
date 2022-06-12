INTERFACE zif_semver_definitions PUBLIC.

  CONSTANTS version TYPE string VALUE '1.0.0' ##NEEDED.

  TYPES:
    BEGIN OF ty_options,
      loose  TYPE abap_bool,
      incpre TYPE abap_bool,
      rtl    TYPE abap_bool,
    END OF ty_options.

ENDINTERFACE.
