CLASS zcl_semver_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS is_numeric
      IMPORTING
        !a            TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_semver_utils IMPLEMENTATION.


  METHOD is_numeric.

    TRY.
        result = xsdbool( |{ a }| CO '0123456789' ).
      CATCH cx_root.
        " can't be converted to string/numeric
        result = abap_false.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
