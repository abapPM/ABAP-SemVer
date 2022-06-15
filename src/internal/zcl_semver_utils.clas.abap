CLASS zcl_semver_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS is_numeric
      IMPORTING
        !data         TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS trim
      IMPORTING
        !data         TYPE clike
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_semver_utils IMPLEMENTATION.


  METHOD is_numeric.
    " Unsigned number (could be bigger than int4 or even int8)

    TRY.
        result = xsdbool( |{ data }| CO '0123456789' ).
      CATCH cx_root.
        " can't be converted to string/numeric
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD trim.
    " Trim tab, cr, lf and spaces Like JavaScript trim

    result = condense( val = data del = | \t\n\r| ).

  ENDMETHOD.
ENDCLASS.
