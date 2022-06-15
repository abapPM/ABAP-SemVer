CLASS ltcl_semver_re DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_semver_re.

    METHODS:
      test_src FOR TESTING,
      test_regex FOR TESTING,
      test_occ FOR TESTING.

ENDCLASS.

CLASS zcl_semver_re DEFINITION LOCAL FRIENDS ltcl_semver_re.

CLASS ltcl_semver_re IMPLEMENTATION.

  METHOD test_src.
    " has a list of src

    DATA i TYPE i.

    FIELD-SYMBOLS <token> TYPE zcl_semver_re=>ty_token.

    DO.
      i += 1.
      ASSIGN COMPONENT i OF STRUCTURE zcl_semver_re=>token TO <token>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      cl_abap_unit_assert=>assert_not_initial(
        act = <token>-src
        msg = |Regex component #{ i } must not be initial| ).
    ENDDO.

  ENDMETHOD.

  METHOD test_regex.
    " has a list of valid regex

    DATA i TYPE i.
    DATA regex TYPE REF TO cl_abap_regex.

    FIELD-SYMBOLS <token> TYPE zcl_semver_re=>ty_token.

    DO.
      i += 1.
      ASSIGN COMPONENT i OF STRUCTURE zcl_semver_re=>token TO <token>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      TRY.
          regex = <token>-regex.
          DATA(matcher) = regex->create_matcher( text = '1.2.3' ).
        CATCH cx_root.
          cl_abap_unit_assert=>fail(
            msg = |Error processing regex component #{ i }| ).
      ENDTRY.
    ENDDO.

  ENDMETHOD.

  METHOD test_occ.
    " either 0 or 1

    DATA i TYPE i.

    FIELD-SYMBOLS <token> TYPE zcl_semver_re=>ty_token.

    DO.
      i += 1.
      ASSIGN COMPONENT i OF STRUCTURE zcl_semver_re=>token TO <token>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      cl_abap_unit_assert=>assert_number_between(
        number = <token>-occ
        lower  = 0
        upper  = 1
        msg    = |Occurence of component #{ i } must be either 0 or 1| ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.
