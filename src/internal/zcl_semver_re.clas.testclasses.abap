CLASS ltcl_semver_re DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_semver_re.

    METHODS:
      test_src FOR TESTING,
      test_regex FOR TESTING.

ENDCLASS.

CLASS zcl_semver_re DEFINITION LOCAL FRIENDS ltcl_semver_re.

CLASS ltcl_semver_re IMPLEMENTATION.

  METHOD test_src.
    " has a list of src

    DATA i TYPE i.

    DO.
      i += 1.
      ASSIGN COMPONENT i OF STRUCTURE zcl_semver_re=>src TO FIELD-SYMBOL(<src>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      cl_abap_unit_assert=>assert_not_initial(
        act = <src>
        msg = |Regex component #{ i } must not be initial| ).
    ENDDO.

  ENDMETHOD.

  METHOD test_regex.
    " has a list of valid regex

    DATA i TYPE i.
    DATA regex TYPE REF TO cl_abap_regex.

    DO.
      i += 1.
      ASSIGN COMPONENT i OF STRUCTURE zcl_semver_re=>re TO FIELD-SYMBOL(<regex>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      TRY.
          regex = <regex>.
          DATA(matcher) = regex->create_matcher( text = '1.2.3' ).
        CATCH cx_root.
          cl_abap_unit_assert=>fail(
            msg = |Error processing regex component #{ i }| ).
      ENDTRY.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
