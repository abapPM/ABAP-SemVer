CLASS ltcl_semver_utils DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      is_numeric FOR TESTING,
      trim FOR TESTING,
      unicode_whitespace FOR TESTING RAISING /apmg/cx_error,
      version_trim FOR TESTING.

ENDCLASS.

CLASS ltcl_semver_utils IMPLEMENTATION.

  METHOD is_numeric.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>is_numeric( '26111968' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>is_numeric( 26111968 )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>is_numeric( '+123' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>is_numeric( 'abc' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>is_numeric( -123 )
      exp = abap_false ).

  ENDMETHOD.

  METHOD trim.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>trim( |  test   | )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>trim( |\t\t test| )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>trim( |test\n \t| )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>trim( |test| )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>trim( |test   test| )
      exp = 'test test' ).

  ENDMETHOD.

  METHOD version_trim.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>version_trim( | v 1.2.3| )
      exp = 'v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_utils=>version_trim( |=  \t 1.2.3 | )
      exp = '=1.2.3' ).

  ENDMETHOD.

  METHOD unicode_whitespace.
    DATA(characters) = VALUE string_table(
      ( CONV string( cl_abap_conv_in_ce=>uccp( '00A0' ) ) )
      ( CONV string( cl_abap_conv_in_ce=>uccp( '0085' ) ) ) ). " JS tests FEFF but ABAP PCRE does not support that
    LOOP AT characters INTO DATA(character).
      cl_abap_unit_assert=>assert_equals(
        act = /apmg/cl_semver_utils=>trim( character && `test` && character && `test` && character )
        exp = 'test test' ).

      DATA(input) = `1.2.3-dev` && character && `=1.2.3-dev`.
      cl_abap_unit_assert=>assert_equals(
        act = /apmg/cl_semver_range=>create( input )->range( )
        exp = '1.2.3-dev' ).
      cl_abap_unit_assert=>assert_equals(
        act = /apmg/cl_semver_ranges=>valid_range( input )
        exp = '1.2.3-dev' ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
