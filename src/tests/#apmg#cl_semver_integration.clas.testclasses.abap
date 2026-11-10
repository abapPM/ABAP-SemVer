CLASS ltcl_semver_whitespace DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA:
      ws_medium            TYPE string,
      ws_large             TYPE string,
      zero_large           TYPE string,
      version_prefix_large TYPE string.

    METHODS:
      setup,
      range_with_whitespace FOR TESTING RAISING /apmg/cx_error,
      range_with_version_prefix FOR TESTING RAISING /apmg/cx_error,
      range_with_0 FOR TESTING RAISING /apmg/cx_error,
      semver_version FOR TESTING RAISING /apmg/cx_error,
      comparator FOR TESTING RAISING /apmg/cx_error.

ENDCLASS.

CLASS ltcl_semver_whitespace IMPLEMENTATION.

  METHOD setup.
    CONSTANTS occ TYPE i VALUE 50000.

    ws_medium = repeat(
      val = ` `
      occ = 125 ).
    ws_large = repeat(
      val = ` `
      occ = occ ).
    zero_large = repeat(
      val = `0`
      occ = occ ). "
    version_prefix_large = repeat(
      val = `v`
      occ = occ ).
  ENDMETHOD.

  METHOD range_with_whitespace.

    " a range with these extra characters would take a few minutes to process if
    " any redos susceptible regexes were used (in JavaScript). ABAP does not
    " seem to have this problem but we will include the tests anyway.

    DATA(r) = |1.2.3 { ws_large } <1.3.0|.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_range=>create( r )->range( )
      exp = '1.2.3 <1.3.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>valid_range( r )
      exp = '1.2.3 <1.3.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>min_version( r )->version
      exp = '1.2.3' ).

    DATA(t) = VALUE string_table( ( `1.2.3` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>min_satisfying( versions = t range = r )
      exp = '1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>max_satisfying( versions = t range = r )
      exp = '1.2.3' ).

    TRY.
        /apmg/cl_semver_range=>create( '> invalid' ).
        cl_abap_unit_assert=>fail( ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD range_with_version_prefix.

    TRY.
        /apmg/cl_semver_range=>create( version_prefix_large ).
        cl_abap_unit_assert=>fail( ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>valid_range( version_prefix_large )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_functions=>satisfies( version = '1.2.3' range = version_prefix_large )
      exp = abap_false ).

    TRY.
        /apmg/cl_semver_ranges=>min_version( version_prefix_large ).
        cl_abap_unit_assert=>fail( ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

    DATA(t) = VALUE string_table( ( `1.2.3` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>min_satisfying( versions = t range = version_prefix_large )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>max_satisfying( versions = t range = version_prefix_large )
      exp = '' ).

    DATA(r) = |>= { version_prefix_large }1|.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_range=>create( r )->range( )
      exp = '>=1.0.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>valid_range( r )
      exp = '>=1.0.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_functions=>satisfies( version = '1.2.3' range = r )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>min_version( r )->version
      exp = '1.0.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>min_satisfying( versions = t range = r )
      exp = '1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>max_satisfying( versions = t range = r )
      exp = '1.2.3' ).

  ENDMETHOD.

  METHOD range_with_0.

    DATA(r) = |1.2.3 { zero_large } <1.3.0|.

    TRY.
        /apmg/cl_semver_range=>create( r ).
        cl_abap_unit_assert=>fail( ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>valid_range( r )
      exp = '' ).

    TRY.
        /apmg/cl_semver_ranges=>min_version( r ).
        cl_abap_unit_assert=>fail( ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

    DATA(t) = VALUE string_table( ( `1.2.3` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>min_satisfying( versions = t range = r )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>max_satisfying( versions = t range = r )
      exp = '' ).

  ENDMETHOD.

  METHOD semver_version.

    DATA(v) = |{ ws_medium }1.2.3{ ws_medium }|.
    DATA(too_long) = |{ ws_large }1.2.3{ ws_large }|.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver=>create( v )->version
      exp = '1.2.3' ).

    TRY.
        /apmg/cl_semver=>create( too_long ).
        cl_abap_unit_assert=>fail( ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD comparator.

    DATA(c) = |{ ws_large }<{ ws_large }1.2.3{ ws_large }|.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_comparator=>create( c )->value
      exp = '<1.2.3' ).

  ENDMETHOD.
ENDCLASS.
