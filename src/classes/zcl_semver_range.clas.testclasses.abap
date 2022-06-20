CLASS ltcl_semver_range DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      range_include FOR TESTING RAISING zcx_semver_error,
      range_intersect FOR TESTING RAISING zcx_semver_error,
      range_exclude FOR TESTING RAISING zcx_semver_error,
      range_parse FOR TESTING RAISING zcx_semver_error,
      empty_comparator FOR TESTING RAISING zcx_semver_error,
      create_from_comparator FOR TESTING RAISING zcx_semver_error,
      create_from_range FOR TESTING RAISING zcx_semver_error,
      strict_vs_loose FOR TESTING RAISING zcx_semver_error,
      to_string FOR TESTING RAISING zcx_semver_error.

ENDCLASS.

CLASS ltcl_semver_range IMPLEMENTATION.

  METHOD range_include.

    LOOP AT zcl_semver_fixtures=>range_include( ) INTO DATA(range_include).
      DATA(msg) = |{ range_include-range } { range_include-version } { range_include-loose } { range_include-incpre }|.
      DATA(r) = zcl_semver_range=>create(
        range  = range_include-range
        loose  = range_include-loose
        incpre = range_include-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = r->test( range_include-version )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD range_exclude.

  ENDMETHOD.

  METHOD range_intersect.

  ENDMETHOD.

  METHOD range_parse.

  ENDMETHOD.

  METHOD empty_comparator.

  ENDMETHOD.

  METHOD create_from_comparator.

  ENDMETHOD.

  METHOD create_from_range.

  ENDMETHOD.

  METHOD strict_vs_loose.

  ENDMETHOD.

  METHOD to_string.

    DATA(semrange) = zcl_semver_range=>create( '>= v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = semrange->to_string( )
      exp = '>=1.2.3' ).

  ENDMETHOD.

ENDCLASS.
