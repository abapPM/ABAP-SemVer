CLASS ltcl_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

* https://github.com/npm/node-semver/tree/main/test/functions
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_test,
        range   TYPE string,
        version TYPE string,
        loose   TYPE abap_bool,
      END OF ty_test,
      ty_tests TYPE STANDARD TABLE OF ty_test WITH DEFAULT KEY.

    METHODS:
      test_clean FOR TESTING RAISING zcx_semver_error,
      test_cmp_invalid FOR TESTING RAISING zcx_semver_error,
      test_cmp_comparison FOR TESTING RAISING zcx_semver_error,
      test_cmp_equality FOR TESTING RAISING zcx_semver_error,
      test_coerce_to_null FOR TESTING RAISING zcx_semver_error,
      test_coerce_to_valid FOR TESTING RAISING zcx_semver_error,
      test_compare FOR TESTING RAISING zcx_semver_error,
      test_compare_build FOR TESTING RAISING zcx_semver_error,
      test_compare_loose FOR TESTING RAISING zcx_semver_error,
      test_diff FOR TESTING RAISING zcx_semver_error,
      test_eq FOR TESTING RAISING zcx_semver_error,
      test_gt FOR TESTING RAISING zcx_semver_error,
      test_gte FOR TESTING RAISING zcx_semver_error,
      test_inc FOR TESTING RAISING zcx_semver_error,
      test_lt FOR TESTING RAISING zcx_semver_error,
      test_lte FOR TESTING RAISING zcx_semver_error,
      test_major FOR TESTING RAISING zcx_semver_error,
      test_minor FOR TESTING RAISING zcx_semver_error,
      test_neq FOR TESTING RAISING zcx_semver_error,
      test_parse FOR TESTING RAISING zcx_semver_error,
      test_patch FOR TESTING RAISING zcx_semver_error,
      test_prerelease FOR TESTING RAISING zcx_semver_error,
      test_rcompare FOR TESTING RAISING zcx_semver_error,
      test_rsort FOR TESTING RAISING zcx_semver_error,
      test_satisfies FOR TESTING RAISING zcx_semver_error,
      test_sort FOR TESTING RAISING zcx_semver_error,
      test_valid FOR TESTING RAISING zcx_semver_error.

ENDCLASS.

CLASS ltcl_tests IMPLEMENTATION.

  METHOD test_clean.
    " Version should be detectable despite extra characters

    DATA(tests) = VALUE ty_tests(
      ( range = '1.2.3' version = '1.2.3' )
      ( range = ' 1.2.3 ' version = '1.2.3' )
      ( range = ' 1.2.3-4 ' version = '1.2.3-4' )
      ( range = ' 1.2.3-pre ' version = '1.2.3-pre' )
      ( range = '  =v1.2.3   ' version = '1.2.3' )
      ( range = 'v1.2.3' version = '1.2.3' )
      ( range = ' v1.2.3 ' version = '1.2.3' )
      ( range = |\t1.2.3| version = '1.2.3' )
      ( range = '>1.2.3' version = '' )
      ( range = '~1.2.3' version = '' )
      ( range = '<=1.2.3' version = '' )
      ( range = '1.2.x' version = '' ) ).

    LOOP AT tests INTO DATA(test).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>clean( test-range )
        exp = test-version
        msg = |{ test-range } { test-version }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_cmp_invalid.

    TRY.
        zcl_semver_functions=>cmp( a = '1.2.3' op = 'a frog' b = '4.5.6' ).
        cl_abap_unit_assert=>fail( msg = 'invalid cmp usage' ).
      CATCH zcx_semver_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD test_cmp_comparison.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = comparison-v0 op = '>' b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg && '>' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = comparison-v1 op = '<' b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg && '<' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = comparison-v1 op = '>' b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg && '>' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = comparison-v0 op = '<' b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg && '<' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = comparison-v1 op = '==' b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg && '==' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = comparison-v0 op = '>=' b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg && '>=' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = comparison-v1 op = '<=' b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg && '<=' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = comparison-v0 op = '!=' b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg && '!=' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_cmp_equality.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      DATA(msg) = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = equality-v0 op = '' b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg && 'nop' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = equality-v0 op = '=' b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg && '=' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = equality-v0 op = '==' b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg && '==' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = equality-v0 op = '!=' b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg && '!=' ).

      " also test with an object. they are === because obj.version matches
      DATA(semver_v0) = zcl_semver=>create( version = equality-v0 loose = equality-loose ).
      DATA(semver_v1) = zcl_semver=>create( version = equality-v1 loose = equality-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = semver_v0 op = '===' b = semver_v1 loose = equality-loose )
        exp = abap_true
        msg = msg && '===' ).

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>cmp( a = semver_v0 op = '!==' b = semver_v1 loose = equality-loose )
        exp = abap_false
        msg = msg && '!==' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_coerce_to_null.

    DATA(tests) = VALUE string_table(
      ( `` )
      ( `.` )
      ( `version one` )
      ( |{ repeat( val = '9' occ = 16 ) }| )
      ( |{ repeat( val = '1' occ = 17 ) }| )
      ( |a{ repeat( val = '9' occ = 16 ) }| )
      ( |a{ repeat( val = '1' occ = 17 ) }| )
      ( |{ repeat( val = '9' occ = 16 ) }a| )
      ( |{ repeat( val = '1' occ = 17 ) }a| )
      ( |{ repeat( val = '9' occ = 16 ) }.4.7.4| )
      ( |{ repeat( val = '9' occ = 16 ) }.{ repeat( val = '2' occ = 16 ) }.{ repeat( val = '3' occ = 16 ) }| )
      ( |{ repeat( val = '1' occ = 16 ) }.{ repeat( val = '9' occ = 16 ) }.{ repeat( val = '3' occ = 16 ) }| )
      ( |{ repeat( val = '1' occ = 16 ) }.{ repeat( val = '2' occ = 16 ) }.{ repeat( val = '9' occ = 16 ) }| ) ).

    LOOP AT tests INTO DATA(test).
      cl_abap_unit_assert=>assert_not_bound(
        act = zcl_semver_functions=>coerce( test )
        msg = |{ test }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_coerce_to_valid.

    TYPES:
      BEGIN OF ty_test,
        version TYPE string,
        res     TYPE string,
        rtl     TYPE abap_bool,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( version = '.1' res = '1.0.0' )
      ( version = '.1.' res = '1.0.0' )
*      ( version = '..1' res = '1.0.0' )
      ( version = '.1.1' res = '1.1.0' )
      ( version = '1.' res = '1.0.0' )
      ( version = '1.0' res = '1.0.0' )
      ( version = '1.0.0' res = '1.0.0' )
      ( version = '0' res = '0.0.0' )
      ( version = '0.0' res = '0.0.0' )
      ( version = '0.0.0' res = '0.0.0' )
      ( version = '0.1' res = '0.1.0' )
      ( version = '0.0.1' res = '0.0.1' )
      ( version = '0.1.1' res = '0.1.1' )
      ( version = '1' res = '1.0.0' )
      ( version = '1.2' res = '1.2.0' )
      ( version = '1.2.3' res = '1.2.3' )
*      ( version = '1.2.3.4' res = '1.2.3' )
      ( version = '13' res = '13.0.0' )
      ( version = '35.12' res = '35.12.0' )
      ( version = '35.12.18' res = '35.12.18' )
*      ( version = '35.12.18.24' res = '35.12.18' )
      ( version = 'v1' res = '1.0.0' )
      ( version = 'v1.2' res = '1.2.0' )
      ( version = 'v1.2.3' res = '1.2.3' )
*      ( version = 'v1.2.3.4' res = '1.2.3' )
      ( version = ' 1' res = '1.0.0' )
      ( version = '1 ' res = '1.0.0' )
*      ( version = '1 0' res = '1.0.0' )
*      ( version = '1 1' res = '1.0.0' )
*      ( version = '1.1 1' res = '1.1.0' )
*      ( version = '1.1-1' res = '1.1.0' )
*      ( version = '1.1-1' res = '1.1.0' )
      ( version = 'a1' res = '1.0.0' )
      ( version = 'a1a' res = '1.0.0' )
      ( version = '1a' res = '1.0.0' )
*      ( version = 'version 1' res = '1.0.0' )
*      ( version = 'version1' res = '1.0.0' )
*      ( version = 'version1.0' res = '1.0.0' )
*      ( version = 'version1.1' res = '1.1.0' )
*      ( version = '42.6.7.9.3-alpha' res = '42.6.7' )
      ( version = 'v2' res = '2.0.0' )
*      ( version = 'v3.4 replaces v3.3.1' res = '3.4.0' )
*      ( version = '4.6.3.9.2-alpha2' res = '4.6.3' )
*      ( version = |{ repeat( val = '1' occ = 17 ) }.2| res = '2.0.0' )
*      ( version = |{ repeat( val = '1' occ = 17 ) }.2.3| res = '2.3.0' )
*      ( version = |1.{ repeat( val = '2' occ = 17 ) }.3| res = '1.0.0' )
*      ( version = |1.2.{ repeat( val = '3' occ = 17 ) }| res = '1.2.0' )
*      ( version = |{ repeat( val = '1' occ = 17 ) }.2.3.4| res = '2.3.4' )
*      ( version = |1.{ repeat( val = '2' occ = 17 ) }.3.4| res = '1.0.0' )
*      ( version = |1.2.{ repeat( val = '3' occ = 17 ) }.4| res = '1.2.0' )
*      ( version = |{ repeat( val = '1' occ = 17 ) }.{ repeat( val = '2' occ = 16 ) }.{ repeat( val = '3' occ = 16 ) }| res = |{ repeat( val = '2' occ = 16 ) }.{ repeat( val = '3' occ = 16 ) }.0| )
*      ( version = |{ repeat( val = '1' occ = 16 ) }.{ repeat( val = '2' occ = 17 ) }.{ repeat( val = '3' occ = 16 ) }| res = |{ repeat( val = '1' occ = 16 ) }.0.0| )
*      ( version = |{ repeat( val = '1' occ = 16 ) }.{ repeat( val = '2' occ = 16 ) }.{ repeat( val = '3' occ = 17 ) }| res = |{ repeat( val = '1' occ = 16 ) }.{ repeat( val = '2' occ = 16 ) }.0| )
*      ( version = |11{ repeat( val = '.1' occ = 126 ) }| res = '11.1.1' )
*      ( version = repeat( val = '1' occ = 16 ) res = |{ repeat( val = '1' occ = 16 ) }.0.0| )
*      ( version = |a{ repeat( val = '1' occ = 16 ) }| res = |{ repeat( val = '1' occ = 16 ) }.0.0| )
*      ( version = |{ repeat( val = '1' occ = 16 ) }.2.3.4| res = |{ repeat( val = '1' occ = 16 ) }.2.3| )
*      ( version = |1.{ repeat( val = '2' occ = 16 ) }.3.4| res = |1.{ repeat( val = '2' occ = 16 ) }.3| )
*      ( version = |1.2.{ repeat( val = '3' occ = 16 ) }.4| res = |1.2.{ repeat( val = '3' occ = 16 ) }| )
*      ( version = |{ repeat( val = '1' occ = 16 ) }.{ repeat( val = '2' occ = 16 ) }.{ repeat( val = '3' occ = 16 ) }| res = |{ repeat( val = '1' occ = 16 ) }.{ repeat( val = '2' occ = 16 ) }.{ repeat( val = '3' occ = 16 ) }| )
*      ( version = |1.2.3.{ repeat( val = '4' occ = 252 ) }.5| res = '1.2.3' )
*      ( version = |1.2.3.{ repeat( val = '4' occ = 1024 ) }| res = '1.2.3' )
*      ( version = |{ repeat( val = '1' occ = 17 ) }.4.7.4| res = '4.7.4' )
      ( version = 10 res = '10.0.0' ) ).
*      ( version = '1.2.3/a/b/c/2.3.4' res = '2.3.4' rtl = abap_true )
*      ( version = '1.2.3.4.5.6' res = '4.5.6' rtl = abap_true )
*      ( version = '1.2.3.4.5/6' res = '6.0.0' rtl = abap_true )
*      ( version = '1.2.3.4./6' res = '6.0.0' rtl = abap_true )
*      ( version = '1.2.3.4/6' res = '6.0.0' rtl = abap_true )
*      ( version = '1.2.3./6' res = '6.0.0' rtl = abap_true )
*      ( version = '1.2.3/6' res = '6.0.0' rtl = abap_true )
*      ( version = '1.2.3.4' res = '2.3.4' rtl = abap_true )
*      ( version = '1.2.3.4xyz' res = '2.3.4' rtl = abap_true ) ).

    LOOP AT tests INTO DATA(test).
      DATA(semver) = zcl_semver_functions=>coerce( version = test-version rtl = test-rtl ).

      cl_abap_unit_assert=>assert_bound(
        act = semver
        msg = |{ test-version } { test-res } { test-rtl }| ).

      cl_abap_unit_assert=>assert_equals(
        act = semver->version
        exp = test-res
        msg = |{ test-version } { test-res } { test-rtl }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_compare.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = +1
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = -1
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = 0
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = 0
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = 0
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = 0
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare( a = equality-v0 b = equality-v0 loose = equality-loose )
        exp = 0
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare( a = equality-v1 b = equality-v1 loose = equality-loose )
        exp = 0
        msg = msg ).

      " also test with an object. they are === because obj.version matches
      DATA(semver_v0) = zcl_semver=>create( version = equality-v0 loose = equality-loose ).
      DATA(semver_v1) = zcl_semver=>create( version = equality-v1 loose = equality-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare( a = semver_v0 b = semver_v1 loose = equality-loose )
        exp = 0
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_compare_build.

    DATA(nobuild) = '1.0.0'.
    DATA(build0) = '1.0.0+0'.
    DATA(build1) = '1.0.0+1'.
    DATA(build10) = '1.0.0+1.0'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>compare_build( a = nobuild b = build0 )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>compare_build( a = build0 b = build0 )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>compare_build( a = build0 b = nobuild )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>compare_build( a = build0 b = '1.0.0+0.0' )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>compare_build( a = build0 b = build1 )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>compare_build( a = build1 b = build0 )
      exp = +1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>compare_build( a = build10 b = build1 )
      exp = +1 ).

  ENDMETHOD.

  METHOD test_compare_loose.
    " strict vs loose version numbers

    TYPES:
      BEGIN OF ty_test,
        loose  TYPE string,
        strict TYPE string,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( loose = '=1.2.3' strict = '1.2.3' )
      ( loose = '01.02.03' strict = '1.2.3' )
      ( loose = '1.2.3-beta.01' strict = '1.2.3-beta.1' )
      ( loose = '   =1.2.3' strict = '1.2.3' )
      ( loose = '1.2.3foo' strict = '1.2.3-foo' ) ).

    LOOP AT tests INTO DATA(test).

      TRY.
          DATA(semver) = zcl_semver=>create( test-loose ).
          cl_abap_unit_assert=>fail( ).
        CATCH zcx_semver_error ##NO_HANDLER.
      ENDTRY.

      semver = zcl_semver=>create( version = test-loose loose = abap_true ).

      cl_abap_unit_assert=>assert_equals(
        act = semver->version
        exp = test-strict ).

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>eq( a = test-loose b = test-strict loose = abap_true )
        exp = abap_true ).

      TRY.
          cl_abap_unit_assert=>assert_equals(
            act = zcl_semver_functions=>eq( a = test-loose b = test-strict )
            exp = abap_true ).

          cl_abap_unit_assert=>fail( ).
        CATCH zcx_semver_error ##NO_HANDLER.
      ENDTRY.

      TRY.
          semver = zcl_semver=>create( test-strict ).
          semver->compare( test-loose ).
          cl_abap_unit_assert=>fail( ).
        CATCH zcx_semver_error ##NO_HANDLER.
      ENDTRY.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>compare_loose( a = test-loose b = test-strict )
        exp = 0 ).

    ENDLOOP.

  ENDMETHOD.

  METHOD test_diff.

    TYPES:
      BEGIN OF ty_test,
        v1  TYPE string,
        v2  TYPE string,
        res TYPE string,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( v1 = '1.2.3' v2 = '0.2.3' res = 'major' )
      ( v1 = '1.4.5' v2 = '0.2.3' res = 'major' )
      ( v1 = '1.2.3' v2 = '2.0.0-pre' res = 'premajor' )
      ( v1 = '1.2.3' v2 = '1.3.3' res = 'minor' )
      ( v1 = '1.0.1' v2 = '1.1.0-pre' res = 'preminor' )
      ( v1 = '1.2.3' v2 = '1.2.4' res = 'patch' )
      ( v1 = '1.2.3' v2 = '1.2.4-pre' res = 'prepatch' )
      ( v1 = '0.0.1' v2 = '0.0.1-pre' res = 'prerelease' )
      ( v1 = '0.0.1' v2 = '0.0.1-pre-2' res = 'prerelease' )
      ( v1 = '1.1.0' v2 = '1.1.0-pre' res = 'prerelease' )
      ( v1 = '1.1.0-pre-1' v2 = '1.1.0-pre-2' res = 'prerelease' )
      ( v1 = '1.0.0' v2 = '1.0.0' res = '' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-v1 } { test-v2 } { test-res } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>diff( version_1 = test-v1 version_2 = test-v2 )
        exp = test-res ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_eq.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>eq( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>eq( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>eq( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>eq( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>eq( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>eq( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>eq( a = equality-v0 b = equality-v0 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>eq( a = equality-v1 b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_gt.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gt( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gt( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gt( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gt( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gt( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gt( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_gte.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gte( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gte( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gte( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gte( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gte( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>gte( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_inc.

    LOOP AT zcl_semver_fixtures=>increments( ) INTO DATA(increments).
      DATA(msg) = |{ increments-version } { increments-release } { increments-identifier } { increments-res } |.

      DATA(s) = zcl_semver_functions=>inc(
                  version    = increments-version
                  release    = increments-release
                  identifier = increments-identifier
                  loose      = increments-loose
                  incpre     = increments-incpre ).

      IF s IS BOUND.
        cl_abap_unit_assert=>assert_equals(
          act = s->version
          exp = increments-res
          msg = msg ).
      ELSE.
        cl_abap_unit_assert=>assert_equals(
          act = ''
          exp = increments-res
          msg = msg ).
      ENDIF.

      DATA(parsed) = zcl_semver_functions=>parse(
                       version = increments-version
                       loose   = increments-loose
                       incpre  = increments-incpre ).

      DATA(parsed_input) = zcl_semver_functions=>parse(
                             version = increments-version
                             loose   = increments-loose
                             incpre  = increments-incpre ).

      IF increments-res IS NOT INITIAL.

        parsed->inc( release = increments-release identifier = increments-identifier ).

        cl_abap_unit_assert=>assert_equals(
          act = parsed->version
          exp = increments-res
          msg = msg && 'version' ).

        DATA(pre_inc) = parsed_input->version.

        zcl_semver_functions=>inc(
          version    = parsed_input
          release    = increments-release
          identifier = increments-identifier
          loose      = increments-loose
          incpre     = increments-incpre ).

        DATA(post_inc) = parsed_input->version.

        cl_abap_unit_assert=>assert_equals(
          act = pre_inc
          exp = post_inc
          msg = msg && 'must not modify its input' ).

      ELSEIF parsed IS NOT INITIAL.

        TRY.
            parsed->inc( release = increments-release identifier = increments-identifier ).
            cl_abap_unit_assert=>fail( ).
          CATCH zcx_semver_error ##NO_HANDLER.
        ENDTRY.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD test_lt.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lt( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lt( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lt( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lt( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lt( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lt( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_lte.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lte( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lte( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lte( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lte( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lte( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>lte( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_major.
    " Version should be detectable despite extra characters

    DATA(tests) = VALUE ty_tests(
      ( range = '1.2.3' version = 1 )
      ( range = ' 1.2.3 ' version = 1 )
      ( range = ' 2.2.3-4 ' version = 2 )
      ( range = ' 3.2.3-pre ' version = 3 )
      ( range = 'v5.2.3' version = 5 )
      ( range = ' v8.2.3 ' version = 8 )
      ( range = |\t13.2.3| version = 13 )
      ( range = '=21.2.3' version = 21 loose = abap_true )
      ( range = 'v=34.2.3' version = 34 loose = abap_true ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-range } { test-loose } { test-version } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>major( version = test-range loose = test-loose )
        exp = condense( test-version )
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_minor.
    " Version should be detectable despite extra characters

    DATA(tests) = VALUE ty_tests(
      ( range = '1.1.3' version = 1 )
      ( range = ' 1.1.3 ' version = 1 )
      ( range = ' 1.2.3-4 ' version = 2 )
      ( range = ' 1.3.3-pre ' version = 3 )
      ( range = 'v1.5.3' version = 5 )
      ( range = ' v1.8.3 ' version = 8 )
      ( range = |\t1.13.3| version = 13 )
      ( range = '=1.21.3' version = 21 loose = abap_true )
      ( range = 'v=1.34.3' version = 34 loose = abap_true ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-range } { test-loose } { test-version } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>minor( version = test-range loose = test-loose )
        exp = condense( test-version )
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_neq.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>neq( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>neq( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>neq( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>neq( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>neq( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>neq( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>neq( a = equality-v0 b = equality-v0 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>neq( a = equality-v1 b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_parse.
    " returns null instead of throwing when presented with garbage

    LOOP AT zcl_semver_fixtures=>invalid_versions( ) INTO DATA(invalid_version).
      DATA(msg) = |{ invalid_version-value } { invalid_version-loose } { invalid_version-reason } |.

      cl_abap_unit_assert=>assert_initial(
        act = zcl_semver_functions=>parse( version = invalid_version-value loose = invalid_version-loose )
        msg = msg ).
    ENDLOOP.

    " parse a version into a SemVer object
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>parse( '1.2.3' )->version
      exp = '1.2.3' ).

    DATA(s) = zcl_semver=>create( '4.5.6' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>parse( s )
      exp = s
      msg = 'return it if it is a SemVer obj' ).

    DATA(l) = zcl_semver=>create( version = '4.2.0' loose = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>parse( version = '4.2.0' loose = abap_true )->version
      exp = l->version
      msg = 'looseness as an option' ).
  ENDMETHOD.

  METHOD test_patch.
    " Version should be detectable despite extra characters

    DATA(tests) = VALUE ty_tests(
      ( range = '1.2.1' version = 1 )
      ( range = ' 1.2.1 ' version = 1 )
      ( range = ' 1.2.2-4 ' version = 2 )
      ( range = ' 1.2.3-pre ' version = 3 )
      ( range = 'v1.2.5' version = 5 )
      ( range = ' v1.2.8 ' version = 8 )
      ( range = |\t1.2.13| version = 13 )
      ( range = '=1.2.21' version = 21 loose = abap_true )
      ( range = 'v=1.2.34' version = 34 loose = abap_true ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-range } { test-loose } { test-version } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>patch( version = test-range loose = test-loose )
        exp = condense( test-version )
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_prerelease.

    TYPES:
      BEGIN OF ty_test,
        version TYPE string,
        loose   TYPE abap_bool,
        prerel  TYPE string_table,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( version = '1.2.2-alpha.1' prerel = VALUE #( ( `alpha` ) ( `1` ) ) )
      ( version = '0.6.1-1' prerel = VALUE #( ( `1` ) ) )
      ( version = '1.0.0-beta.2' prerel = VALUE #( ( `beta` ) ( `2` ) ) )
      ( version = 'v0.5.4-pre' prerel = VALUE #( ( `pre` ) ) )
      ( version = '1.2.2-alpha.1' prerel = VALUE #( ( `alpha` ) ( `1` ) ) loose = abap_false )
      ( version = '0.6.1beta' prerel = VALUE #( ( `beta` ) ) loose = abap_true )
      ( version = '1.0.0' loose = abap_true )
      ( version = '~2.0.0-alpha.1'  loose = abap_false )
      ( version = 'invalid version' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-version } { test-loose } { lines( test-prerel ) } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>prerelease( version = test-version loose = test-loose )
        exp = test-prerel
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_rcompare.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>rcompare( a = '1.0.0' b = '1.0.1' )
      exp = +1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>rcompare( a = '1.0.0' b = '1.0.0' )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>rcompare( a = '1.0.0+0' b = '1.0.0' )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>rcompare( a = '1.0.1' b = '1.0.0' )
      exp = -1 ).

  ENDMETHOD.

  METHOD test_rsort.

    DATA(list) = VALUE string_table(
      ( `1.2.3+1` )
      ( `1.2.3+0` )
      ( `1.2.3` )
      ( `5.9.6` )
      ( `0.1.2` ) ).

    DATA(rsorted) = VALUE string_table(
      ( `5.9.6` )
      ( `1.2.3+1` )
      ( `1.2.3+0` )
      ( `1.2.3` )
      ( `0.1.2` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>rsort( list )
      exp = rsorted ).

  ENDMETHOD.

  METHOD test_satisfies.

    DATA tests TYPE zcl_semver_fixtures=>ty_ranges.

    LOOP AT zcl_semver_fixtures=>range_include( ) INTO DATA(range_include).
      DATA(msg) = |{ range_include-range } { range_include-version } { range_include-loose } { range_include-incpre } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>satisfies(
                 version = range_include-version
                 range   = range_include-range
                 loose   = range_include-loose
                 incpre  = range_include-incpre )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_semver_fixtures=>range_exclude( ) INTO DATA(range_exclude).
      msg = |{ range_exclude-range } { range_exclude-version } { range_exclude-loose } { range_exclude-incpre } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>satisfies(
                 version = range_exclude-version
                 range   = range_exclude-range
                 loose   = range_exclude-loose
                 incpre  = range_exclude-incpre )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    " invalid ranges never satisfied (but do not throw)
    tests = VALUE #(
      ( range = 'blerg' version = '1.2.3' )
      ( range = 'git+https://user:password0123@github.com/foo' version = '123.0.0' loose = abap_true )
      ( range = '^1.2.3' version = '2.0.0-pre' )
      ( range = '0.x' version = '' )
      ( range = '*' version = '' ) ).

    LOOP AT tests INTO DATA(test).
      msg = |{ test-range } { test-version } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_semver_functions=>satisfies( version = test-version range = test-range loose = test-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_sort.

    DATA(list) = VALUE string_table(
      ( `1.2.3+1` )
      ( `1.2.3+0` )
      ( `1.2.3` )
      ( `5.9.6` )
      ( `0.1.2` ) ).

    DATA(sorted) = VALUE string_table(
      ( `0.1.2` )
      ( `1.2.3` )
      ( `1.2.3+0` )
      ( `1.2.3+1` )
      ( `5.9.6` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>sort( list )
      exp = sorted ).

  ENDMETHOD.

  METHOD test_valid.
    " returns null instead of throwing when presented with garbage

    LOOP AT zcl_semver_fixtures=>invalid_versions( ) INTO DATA(invalid_version).
      DATA(msg) = |{ invalid_version-value } { invalid_version-loose } { invalid_version-reason } |.

      cl_abap_unit_assert=>assert_initial(
        act = zcl_semver_functions=>valid( version = invalid_version-value loose = invalid_version-loose )
        msg = msg ).
    ENDLOOP.

    " validate a version into a SemVer object
    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>valid( '1.2.3' )
      exp = '1.2.3' ).

    DATA(s) = zcl_semver=>create( '4.5.6' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>valid( s )
      exp = '4.5.6'
      msg = 'return the version if a SemVer obj' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver_functions=>valid( version = '4.2.0foo' loose = abap_true )
      exp = '4.2.0-foo'
      msg = 'looseness as an option' ).

  ENDMETHOD.

ENDCLASS.
