CLASS ltcl_semver_ranges DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      gtr FOR TESTING RAISING zcx_semver_error,
      intersects FOR TESTING RAISING zcx_semver_error,
      ltr FOR TESTING RAISING zcx_semver_error,
      max_satisfying FOR TESTING RAISING zcx_semver_error,
      min_satisfying FOR TESTING RAISING zcx_semver_error,
      min_version FOR TESTING RAISING zcx_semver_error,
      outside FOR TESTING RAISING zcx_semver_error,
      simplify FOR TESTING RAISING zcx_semver_error,
      subset FOR TESTING RAISING zcx_semver_error,
      to_comparators FOR TESTING RAISING zcx_semver_error,
      valid_range FOR TESTING RAISING zcx_semver_error.

ENDCLASS.

CLASS ltcl_semver_ranges IMPLEMENTATION.

  METHOD gtr.

    " Version should be greater than range
    LOOP AT zcl_semver_fixtures=>version_gt_range( ) INTO DATA(version_gt_range).
      DATA(msg) = |{ version_gt_range-range } { version_gt_range-version } { version_gt_range-loose }|.
      DATA(act) = zcl_semver_ranges=>gtr(
        range   = version_gt_range-range
        version = version_gt_range-version
        loose   = version_gt_range-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    " Version should not be greater than range
    LOOP AT zcl_semver_fixtures=>version_not_gt_range( ) INTO DATA(version_not_gt_range).
      msg = |{ version_not_gt_range-range } { version_not_gt_range-version } { version_not_gt_range-loose }|.
      act = zcl_semver_ranges=>gtr(
        range   = version_not_gt_range-range
        version = version_not_gt_range-version
        loose   = version_not_gt_range-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD intersects.

  ENDMETHOD.

  METHOD ltr.

    " Version should be less than range
    LOOP AT zcl_semver_fixtures=>version_lt_range( ) INTO DATA(version_lt_range).
      DATA(msg) = |{ version_lt_range-range } { version_lt_range-version } { version_lt_range-loose }|.
      DATA(act) = zcl_semver_ranges=>ltr(
        range   = version_lt_range-range
        version = version_lt_range-version
        loose   = version_lt_range-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    " Version not should be less than range
    LOOP AT zcl_semver_fixtures=>version_not_lt_range( ) INTO DATA(version_not_lt_range).
      msg = |{ version_not_lt_range-range } { version_not_lt_range-version } { version_not_lt_range-loose }|.
      act = zcl_semver_ranges=>ltr(
        range   = version_not_lt_range-range
        version = version_not_lt_range-version
        loose   = version_not_lt_range-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD max_satisfying.

  ENDMETHOD.

  METHOD min_satisfying.

  ENDMETHOD.

  METHOD min_version.

  ENDMETHOD.

  METHOD outside.

  ENDMETHOD.

  METHOD simplify.

  ENDMETHOD.

  METHOD subset.

  ENDMETHOD.

  METHOD to_comparators.

    TYPES:
      BEGIN OF ty_test,
        range TYPE string,
        comps TYPE string,
      END OF ty_test,
      ty_tests TYPE STANDARD TABLE OF ty_test WITH DEFAULT KEY.

    DATA test_act TYPE string.

    DATA(tests) = VALUE ty_tests(
      ( range = '1.0.0 - 2.0.0' comps = '>=1.0.0 <=2.0.0' )
      ( range = '1.0.0' comps = '1.0.0' )
      ( range = '>=*' comps = '' )
      ( range = '' comps = '' )
      ( range = '*' comps = '' )
      ( range = '*' comps = '' )
      ( range = '>=1.0.0' comps = '>=1.0.0' )
      ( range = '>=1.0.0' comps = '>=1.0.0' )
      ( range = '>=1.0.0' comps = '>=1.0.0' )
      ( range = '>1.0.0' comps = '>1.0.0' )
      ( range = '>1.0.0' comps = '>1.0.0' )
      ( range = '<=2.0.0' comps = '<=2.0.0' )
      ( range = '1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '<=2.0.0' comps = '<=2.0.0' )
      ( range = '<=2.0.0' comps = '<=2.0.0' )
      ( range = '<2.0.0' comps = '<2.0.0' )
      ( range = '<2.0.0' comps = '<2.0.0' )
      ( range = '>= 1.0.0' comps = '>=1.0.0' )
      ( range = '>= 1.0.0' comps = '>=1.0.0' )
      ( range = '>=  1.0.0' comps = '>=1.0.0' )
      ( range = '> 1.0.0' comps = '>1.0.0' )
      ( range = '>  1.0.0' comps = '>1.0.0' )
      ( range = '<=  2.0.0' comps = '<=2.0.0' )
      ( range = '<= 2.0.0' comps = '<=2.0.0' )
      ( range = '<= 2.0.0' comps = '<=2.0.0' )
      ( range = '<    2.0.0' comps = '<2.0.0' )
      ( range = '<\t2.0.0' comps = '<2.0.0' )
      ( range = '>=0.1.97' comps = '>=0.1.97' )
      ( range = '>=0.1.97' comps = '>=0.1.97' )
      ( range = '0.1.20 || 1.2.4' comps = '0.1.20 , 1.2.4' )
      ( range = '>=0.2.3 || <0.0.1' comps = '>=0.2.3 , <0.0.1' )
      ( range = '>=0.2.3 || <0.0.1' comps = '>=0.2.3 , <0.0.1' )
      ( range = '>=0.2.3 || <0.0.1' comps = '>=0.2.3 , <0.0.1' )
      ( range = '||' comps = '' )
      ( range = '2.x.x' comps = '>=2.0.0 <3.0.0-0' )
      ( range = '1.2.x' comps = '>=1.2.0 <1.3.0-0' )
      ( range = '1.2.x || 2.x' comps = '>=1.2.0 <1.3.0-0 , >=2.0.0 <3.0.0-0' )
      ( range = '1.2.x || 2.x' comps = '>=1.2.0 <1.3.0-0 , >=2.0.0 <3.0.0-0' )
      ( range = 'x' comps = '' )
      ( range = '2.*.*' comps = '>=2.0.0 <3.0.0-0' )
      ( range = '1.2.*' comps = '>=1.2.0 <1.3.0-0' )
      ( range = '1.2.* || 2.*' comps = '>=1.2.0 <1.3.0-0 , >=2.0.0 <3.0.0-0' )
      ( range = '1.2.* || 2.*' comps = '>=1.2.0 <1.3.0-0 , >=2.0.0 <3.0.0-0' )
      ( range = '*' comps = '' )
      ( range = '2' comps = '>=2.0.0 <3.0.0-0' )
      ( range = '2.3' comps = '>=2.3.0 <2.4.0-0' )
      ( range = '~2.4' comps = '>=2.4.0 <2.5.0-0' )
      ( range = '~2.4' comps = '>=2.4.0 <2.5.0-0' )
      ( range = '~>3.2.1' comps = '>=3.2.1 <3.3.0-0' )
      ( range = '~1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '~>1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '~> 1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '~1.0' comps = '>=1.0.0 <1.1.0-0' )
      ( range = '~ 1.0' comps = '>=1.0.0 <1.1.0-0' )
      ( range = '~ 1.0.3' comps = '>=1.0.3 <1.1.0-0' )
      ( range = '~> 1.0.3' comps = '>=1.0.3 <1.1.0-0' )
      ( range = '<1' comps = '<1.0.0-0' )
      ( range = '< 1' comps = '<1.0.0-0' )
      ( range = '>=1' comps = '>=1.0.0' )
      ( range = '>= 1' comps = '>=1.0.0' )
      ( range = '<1.2' comps = '<1.2.0-0' )
      ( range = '< 1.2' comps = '<1.2.0-0' )
      ( range = '1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '1 2' comps = '>=1.0.0 <2.0.0-0 >=2.0.0 <3.0.0-0' )
      ( range = '1.2 - 3.4.5' comps = '>=1.2.0 <=3.4.5' )
      ( range = '1.2.3 - 3.4' comps = '>=1.2.3 <3.5.0-0' )
      ( range = '1.2.3 - 3' comps = '>=1.2.3 <4.0.0-0' )
      ( range = '>*' comps = '<0.0.0-0' )
      ( range = '<*' comps = '<0.0.0-0' )
      ( range = '>X' comps = '<0.0.0-0' )
      ( range = '<X' comps = '<0.0.0-0' )
      ( range = '<x <* || >* 2.x' comps = '<0.0.0-0' )
      ( range = '>x 2.x || * || <x' comps = '' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(comparators) = zcl_semver_ranges=>to_comparators( test-range ).

      CLEAR test_act.
      LOOP AT comparators ASSIGNING FIELD-SYMBOL(<comparator>).
        IF test_act IS INITIAL.
          test_act = <comparator>->value.
        ELSE.
          test_act = test_act && ' , ' && <comparator>->to_string( ).
        ENDIF.
      ENDLOOP.

      cl_abap_unit_assert=>assert_equals(
        act = test_act
        exp = test-comps
        msg = |{ test-range }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD valid_range.

    " translate ranges into their canonical form
    LOOP AT zcl_semver_fixtures=>range_parse( ) INTO DATA(range_parse).
      DATA(msg) = |{ range_parse-range } { range_parse-res } { range_parse-loose } { range_parse-incpre }|.
      DATA(act) = zcl_semver_ranges=>valid_range(
        range   = range_parse-range
        loose   = range_parse-loose
        incpre  = range_parse-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = range_parse-res
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
