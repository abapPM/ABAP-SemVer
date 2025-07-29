CLASS ltcl_semver_comparator DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      comparator FOR TESTING RAISING /apmg/cx_error,
      to_string FOR TESTING RAISING /apmg/cx_error,
      intersect FOR TESTING RAISING /apmg/cx_error,
      any FOR TESTING RAISING /apmg/cx_error,
      invalid FOR TESTING RAISING /apmg/cx_error,
      ignore_equal FOR TESTING RAISING /apmg/cx_error.

ENDCLASS.

CLASS ltcl_semver_comparator IMPLEMENTATION.

  METHOD comparator.

    DATA(c) = /apmg/cl_semver_comparator=>create( '>=1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = c->test( '1.2.4' )
      exp = abap_true ).

    DATA(c2) = /apmg/cl_semver_comparator=>create( c ).

    cl_abap_unit_assert=>assert_equals(
      act = c2->test( '1.2.4' )
      exp = abap_true ).

    DATA(c3) = /apmg/cl_semver_comparator=>create( comp = c loose = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = c3->test( '1.2.4' )
      exp = abap_true ).

    " test an invalid version, should not throw
    DATA(c4) = /apmg/cl_semver_comparator=>create( c ).

    cl_abap_unit_assert=>assert_equals(
      act = c4->test( 'not a version string' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD to_string.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_comparator=>create( '>= 1.2.3' )->to_string( )
      exp = '>=1.2.3' ).

  ENDMETHOD.

  METHOD intersect.

    LOOP AT /apmg/cl_semver_fixtures=>comparator_intersection( ) INTO DATA(intersection).
      DATA(msg) = |{ intersection-c0 } { intersection-c1 }|.
      DATA(comp0) = /apmg/cl_semver_comparator=>create( intersection-c0 ).
      DATA(comp1) = /apmg/cl_semver_comparator=>create( intersection-c1 ).

      cl_abap_unit_assert=>assert_equals(
        act = comp0->intersects( comp = comp1 incpre = intersection-incpre )
        exp = intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = comp1->intersects( comp = comp0 incpre = intersection-incpre )
        exp = intersection-res
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD any.
    " ANY matches anything

    DATA(c) = /apmg/cl_semver_comparator=>create( '' ).

    cl_abap_unit_assert=>assert_equals(
      act = c->test( '1.2.3' )
      exp = abap_true
      msg = 'ANY should match anything' ).

    DATA(c1) = /apmg/cl_semver_comparator=>create( '>=1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = c1->test( /apmg/cl_semver_comparator=>any_semver->version )
      exp = abap_true
      msg = 'anything should match ANY' ).

  ENDMETHOD.

  METHOD invalid.

    TRY.
        /apmg/cl_semver_comparator=>create( 'foo bar baz' ).

        cl_abap_unit_assert=>fail( msg = 'Should throw invalid comparator' ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD ignore_equal.
    " equal sign is ignored

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_comparator=>create( '=1.2.3' )->value
      exp = /apmg/cl_semver_comparator=>create( '1.2.3' )->value ).

  ENDMETHOD.

ENDCLASS.
