CLASS ltcl_semver_range DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      setup,
      range_include FOR TESTING RAISING /apmg/cx_error,
      range_intersect FOR TESTING RAISING /apmg/cx_error,
      range_exclude FOR TESTING RAISING /apmg/cx_error,
      range_parse FOR TESTING RAISING /apmg/cx_error,
      range_parse_options FOR TESTING RAISING /apmg/cx_error,
      legacy_range_parse_options FOR TESTING RAISING /apmg/cx_error,
      range_formatted FOR TESTING RAISING /apmg/cx_error,
      empty_comparator FOR TESTING RAISING /apmg/cx_error,
      create_from_comparator FOR TESTING RAISING /apmg/cx_error,
      create_from_range FOR TESTING RAISING /apmg/cx_error,
      strict_vs_loose FOR TESTING RAISING /apmg/cx_error,
      to_string FOR TESTING RAISING /apmg/cx_error.

ENDCLASS.

CLASS /apmg/cl_semver_range DEFINITION LOCAL FRIENDS ltcl_semver_range.

CLASS ltcl_semver_range IMPLEMENTATION.

  METHOD setup.
    /apmg/cl_semver_range=>clear_cache( ).
  ENDMETHOD.

  METHOD range_include.

    LOOP AT /apmg/cl_semver_fixtures=>range_include( ) INTO DATA(range_include).
      DATA(msg) = |{ range_include-range } { range_include-version } { range_include-loose } { range_include-incpre }|.
      DATA(r) = /apmg/cl_semver_range=>create(
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

    LOOP AT /apmg/cl_semver_fixtures=>range_exclude( ) INTO DATA(range_exclude).
      DATA(msg) = |{ range_exclude-range } { range_exclude-version } { range_exclude-loose } { range_exclude-incpre }|.
      DATA(r) = /apmg/cl_semver_range=>create(
        range  = range_exclude-range
        loose  = range_exclude-loose
        incpre = range_exclude-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = r->test( range_exclude-version )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD range_intersect.

    LOOP AT /apmg/cl_semver_fixtures=>range_intersection( ) INTO DATA(range_intersection).
      DATA(msg) = |{ range_intersection-r0 } { range_intersection-r1 }|.
      DATA(r0) = /apmg/cl_semver_range=>create( range_intersection-r0 ).
      DATA(r1) = /apmg/cl_semver_range=>create( range_intersection-r1 ).

      cl_abap_unit_assert=>assert_equals(
        act = r0->intersects( r1 )
        exp = range_intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = r1->intersects( r0 )
        exp = range_intersection-res
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD range_parse.
    LOOP AT /apmg/cl_semver_fixtures=>range_parse( ) INTO DATA(range_parse).
      DATA(description) = |{ range_parse-range } [loose={ range_parse-loose }, incpre={ range_parse-incpre }]|.
      IF range_parse-res IS INITIAL.
        TRY.
            /apmg/cl_semver_range=>create(
              range  = range_parse-range
              loose  = range_parse-loose
              incpre = range_parse-incpre ).
            cl_abap_unit_assert=>fail( msg = |Invalid range: { description }| ).
          CATCH /apmg/cx_error ##NO_HANDLER.
        ENDTRY.
      ELSE.
        DATA(parsed) = /apmg/cl_semver_range=>create(
          range  = range_parse-range
          loose  = range_parse-loose
          incpre = range_parse-incpre ).
        cl_abap_unit_assert=>assert_equals(
          act = COND string( WHEN parsed->range( ) IS INITIAL THEN `*` ELSE parsed->range( ) )
          exp = range_parse-res
          msg = |{ description } => { range_parse-res }| ).
        cl_abap_unit_assert=>assert_equals(
          act = parsed->range( )
          exp = /apmg/cl_semver_range=>create(
            range  = range_parse-res
            incpre = range_parse-incpre )->range( )
          msg = |Parsing both yields the same result: { description }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD range_parse_options.
    DATA:
      default_inputs  TYPE string_table,
      loose_inputs    TYPE string_table,
      incpre_inputs   TYPE string_table,
      combined_inputs TYPE string_table.

    LOOP AT /apmg/cl_semver_fixtures=>range_parse( ) INTO DATA(range_parse).
      IF range_parse-loose = abap_true AND range_parse-incpre = abap_true.
        APPEND range_parse-range TO combined_inputs.
      ELSEIF range_parse-loose = abap_true.
        APPEND range_parse-range TO loose_inputs.
      ELSEIF range_parse-incpre = abap_true.
        APPEND range_parse-range TO incpre_inputs.
      ELSE.
        APPEND range_parse-range TO default_inputs.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      act = lines( default_inputs )
      exp = 348
      msg = 'Retains every source fixture row' ).
    DATA(unique_inputs) = default_inputs.
    SORT unique_inputs.
    DELETE ADJACENT DUPLICATES FROM unique_inputs.
    cl_abap_unit_assert=>assert_equals(
      act = lines( unique_inputs )
      exp = 335
      msg = 'Retains every distinct source input' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( loose_inputs )
      exp = lines( default_inputs )
      msg = 'Loose mode has the same source fixture rows' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( incpre_inputs )
      exp = lines( default_inputs )
      msg = 'Include-prerelease mode has the same source fixture rows' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( combined_inputs )
      exp = lines( default_inputs )
      msg = 'Combined mode has the same source fixture rows' ).
    cl_abap_unit_assert=>assert_equals(
      act = loose_inputs
      exp = default_inputs
      msg = 'Loose mode covers the same inputs' ).
    cl_abap_unit_assert=>assert_equals(
      act = incpre_inputs
      exp = default_inputs
      msg = 'Include-prerelease mode covers the same inputs' ).
    cl_abap_unit_assert=>assert_equals(
      act = combined_inputs
      exp = default_inputs
      msg = 'Combined mode covers the same inputs' ).
  ENDMETHOD.


  METHOD legacy_range_parse_options.
    TYPES:
      BEGIN OF ty_case,
        range  TYPE string,
        expect TYPE string,
        loose  TYPE abap_bool,
      END OF ty_case,
      ty_cases TYPE STANDARD TABLE OF ty_case WITH EMPTY KEY.
    DATA(cases) = VALUE ty_cases(
      ( range = '>01.02.03' expect = '>1.2.3' loose = abap_true )
      ( range = '>01.02.03' )
      ( range = '>= 1.2.3' expect = '>=1.2.3' )
      ( range = '1.2.3' expect = '1.2.3' )
      ( range = '1.2.3' expect = '1.2.3' ) ).
    LOOP AT cases INTO DATA(test).
      IF test-expect IS INITIAL.
        TRY.
            /apmg/cl_semver_range=>create( range = test-range loose = test-loose ).
            cl_abap_unit_assert=>fail( msg = 'Strict boolean rejects leading zeros' ).
          CATCH /apmg/cx_error ##NO_HANDLER.
        ENDTRY.
      ELSE.
        cl_abap_unit_assert=>assert_equals(
          act = /apmg/cl_semver_range=>create( range = test-range loose = test-loose )->range( )
          exp = test-expect
          msg = |Legacy range parsing options [loose={ test-loose }]| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD range_formatted.
    " formatted value is calculated lazily and cached

    DATA(r) = /apmg/cl_semver_range=>create( '>= 1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = r->formatted
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = r->format( )
      exp = '>=1.2.3' ).
    cl_abap_unit_assert=>assert_equals(
      act = r->formatted
      exp = '>=1.2.3' ).
    cl_abap_unit_assert=>assert_equals(
      act = r->format( )
      exp = '>=1.2.3' ).

  ENDMETHOD.


  METHOD empty_comparator.

    TRY.
        /apmg/cl_semver_range=>create(
          range = 'sadf||asdf'
          loose = abap_true ).
        cl_abap_unit_assert=>fail( ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD create_from_comparator.

    DATA(c) = /apmg/cl_semver_comparator=>create( '>=1.2.3' ).

    DATA(r) = /apmg/cl_semver_range=>create( c ).

    cl_abap_unit_assert=>assert_equals(
      act = r->raw
      exp = c->value ).

  ENDMETHOD.

  METHOD create_from_range.

    DATA(loose) = /apmg/cl_semver_range=>create(
      range = '1.2.3'
      loose = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_range=>create( range = loose loose = abap_true )->range( )
      exp = loose->range( ) ).

    cl_abap_unit_assert=>assert_differs(
      act = /apmg/cl_semver_range=>create( range = loose )->options
      exp = loose->options ).

    DATA(incpre) = /apmg/cl_semver_range=>create(
      range  = '1.2.3'
      incpre = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_range=>create( range = incpre incpre = abap_true )->range( )
      exp = incpre->range( ) ).

    cl_abap_unit_assert=>assert_differs(
      act = /apmg/cl_semver_range=>create( range = incpre )->options
      exp = incpre->options ).

  ENDMETHOD.

  METHOD strict_vs_loose.

    TRY.
        DATA(loose) = /apmg/cl_semver_range=>create( '>=01.02.03' ).
        cl_abap_unit_assert=>fail( ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_range=>create( range = '>=01.02.03' loose = abap_true )->range( )
      exp = '>=1.2.3' ).

    TRY.
        loose = /apmg/cl_semver_range=>create( '~1.02.03beta' ).
        cl_abap_unit_assert=>fail( ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_range=>create( range = '~1.02.03beta' loose = abap_true )->range( )
      exp = '>=1.2.3-beta <1.3.0-0' ).

  ENDMETHOD.

  METHOD to_string.

    DATA(semrange) = /apmg/cl_semver_range=>create( '>= v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = semrange->to_string( )
      exp = '>=1.2.3' ).

  ENDMETHOD.

ENDCLASS.
