CLASS ltcl_trim_whitespace DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    TYPES:
      ty_options TYPE STANDARD TABLE OF /apmg/if_semver_options=>ty_options WITH EMPTY KEY,
      BEGIN OF ty_case,
        input    TYPE string,
        expected TYPE string,
        minimum  TYPE string,
        matching TYPE string_table,
      END OF ty_case,
      ty_cases TYPE STANDARD TABLE OF ty_case WITH EMPTY KEY.

    DATA options TYPE ty_options.

    METHODS:
      setup,
      equality_after_versions FOR TESTING RAISING /apmg/cx_error,
      release_comparator_sets FOR TESTING RAISING /apmg/cx_error,
      malformed_boundaries FOR TESTING RAISING /apmg/cx_error,
      invalid_loose_alternatives FOR TESTING RAISING /apmg/cx_error,
      normalized_comparator_errors FOR TESTING RAISING /apmg/cx_error.

    METHODS check_range
      IMPORTING
        input    TYPE string
        expected TYPE string
        minimum  TYPE string
        versions TYPE string_table
        matching TYPE string_table
        opts     TYPE /apmg/if_semver_options=>ty_options OPTIONAL
      RAISING
        /apmg/cx_error.
ENDCLASS.

CLASS ltcl_trim_whitespace IMPLEMENTATION.

  METHOD setup.
    options = VALUE #(
      ( )
      ( loose = abap_true )
      ( incpre = abap_true )
      ( loose = abap_true incpre = abap_true ) ).
    /apmg/cl_semver_range=>clear_cache( ).
  ENDMETHOD.


  METHOD check_range.
    DATA minimum_matching TYPE string.
    DATA maximum_matching TYPE string.

    DATA(context) = |{ input } [loose={ opts-loose }, incpre={ opts-incpre }]|.
    DATA(range) = /apmg/cl_semver_range=>create( range = input loose = opts-loose incpre = opts-incpre ).
    cl_abap_unit_assert=>assert_equals(
      act = range->range( )
      exp = expected
      msg = |{ context }: Range preserves the comparator set| ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>valid_range( range = input loose = opts-loose incpre = opts-incpre )
      exp = expected
      msg = |{ context }: validRange preserves the comparator set| ).


    LOOP AT versions INTO DATA(version).
      DATA(matches) = xsdbool( line_exists( matching[ table_line = version ] ) ).
      cl_abap_unit_assert=>assert_equals(
        act = range->test( version )
        exp = matches
        msg = |{ context }: Range.test({ version })| ).
      cl_abap_unit_assert=>assert_equals(
        act = /apmg/cl_semver_functions=>satisfies(
          version = version
          range   = input
          loose   = opts-loose
          incpre  = opts-incpre )
        exp = matches
        msg = |{ context }: satisfies({ version })| ).
    ENDLOOP.

    DATA(min) = /apmg/cl_semver_ranges=>min_version( range = input loose = opts-loose incpre = opts-incpre ).
    IF minimum IS INITIAL.
      cl_abap_unit_assert=>assert_not_bound(
        act = min
        msg = |{ context }: minVersion preserves the lower bound| ).
    ELSE.
      cl_abap_unit_assert=>assert_bound(
        act = min
        msg = |{ context }: minVersion returns a version| ).
      cl_abap_unit_assert=>assert_equals(
        act = min->version
        exp = minimum
        msg = |{ context }: minVersion preserves the lower bound| ).
    ENDIF.

    IF matching IS NOT INITIAL.
      minimum_matching = matching[ 1 ].
      maximum_matching = matching[ lines( matching ) ].
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>min_satisfying(
        versions = versions
        range    = input
        loose    = opts-loose
        incpre   = opts-incpre )
      exp = minimum_matching
      msg = |{ context }: minSatisfying| ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>max_satisfying(
        versions = versions
        range    = input
        loose    = opts-loose
        incpre   = opts-incpre )
      exp = maximum_matching
      msg = |{ context }: maxSatisfying| ).
  ENDMETHOD.


  METHOD equality_after_versions.
    DATA(names) = VALUE string_table( ( `` ) ( `dev` ) ( `v` ) ( `vv` ) ( `1.dev` ) ( `dev.1` ) ( `beta.1` ) ( `preview` ) ).
    DATA(metadata) = VALUE string_table( ( `` ) ( `+build` ) ( `+build.1` ) ).
    DATA(whitespace) = VALUE string_table(
      ( ` ` ) ( `  ` ) ( |\t| ) ( |\n| )
      ( CONV string( cl_abap_conv_in_ce=>uccp( '00A0' ) ) )
      ( CONV string( cl_abap_conv_in_ce=>uccp( 'FEFF' ) ) ) ).
    DATA checked TYPE i.

    LOOP AT names INTO DATA(name).
      LOOP AT metadata INTO DATA(build).
        LOOP AT whitespace INTO DATA(ws).
          LOOP AT options INTO DATA(opts).
            DATA(version) = `1.2.3` && COND string( WHEN name IS NOT INITIAL THEN `-` && name ).
            DATA(input) = version && build && ws && `=` && ws && version.
            DATA(other) = COND string( WHEN name IS NOT INITIAL THEN `1.2.3` ELSE `1.2.3-dev` ).
            check_range(
              input    = input
              expected = version
              minimum  = version
              versions = VALUE #( ( `1.2.2` ) ( version ) ( other ) ( `1.2.4` ) )
              matching = VALUE #( ( version ) )
              opts     = opts ).
            checked = checked + 1.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals(
      act = checked
      exp = 576
      msg = 'Checks every declared combination' ).
  ENDMETHOD.


  METHOD release_comparator_sets.
    DATA(versions) = VALUE string_table(
      ( `0.0.0` ) ( `0.2.3` ) ( `0.2.4` ) ( `0.3.0` ) ( `1.0.0` ) ( `1.2.2` )
      ( `1.2.3-dev` ) ( `1.2.3` ) ( `1.2.4` ) ( `1.3.0` ) ( `1.5.0` ) ( `2.0.0` ) ).
    DATA(cases) = VALUE ty_cases(
      ( input = |>=\t1.2.3 <\n2.0.0|
        expected = `>=1.2.3 <2.0.0` minimum = `1.2.3`
        matching = VALUE #( ( `1.2.3` ) ( `1.2.4` ) ( `1.3.0` ) ( `1.5.0` ) ) )
      ( input = `>= 1.2 < 2`
        expected = `>=1.2.0 <2.0.0-0` minimum = `1.2.0`
        matching = VALUE #( ( `1.2.2` ) ( `1.2.3` ) ( `1.2.4` ) ( `1.3.0` ) ( `1.5.0` ) ) )
      ( input = `~ 1.2 >= 1.2.3`
        expected = `>=1.2.0 <1.3.0-0 >=1.2.3` minimum = `1.2.3`
        matching = VALUE #( ( `1.2.3` ) ( `1.2.4` ) ) )
      ( input = `^ 0.2.3 < 0.3.0`
        expected = `>=0.2.3 <0.3.0-0 <0.3.0` minimum = `0.2.3`
        matching = VALUE #( ( `0.2.3` ) ( `0.2.4` ) ) )
      ( input = `<= 0.x`
        expected = `<1.0.0-0` minimum = `0.0.0`
        matching = VALUE #( ( `0.0.0` ) ( `0.2.3` ) ( `0.2.4` ) ( `0.3.0` ) ) )
      ( input = `> *` expected = `<0.0.0-0` )
      ( input = `>= 2 < 1` expected = `>=2.0.0 <1.0.0-0` )
      ( input = `>= 1.x <= 2.X`
        expected = `>=1.0.0 <3.0.0-0` minimum = `1.0.0`
        matching = VALUE #( ( `1.0.0` ) ( `1.2.2` ) ( `1.2.3` ) ( `1.2.4` ) ( `1.3.0` ) ( `1.5.0` ) ( `2.0.0` ) ) )
      ( input = `1.2.3+build - 2.0.0+build`
        expected = `>=1.2.3 <=2.0.0` minimum = `1.2.3`
        matching = VALUE #( ( `1.2.3` ) ( `1.2.4` ) ( `1.3.0` ) ( `1.5.0` ) ( `2.0.0` ) ) ) ).

    LOOP AT cases INTO DATA(test).
      check_range(
        input    = test-input
        expected = test-expected
        minimum  = test-minimum
        versions = versions
        matching = test-matching ).
    ENDLOOP.
  ENDMETHOD.


  METHOD malformed_boundaries.
    DATA(inputs) = VALUE string_table(
      ( `1.2.3-dev= 1` )
      ( `1.2.3-v= x` )
      ( `1.2.3-1.dev= 2.0.0` )
      ( `1.2.3-dev+build= 1` )
      ( `1.2.3= 1` )
      ( `1.2.3-beta.1= 1` ) ).
    DATA(versions) = VALUE string_table( ( `0.0.0` ) ( `1.2.3-dev` ) ( `1.2.3` ) ( `1.5.0` ) ( `2.0.0` ) ).

    LOOP AT inputs INTO DATA(input).
      LOOP AT options INTO DATA(opts).
        DATA(context) = |{ input } [loose={ opts-loose }, incpre={ opts-incpre }]|.
        TRY.
            /apmg/cl_semver_range=>create( range = input loose = opts-loose incpre = opts-incpre ).
            cl_abap_unit_assert=>fail( msg = |{ context }: Range must reject| ).
          CATCH /apmg/cx_error ##NO_HANDLER.
        ENDTRY.
        cl_abap_unit_assert=>assert_initial(
          act = /apmg/cl_semver_ranges=>valid_range( range = input loose = opts-loose incpre = opts-incpre )
          msg = |{ context }: validRange rejects| ).
        LOOP AT versions INTO DATA(version).
          cl_abap_unit_assert=>assert_false(
            act = /apmg/cl_semver_functions=>satisfies(
              version = version
              range   = input
              loose   = opts-loose
              incpre  = opts-incpre )
            msg = |{ context }: satisfies({ version }) rejects| ).
        ENDLOOP.
        TRY.
            /apmg/cl_semver_ranges=>min_version( range = input loose = opts-loose incpre = opts-incpre ).
            cl_abap_unit_assert=>fail( msg = |{ context }: minVersion must reject| ).
          CATCH /apmg/cx_error ##NO_HANDLER.
        ENDTRY.
        cl_abap_unit_assert=>assert_initial(
          act = /apmg/cl_semver_ranges=>min_satisfying(
            versions = versions
            range    = input
            loose    = opts-loose
            incpre   = opts-incpre )
          msg = |{ context }: minSatisfying rejects| ).
        cl_abap_unit_assert=>assert_initial(
          act = /apmg/cl_semver_ranges=>max_satisfying(
            versions = versions
            range    = input
            loose    = opts-loose
            incpre   = opts-incpre )
          msg = |{ context }: maxSatisfying rejects| ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD invalid_loose_alternatives.
    DATA(versions) = VALUE string_table( ( `0.0.0` ) ( `1.2.3-dev` ) ( `1.2.3` ) ( `1.5.0` ) ( `2.0.0` ) ).
    DATA(cases) = VALUE ty_cases(
      ( input = `1.2.3-dev || 1.2.3-dev= x` expected = `1.2.3-dev` )
      ( input = `1.2.3-dev= 1 || 2.0.0` expected = `2.0.0` )
      ( input = `1.2.3 || 1.2.3= x` expected = `1.2.3` ) ).

    LOOP AT options INTO DATA(opts) WHERE loose = abap_true.
      LOOP AT cases INTO DATA(test).
        check_range(
          input    = test-input
          expected = test-expected
          minimum  = test-expected
          versions = versions
          matching = VALUE #( ( test-expected ) )
          opts     = opts ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD normalized_comparator_errors.
    DATA(input) = |\t1.2.3-dev=\n1 |.
    TYPES:
      BEGIN OF ty_error,
        loose TYPE abap_bool,
        text  TYPE string,
      END OF ty_error,
      ty_errors TYPE STANDARD TABLE OF ty_error WITH EMPTY KEY.
    DATA(errors) = VALUE ty_errors(
      ( loose = abap_false text = `Invalid comparator: 1.2.3-dev=1` )
      ( loose = abap_true text = `Invalid SemVer Range: 1.2.3-dev= 1` ) ).

    LOOP AT errors INTO DATA(expected).
      TRY.
          /apmg/cl_semver_range=>create( range = input loose = expected-loose ).
          cl_abap_unit_assert=>fail( msg = |Range must reject [loose={ expected-loose }]| ).
        CATCH /apmg/cx_error INTO DATA(error).
          cl_abap_unit_assert=>assert_equals(
            act = error->get_text( )
            exp = expected-text
            msg = |Invalid comparator errors preserve the normalized input [loose={ expected-loose }]| ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
