CLASS ltcl_comparator_normalization DEFINITION FOR TESTING RISK LEVEL HARMLESS
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

CLASS ltcl_comparator_normalization IMPLEMENTATION.

  METHOD setup.
    options = VALUE #(
      ( )
      ( loose  = abap_true )
      ( incpre = abap_true )
      ( loose  = abap_true incpre = abap_true ) ).
    /apmg/cl_semver_range=>clear_cache( ).
  ENDMETHOD.

  METHOD check_range.
    DATA minimum_matching TYPE string.
    DATA maximum_matching TYPE string.

    DATA(context) = |{ input } [loose={ opts-loose }, incpre={ opts-incpre }]|.

    TRY.
        DATA(range) = /apmg/cl_semver_range=>create( range = input loose = opts-loose incpre = opts-incpre ).
      CATCH /apmg/cx_error INTO DATA(error).
        cl_abap_unit_assert=>fail( |{ context }: { error->get_text( ) }| ).
    ENDTRY.

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
      ( CONV string( cl_abap_conv_in_ce=>uccp( '3000' ) ) ) ). " JS tests FEFF but ABAP does not support that
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
      ( `0.0.0` )     ( `0.2.3` ) ( `0.2.4` ) ( `0.3.0` ) ( `1.0.0` ) ( `1.2.2` )
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
      ( input = `1.2.3-dev= 1 || 2.0.0`     expected = `2.0.0` )
      ( input = `1.2.3 || 1.2.3= x`         expected = `1.2.3` ) ).

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
      ( loose = abap_true  text = `Invalid SemVer Range: 1.2.3-dev= 1` ) ).

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

CLASS ltcl_long_identifiers DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    TYPES:
      ty_options TYPE STANDARD TABLE OF /apmg/if_semver_options=>ty_options WITH EMPTY KEY,
      ty_lengths TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    CONSTANTS long_id TYPE string VALUE '928490632884417731e7af463c92b034d6a78268fc993bcb88a57944'.

    DATA options TYPE ty_options.

    METHODS:
      setup,
      long_prereleases FOR TESTING RAISING /apmg/cx_error,
      build_metadata_limits FOR TESTING RAISING /apmg/cx_error,
      invalid_version_limits FOR TESTING RAISING /apmg/cx_error.

    METHODS check_range
      IMPORTING
        input    TYPE string
        expected TYPE string
        version  TYPE string
        opts     TYPE /apmg/if_semver_options=>ty_options
      RAISING
        /apmg/cx_error.

    METHODS check_invalid_range
      IMPORTING
        input TYPE string
        opts  TYPE /apmg/if_semver_options=>ty_options
      RAISING
        /apmg/cx_error.

ENDCLASS.

CLASS ltcl_long_identifiers IMPLEMENTATION.

  METHOD setup.
    options = VALUE #(
      ( )
      ( loose = abap_true )
      ( incpre = abap_true )
      ( loose = abap_true incpre = abap_true ) ).
    /apmg/cl_semver_range=>clear_cache( ).
  ENDMETHOD.

  METHOD check_range.
    DATA(context) = |{ input } [loose={ opts-loose }, incpre={ opts-incpre }]|.
    DATA(versions) = VALUE string_table( ( `0.0.0-0` ) ( version ) ( `1.2.4` ) ).
    DATA(range) = /apmg/cl_semver_range=>create( range = input loose = opts-loose incpre = opts-incpre ).

    cl_abap_unit_assert=>assert_equals(
      act = range->range( )
      exp = expected
      msg = |{ context }: Range preserves the identifier| ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>valid_range( range = input loose = opts-loose incpre = opts-incpre )
      exp = expected
      msg = |{ context }: validRange preserves the identifier| ).
    cl_abap_unit_assert=>assert_true(
      act = range->test( version )
      msg = |{ context }: Range.test accepts the matching version| ).
    cl_abap_unit_assert=>assert_true(
      act = /apmg/cl_semver_functions=>satisfies(
        version = version range = input loose = opts-loose incpre = opts-incpre )
      msg = |{ context }: satisfies accepts the matching version| ).
    cl_abap_unit_assert=>assert_false(
      act = /apmg/cl_semver_functions=>satisfies(
        version = `0.0.0-0` range = input loose = opts-loose incpre = opts-incpre )
      msg = |{ context }: excludes a different prerelease| ).
    cl_abap_unit_assert=>assert_false(
      act = /apmg/cl_semver_functions=>satisfies(
        version = `1.2.4` range = input loose = opts-loose incpre = opts-incpre )
      msg = |{ context }: excludes a different release| ).
    DATA(minimum) = /apmg/cl_semver_ranges=>min_version(
      range = input loose = opts-loose incpre = opts-incpre ).
    cl_abap_unit_assert=>assert_bound( act = minimum msg = |{ context }: minVersion returns a version| ).
    cl_abap_unit_assert=>assert_equals(
      act = minimum->version
      exp = version
      msg = |{ context }: minVersion preserves the identifier| ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>min_satisfying(
        versions = versions range = input loose = opts-loose incpre = opts-incpre )
      exp = version
      msg = |{ context }: minSatisfying selects the version| ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_ranges=>max_satisfying(
        versions = versions range = input loose = opts-loose incpre = opts-incpre )
      exp = version
      msg = |{ context }: maxSatisfying selects the version| ).
  ENDMETHOD.

  METHOD check_invalid_range.
    DATA(context) = |{ input } [loose={ opts-loose }, incpre={ opts-incpre }]|.
    DATA(versions) = VALUE string_table( ( `1.2.3` ) ).

    TRY.
        /apmg/cl_semver_range=>create( range = input loose = opts-loose incpre = opts-incpre ).
        cl_abap_unit_assert=>fail( msg = |{ context }: Range must reject| ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.
    cl_abap_unit_assert=>assert_initial(
      act = /apmg/cl_semver_ranges=>valid_range( range = input loose = opts-loose incpre = opts-incpre )
      msg = |{ context }: validRange rejects| ).
    cl_abap_unit_assert=>assert_false(
      act = /apmg/cl_semver_functions=>satisfies(
        version = `1.2.3` range = input loose = opts-loose incpre = opts-incpre )
      msg = |{ context }: satisfies rejects| ).
    TRY.
        /apmg/cl_semver_ranges=>min_version( range = input loose = opts-loose incpre = opts-incpre ).
        cl_abap_unit_assert=>fail( msg = |{ context }: minVersion must reject| ).
      CATCH /apmg/cx_error ##NO_HANDLER.
    ENDTRY.
    cl_abap_unit_assert=>assert_initial(
      act = /apmg/cl_semver_ranges=>min_satisfying(
        versions = versions range = input loose = opts-loose incpre = opts-incpre )
      msg = |{ context }: minSatisfying rejects| ).
    cl_abap_unit_assert=>assert_initial(
      act = /apmg/cl_semver_ranges=>max_satisfying(
        versions = versions range = input loose = opts-loose incpre = opts-incpre )
      msg = |{ context }: maxSatisfying rejects| ).
  ENDMETHOD.

  METHOD long_prereleases.
    DATA(max) = /apmg/if_semver_constants=>max_safe_integer.
    DATA(component_length) = /apmg/if_semver_constants=>max_safe_component_length.
    DATA(versions) = VALUE string_table(
      ( |0.0.0-{ long_id }| )
      ( |1.1.1-{ long_id }| )
      ( |{ max }.{ max }.{ max }-{ long_id }| ) ).
    DATA(lengths) = VALUE ty_lengths(
      ( component_length - 1 )
      ( component_length )
      ( component_length + 1 )
      ( 18 )
      ( /apmg/if_semver_constants=>max_length - strlen( `1.2.3-a` ) ) ).

    LOOP AT lengths INTO DATA(length).
      APPEND |1.2.3-{ repeat( val = `9` occ = length ) }a| TO versions.
    ENDLOOP.

    LOOP AT options INTO DATA(opts).
      LOOP AT versions INTO DATA(version).
        DATA(context) = |{ version } [loose={ opts-loose }, incpre={ opts-incpre }]|.
        cl_abap_unit_assert=>assert_equals(
          act = /apmg/cl_semver_functions=>valid( version = version loose = opts-loose incpre = opts-incpre )
          exp = version
          msg = |{ context }: valid accepts the long prerelease| ).
        DATA(semver) = /apmg/cl_semver=>create( version = version loose = opts-loose incpre = opts-incpre ).
        cl_abap_unit_assert=>assert_equals(
          act = semver->version
          exp = version
          msg = |{ context }: SemVer preserves the prerelease| ).
        check_range(
          input = |= \t { version }|
          expected = version
          version = version
          opts = opts ).
        check_range(
          input = |>= \t { version } <= \n { version }|
          expected = |>={ version } <={ version }|
          version = version
          opts = opts ).
        check_range(
          input = |{ version } - { version }|
          expected = |>={ version } <={ version }|
          version = version
          opts = opts ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_metadata_limits.
    DATA(version) = `1.2.3`.
    DATA(identifiers) = VALUE string_table(
      ( long_id )
      ( repeat( val = `9` occ = /apmg/if_semver_constants=>max_safe_component_length + 1 ) )
      ( repeat( val = `9` occ = /apmg/if_semver_constants=>max_safe_build_length ) )
      ( repeat( val = `9` occ = /apmg/if_semver_constants=>max_safe_build_length + 1 ) ) ).

    LOOP AT options INTO DATA(opts).
      LOOP AT identifiers INTO DATA(identifier).
        DATA(input) = |{ version }+{ identifier }|.
        DATA(context) = |{ input } [loose={ opts-loose }, incpre={ opts-incpre }]|.
        IF strlen( input ) <= /apmg/if_semver_constants=>max_length.
          cl_abap_unit_assert=>assert_equals(
            act = /apmg/cl_semver_functions=>valid( version = input loose = opts-loose incpre = opts-incpre )
            exp = version
            msg = |{ context }: valid accepts metadata within the version limit| ).
          DATA(semver) = /apmg/cl_semver=>create( version = input loose = opts-loose incpre = opts-incpre ).
          DATA(build) = VALUE string_table( ( identifier ) ).
          cl_abap_unit_assert=>assert_equals(
            act = semver->build
            exp = build
            msg = |{ context }: SemVer preserves metadata| ).
        ELSE.
          cl_abap_unit_assert=>assert_initial(
            act = /apmg/cl_semver_functions=>valid( version = input loose = opts-loose incpre = opts-incpre )
            msg = |{ context }: valid retains the version length limit| ).
          TRY.
              /apmg/cl_semver=>create( version = input loose = opts-loose incpre = opts-incpre ).
              cl_abap_unit_assert=>fail( msg = |{ context }: SemVer must reject| ).
            CATCH /apmg/cx_error ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        check_range(
          input = |= { input }|
          expected = version
          version = version
          opts = opts ).
        check_range(
          input = |>= { input } <= { input }|
          expected = |>={ version } <={ version }|
          version = version
          opts = opts ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD invalid_version_limits.
    DATA(over_max) = /apmg/if_semver_constants=>max_safe_integer + 1.
    DATA(versions) = VALUE string_table(
      ( |1.2.3-{ repeat( val = `9` occ = /apmg/if_semver_constants=>max_length - strlen( `1.2.3-` ) ) }a| )
      ( |{ over_max }.1.1| )
      ( |1.{ over_max }.1| )
      ( |1.1.{ over_max }| ) ).

    LOOP AT options INTO DATA(opts).
      LOOP AT versions INTO DATA(version).
        DATA(context) = |{ version } [loose={ opts-loose }, incpre={ opts-incpre }]|.
        cl_abap_unit_assert=>assert_initial(
          act = /apmg/cl_semver_functions=>valid( version = version loose = opts-loose incpre = opts-incpre )
          msg = |{ context }: valid rejects| ).
        TRY.
            /apmg/cl_semver=>create( version = version loose = opts-loose incpre = opts-incpre ).
            cl_abap_unit_assert=>fail( msg = |{ context }: SemVer must reject| ).
          CATCH /apmg/cx_error ##NO_HANDLER.
        ENDTRY.
        check_invalid_range( input = |= { version }| opts = opts ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_whitespace DEFINITION FOR TESTING RISK LEVEL HARMLESS
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

CLASS ltcl_whitespace IMPLEMENTATION.

  METHOD setup.
    CONSTANTS occ TYPE i VALUE 50000. " JS tests 500000 but that's too slow for ABAP

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
