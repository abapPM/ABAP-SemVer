CLASS ltcl_semver_trim_whitespc DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION LONG FINAL.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_case,
        input    TYPE string,
        expected TYPE string,
      END OF ty_case,
      ty_cases TYPE STANDARD TABLE OF ty_case WITH EMPTY KEY.

    METHODS:
      fixture_ranges FOR TESTING,
      operator_whitespace FOR TESTING,
      bounded_combinations FOR TESTING,
      match_boundaries FOR TESTING,
      identifier_fragments FOR TESTING,
      long_prefixes FOR TESTING,
      digit_run_cache FOR TESTING,
      independent_matchers FOR TESTING.

    METHODS assert_legacy
      IMPORTING input TYPE string.

ENDCLASS.

CLASS /apmg/cl_semver_trim_whitespc DEFINITION LOCAL FRIENDS ltcl_semver_trim_whitespc.

CLASS ltcl_semver_trim_whitespc IMPLEMENTATION.

  METHOD assert_legacy.
    " The exported ABAP token omits whitespace in the version prefix for POSIX.
    DATA(pattern) = replace(
      val  = /apmg/cl_semver_re=>token-comparatortrim-safe_src
      sub  = `[v=]*`
      with = `[v=\s]*`
      occ  = 0 ).
    DATA(expected) = replace(
      val   = input
      regex = pattern
      with  = /apmg/cl_semver_re=>comparator_trim_replace
      occ   = 0 ) ##REGEX_POSIX.
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_trim_whitespc=>trim_comparator_whitespace( input )
      exp = expected
      msg = |Input: { input }| ).
  ENDMETHOD.

  METHOD fixture_ranges.
    DATA inputs TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    LOOP AT /apmg/cl_semver_fixtures=>range_parse( ) INTO DATA(fixture).
      INSERT /apmg/cl_semver_utils=>trim( fixture-range ) INTO TABLE inputs.
    ENDLOOP.
    LOOP AT inputs INTO DATA(input).
      assert_legacy( input ).
    ENDLOOP.
  ENDMETHOD.

  METHOD operator_whitespace.
    DATA(cases) = VALUE ty_cases(
      ( input = ` > 1`      expected = ` >1` )
      ( input = `>= vvv1`   expected = `>=vvv1` )
      ( input = `>= v = 1`  expected = `>=v = 1` )
      ( input = `v = 1`     expected = `v = 1` )
      ( input = `= = 1`     expected = `== 1` )
      ( input = `> = 1`     expected = `>= 1` )
      ( input = `> invalid` expected = `> invalid` ) ).
    LOOP AT cases INTO DATA(test_case).
      cl_abap_unit_assert=>assert_equals(
        act = /apmg/cl_semver_trim_whitespc=>trim_comparator_whitespace( test_case-input )
        exp = test_case-expected
        msg = |Input: { test_case-input }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD bounded_combinations.
    DATA(operators) = VALUE string_table( ( `` ) ( `=` ) ( `==` ) ( `<` ) ( `<=` ) ( `>` ) ( `>=` ) ).
    DATA(versions) = VALUE string_table(
      ( `0` ) ( `1` ) ( `1.2` ) ( `1.2.3` ) ( `x` ) ( `X` ) ( `*` )
      ( `0.0.0-928490632884417731e7af463c92b034d6a78268fc993bcb88a57944` )
      ( |1.2.3-{ repeat( val = `9` occ = /apmg/if_semver_constants=>max_length - strlen( `1.2.3-a` ) ) }a| ) ).

    LOOP AT /apmg/cl_semver_trim_whitespc=>whitespace_characters INTO DATA(code).
      DATA(ws) = CONV string( cl_abap_conv_in_ce=>uccpi( code ) ).
      IF code = 32.
        ws = ` `.
      ENDIF.
      DATA(prefix_characters) = VALUE string_table( ( `v` ) ( `=` ) ( ws ) ).
      DATA(prefixes) = VALUE string_table( ( `` ) ( `vvv` ) ( `===` ) ( `v=v` ) ).
      LOOP AT prefix_characters INTO DATA(first).
        APPEND first TO prefixes.
        LOOP AT prefix_characters INTO DATA(second).
          APPEND first && second TO prefixes.
        ENDLOOP.
      ENDLOOP.
      LOOP AT operators INTO DATA(operator).
        LOOP AT prefixes INTO DATA(prefix).
          LOOP AT versions INTO DATA(version).
            DATA(inputs) = VALUE string_table(
              ( operator && ws && prefix && version )
              ( ws && operator && ws && prefix && version )
              ( `1.2.3` && ws && operator && ws && prefix && version )
              ( operator && ws && prefix && version && ws && `<` && ws && `2.0.0` ) ).
            LOOP AT inputs INTO DATA(input).
              assert_legacy( input ).
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD match_boundaries.
    DATA(inputs) = VALUE string_table(
      ( `1.2.3 = 1.2.3` )
      ( `1.2.3-dev = 1.2.3-dev` )
      ( `1.2.3-vv = 1.2` )
      ( `1.2.3-1.dev = x` )
      ( `1.2.3-dev.1 = 1.2.3-dev.1` )
      ( `1.2.3-dev= 1` )
      ( `1.2.3dev = 1.2.3` )
      ( `1.2.3-dev+build = 1.2.3-dev` )
      ( `1.2.3-dev  = 1.2.3-dev` )
      ( `1.2.3-dev   1.2.3` )
      ( `1.2.3-dev = 1.2.3-dev = 1.2.3-dev` )
      ( `1.2.3-dev || 1.2.3-dev= x` )
      ( `^1.2.3-dev = 1.2.3-dev < 2.0.0` )
      ( `< >= 1.2.3` )
      ( `vvv< 1.2.3` )
      ( `v v v` )
      ( `= = =` )
      ( `>vvv` ) ).
    LOOP AT inputs INTO DATA(input).
      assert_legacy( input ).
    ENDLOOP.
  ENDMETHOD.

  METHOD identifier_fragments.
    DATA(fragments) = VALUE string_table(
      ( `x.x.x` )            ( `x.x.x-` )       ( `x.x.x-01.2` ) ( `x.x.x-a+build` )
      ( `1.2.3-` )           ( `1.2.3-A` )      ( `1.2.3-a.` )   ( `1.2.3+` )
      ( `1.2.3+build.more` ) ( `1.2.3+build.` ) ( `1..2` )       ( `1.2.` ) ).
    DO 4 TIMES.
      DATA(length) = /apmg/if_semver_constants=>max_length + sy-index - 2.
      DATA(digits) = repeat( val = `9` occ = length ).
      APPEND |{ digits }.1.2-v| TO fragments.
      APPEND |1.2.3-{ digits }v| TO fragments.
      APPEND |x.x.x-{ digits }v| TO fragments.
      length = /apmg/if_semver_constants=>max_safe_build_length + sy-index - 2.
      DATA(identifier) = repeat( val = `v` occ = length ).
      APPEND |1.2.3-{ identifier }| TO fragments.
      APPEND |1.2.3+{ identifier }| TO fragments.
      APPEND |x.x.x-{ identifier }| TO fragments.
    ENDDO.
    LOOP AT fragments INTO DATA(version).
      assert_legacy( |> { version } = 1.2.3| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD long_prefixes.
    DATA(prefix) = repeat( val = `v= ` occ = 100000 ).
    DATA(operators) = VALUE string_table( ( `` ) ( `<` ) ( `>` ) ).
    LOOP AT operators INTO DATA(operator).
      DATA(input) = operator && ` ` && prefix && `invalid`.
      cl_abap_unit_assert=>assert_equals(
        act = /apmg/cl_semver_trim_whitespc=>trim_comparator_whitespace( input )
        exp = input
        msg = 'Invalid prefix is unchanged' ).
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_trim_whitespc=>trim_comparator_whitespace( `> ` && prefix && `1` )
      exp = `>` && prefix && `1`
      msg = 'A valid prefix retains its internal whitespace' ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_trim_whitespc=>trim_comparator_whitespace( prefix && `< 1.2.3` )
      exp = prefix && `<1.2.3`
      msg = 'An invalid prefix does not hide a later comparator' ).
  ENDMETHOD.

  METHOD digit_run_cache.
    DATA(scanner) = NEW /apmg/cl_semver_trim_whitespc( ).
    scanner->text = repeat( val = `0` occ = 4096 ) && `.1.x ` && repeat( val = `9` occ = 300 ).
    scanner->length = strlen( scanner->text ).
    cl_abap_unit_assert=>assert_equals(
      act = scanner->find_digits_end( start = 0 limit = /apmg/if_semver_constants=>max_length )
      exp = /apmg/if_semver_constants=>max_length ).
    cl_abap_unit_assert=>assert_equals( act = scanner->digit_runs-end exp = 4096 ).
    DO 4095 TIMES.
      DATA(start) = sy-index.
      cl_abap_unit_assert=>assert_equals(
        act = scanner->find_digits_end( start = start limit = /apmg/if_semver_constants=>max_length )
        exp = nmin( val1 = start + /apmg/if_semver_constants=>max_length val2 = 4096 ) ).
      cl_abap_unit_assert=>assert_equals( act = scanner->digit_runs-start exp = 0 ).
      cl_abap_unit_assert=>assert_equals( act = scanner->digit_runs-end exp = 4096 ).
    ENDDO.
    DATA(second_start) = 4101.
    cl_abap_unit_assert=>assert_equals(
      act = scanner->find_digits_end( start = second_start limit = 256 )
      exp = second_start + 256 ).
    cl_abap_unit_assert=>assert_equals( act = scanner->digit_runs-previous_start exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = scanner->digit_runs-previous_end exp = 4096 ).
    cl_abap_unit_assert=>assert_equals( act = scanner->find_digits_end( start = 1 limit = 256 ) exp = 257 ).
    cl_abap_unit_assert=>assert_equals( act = scanner->digit_runs-start exp = second_start ).

    DATA(zeros) = repeat( val = `0` occ = 4096 ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_trim_whitespc=>trim_comparator_whitespace( zeros )
      exp = zeros ).
    DATA(failed_version) = repeat( val = `0` occ = /apmg/if_semver_constants=>max_length ) && `.1.x `.
    DATA(input) = repeat( val = failed_version occ = 16 ) && `< 1.2.3`.
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_trim_whitespc=>trim_comparator_whitespace( input )
      exp = repeat( val = failed_version occ = 16 ) && `<1.2.3` ).
  ENDMETHOD.

  METHOD independent_matchers.
    DATA(matcher) = /apmg/cl_semver_re=>token-comparatortrim-safe_regex->create_matcher(
      text = `> 1.2.3 < 2.0.0` ).
    cl_abap_unit_assert=>assert_true( matcher->find_next( ) ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_trim_whitespc=>trim_comparator_whitespace( `> 1.2.3` )
      exp = `>1.2.3` ).
    cl_abap_unit_assert=>assert_true( matcher->find_next( ) ).
    cl_abap_unit_assert=>assert_false( matcher->find_next( ) ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_trim_whitespc=>trim_comparator_whitespace( `< 2.0.0` )
      exp = `<2.0.0` ).
  ENDMETHOD.
ENDCLASS.
