CLASS ltcl_semver_re DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    TYPES ty_options TYPE STANDARD TABLE OF /apmg/if_semver_options=>ty_options WITH EMPTY KEY.

    METHODS:
      test_src FOR TESTING,
      test_regex FOR TESTING,
      test_occ FOR TESTING,
      comparator_trim_exports FOR TESTING RAISING /apmg/cx_error.

ENDCLASS.

CLASS /apmg/cl_semver_re DEFINITION LOCAL FRIENDS ltcl_semver_re.

CLASS ltcl_semver_re IMPLEMENTATION.

  METHOD test_src.
    " Semver itself has a list of src

    DATA i TYPE i.

    FIELD-SYMBOLS <token> TYPE /apmg/cl_semver_re=>ty_token.

    DO.
      i = i + 1.
      ASSIGN COMPONENT i OF STRUCTURE /apmg/cl_semver_re=>token TO <token>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      cl_abap_unit_assert=>assert_not_initial(
        act = <token>-src
        msg = |Regex component #{ i } must not be initial| ).

      cl_abap_unit_assert=>assert_equals(
        act = <token>-src
        exp = <token>-regex->pattern
        msg = |Regex component #{ i } does not match source| ).

      cl_abap_unit_assert=>assert_equals(
        act = <token>-safe_src
        exp = <token>-safe_regex->pattern
        msg = |Regex component #{ i } does not match source| ).

      IF <token>-safe_src CS '\s+'.
        cl_abap_unit_assert=>fail( msg = |Regex component #{ i } must not contain greedy whitespace| ).
      ENDIF.

      IF <token>-safe_src CS '\s*'.
        cl_abap_unit_assert=>fail( msg = |Regex component #{ i } must not contain greedy whitespace| ).
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD test_regex.
    " has a list of valid regex

    DATA i TYPE i.
    DATA regex TYPE REF TO cl_abap_regex.

    FIELD-SYMBOLS <token> TYPE /apmg/cl_semver_re=>ty_token.

    DO.
      i = i + 1.
      ASSIGN COMPONENT i OF STRUCTURE /apmg/cl_semver_re=>token TO <token>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      TRY.
          regex = <token>-regex.
          regex->create_matcher( text = '1.2.3' ).
        CATCH cx_root.
          cl_abap_unit_assert=>fail( msg = |Error processing regex component #{ i }| ).
      ENDTRY.
    ENDDO.

  ENDMETHOD.

  METHOD test_occ.
    " either 0 or 1

    DATA i TYPE i.

    FIELD-SYMBOLS <token> TYPE /apmg/cl_semver_re=>ty_token.

    DO.
      i = i + 1.
      ASSIGN COMPONENT i OF STRUCTURE /apmg/cl_semver_re=>token TO <token>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      cl_abap_unit_assert=>assert_number_between(
        number = <token>-occ
        lower  = 0
        upper  = 1
        msg    = |Occurence of component #{ i } must be either 0 or 1| ).
    ENDDO.

  ENDMETHOD.


  METHOD comparator_trim_exports.
    DATA(token) = /apmg/cl_semver_re=>token-comparatortrim.

    cl_abap_unit_assert=>assert_equals(
      act = token-src
      exp = token-regex->pattern
      msg = 'Comparator trim regex matches its exported source' ).
    cl_abap_unit_assert=>assert_equals(
      act = token-safe_src
      exp = token-safe_regex->pattern
      msg = 'Safe comparator trim regex matches its exported source' ).
    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_re=>comparator_trim_replace
      exp = '$1$2$3'
      msg = 'Comparator trim replacement remains exported' ).

    DATA(matcher) = token-regex->create_matcher( text = '> 1.2.3' ).
    cl_abap_unit_assert=>assert_equals(
      act = matcher->replace_all( /apmg/cl_semver_re=>comparator_trim_replace )
      exp = 1
      msg = 'Exported comparator trim regex remains usable' ).
    cl_abap_unit_assert=>assert_equals(
      act = matcher->text
      exp = '>1.2.3'
      msg = 'Exported comparator trim regex still removes comparator whitespace' ).

    DATA(safe_matcher) = token-safe_regex->create_matcher( text = '> 1.2.3' ).
    cl_abap_unit_assert=>assert_equals(
      act = safe_matcher->replace_all( /apmg/cl_semver_re=>comparator_trim_replace )
      exp = 1
      msg = 'Exported safe comparator trim regex remains usable' ).
    cl_abap_unit_assert=>assert_equals(
      act = safe_matcher->text
      exp = '>1.2.3'
      msg = 'Exported safe comparator trim regex still removes comparator whitespace' ).

    DATA(original_regex) = /apmg/cl_semver_re=>token-comparatortrim-regex.
    DATA(original_safe_regex) = /apmg/cl_semver_re=>token-comparatortrim-safe_regex.
    DATA(options_list) = VALUE ty_options(
      ( )
      ( loose = abap_true )
      ( incpre = abap_true )
      ( loose = abap_true incpre = abap_true ) ).

    TRY.
        CLEAR:
          /apmg/cl_semver_re=>token-comparatortrim-regex,
          /apmg/cl_semver_re=>token-comparatortrim-safe_regex.
        LOOP AT options_list INTO DATA(options).
          /apmg/cl_semver_range=>clear_cache( ).
          DATA(range) = /apmg/cl_semver_range=>create(
            range  = '>= 1.2.3 < 2.0.0'
            loose  = options-loose
            incpre = options-incpre ).
          cl_abap_unit_assert=>assert_equals(
            act = range->range( )
            exp = '>=1.2.3 <2.0.0'
            msg = |Range parsing works with loose={ options-loose } incpre={ options-incpre }| ).
          cl_abap_unit_assert=>assert_true(
            act = range->test( '1.5.0' )
            msg = |Range membership works with loose={ options-loose } incpre={ options-incpre }| ).
        ENDLOOP.
      CLEANUP.
        /apmg/cl_semver_re=>token-comparatortrim-regex = original_regex.
        /apmg/cl_semver_re=>token-comparatortrim-safe_regex = original_safe_regex.
        /apmg/cl_semver_range=>clear_cache( ).
    ENDTRY.
    /apmg/cl_semver_re=>token-comparatortrim-regex = original_regex.
    /apmg/cl_semver_re=>token-comparatortrim-safe_regex = original_safe_regex.
    /apmg/cl_semver_range=>clear_cache( ).
  ENDMETHOD.

ENDCLASS.
