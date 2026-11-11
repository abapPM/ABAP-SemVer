CLASS ltcl_semver_constants DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      types FOR TESTING RAISING /apmg/cx_error,
      whitespace FOR TESTING RAISING /apmg/cx_error.

    METHODS get_type
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE abap_typekind.

ENDCLASS.

CLASS ltcl_semver_constants IMPLEMENTATION.

  METHOD get_type.
    result = cl_abap_typedescr=>describe_by_data( val )->type_kind.
  ENDMETHOD.

  METHOD types.

    cl_abap_unit_assert=>assert_equals(
      act = get_type( /apmg/if_semver_constants=>max_length )
      exp = cl_abap_typedescr=>typekind_int ).

    cl_abap_unit_assert=>assert_equals(
      act = get_type( /apmg/if_semver_constants=>max_safe_integer )
      exp = cl_abap_typedescr=>typekind_int ).

    cl_abap_unit_assert=>assert_equals(
      act = get_type( /apmg/if_semver_constants=>max_safe_component_length )
      exp = cl_abap_typedescr=>typekind_int ).

    cl_abap_unit_assert=>assert_equals(
      act = get_type( /apmg/if_semver_constants=>max_safe_build_length )
      exp = cl_abap_typedescr=>typekind_int ).

    cl_abap_unit_assert=>assert_equals(
      act = get_type( /apmg/if_semver_constants=>semver_spec_version )
      exp = cl_abap_typedescr=>typekind_string ).

    cl_abap_unit_assert=>assert_equals(
      act = get_type( /apmg/if_semver_constants=>release_types )
      exp = cl_abap_typedescr=>typekind_struct2 ).

  ENDMETHOD.

  METHOD whitespace.

    TYPES ty_int TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    DATA act TYPE ty_int.

    DATA(code) = 0.
    DO 65536 TIMES.
      IF code <> 32.
        DATA(char) = CONV string( cl_abap_conv_in_ce=>uccpi( code ) ).
      ELSE.
        char = ` `.
      ENDIF.

      IF match( val = char pcre = '\s' ) = char.
        INSERT code INTO TABLE act.
      ENDIF.

      code = code + 1.
    ENDDO.

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = /apmg/cl_semver_trim_whitespc=>whitespace_characters ).

  ENDMETHOD.
ENDCLASS.

CLASS ltcl_semver_re DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      semver FOR TESTING RAISING /apmg/cx_error,
      comparator FOR TESTING RAISING /apmg/cx_error.

ENDCLASS.

CLASS ltcl_semver_re IMPLEMENTATION.

  METHOD semver.
    " Semver itself has a list of src, re, and tokens
    " Skipped since mostly irrelevant for ABAP
  ENDMETHOD.

  METHOD comparator.
    " Comparator trim regexes stay exported but are not used by range parsing
    " Skipped since mostly irrelevant for ABAP
  ENDMETHOD.
ENDCLASS.
