CLASS zcl_semver_comparator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS any_symbol TYPE string VALUE 'SemVer ANY'.

    DATA:
      operator TYPE string READ-ONLY,
      value    TYPE string READ-ONLY,
      semver   TYPE REF TO zcl_semver READ-ONLY.

    METHODS constructor
      IMPORTING
        comp   TYPE string
        loose  TYPE abap_bool DEFAULT abap_false
        incpre TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_semver_error.

    METHODS get_any
      RETURNING
        VALUE(result) TYPE REF TO zcl_semver
      RAISING
        zcx_semver_error.

    METHODS parse
      IMPORTING
        comp TYPE string
      RAISING
        zcx_semver_error.

    METHODS to_string
      RETURNING
        VALUE(result) TYPE string.

    METHODS test
      IMPORTING
        version       TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS intersects
      IMPORTING
        comp          TYPE REF TO zcl_semver_comparator
        opt           TYPE zif_semver_definitions=>ty_options
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

  PRIVATE SECTION.

    DATA options TYPE zif_semver_definitions=>ty_options.

ENDCLASS.



CLASS zcl_semver_comparator IMPLEMENTATION.


  METHOD constructor.

    options-loose  = loose.
    options-incpre = incpre.

    parse( comp ).

    IF semver = get_any( ).
      value = ''.
    ELSE.
      value = operator && semver->version.
    ENDIF.

  ENDMETHOD.


  METHOD get_any.
    result = NEW zcl_semver( any_symbol ).
  ENDMETHOD.


  METHOD intersects ##TODO.

    IF operator = ''.
      IF value = ''.
        result = abap_true.
      ELSE.
        "result = NEW zcl_range( value = comp->value opt = opt )->test( value ).
      ENDIF.
    ELSEIF comp->operator = ''.
      IF comp->value = ''.
        result = abap_true.
      ELSE.
        "result = NEW zcl_range( value = value opt = opt )->test( comp->value ). " comp->semver?
      ENDIF.
    ELSE.
      DATA(same_direction_increasing) = xsdbool(
        ( operator = '>=' OR operator = '>' ) AND
        ( comp->operator = '>=' OR comp->operator = '>' ) ).
      DATA(same_direction_decreasing) = xsdbool(
        ( operator = '<=' OR operator = '<' ) AND
        ( comp->operator = '<=' OR comp->operator = '<' ) ).
      DATA(same_semver) = xsdbool(
        semver->version = comp->semver->version ).
      DATA(different_directions_inclusive) = xsdbool(
        ( operator = '>=' OR operator = '<=' ) AND
        ( comp->operator = '>=' OR comp->operator = '<=' ) ).
      DATA(opposite_directions_less) = xsdbool(
        zcl_semver_functions=>cmp(
          a     = semver->version
          op    = '<'
          b     = comp->semver->version
          loose = opt-loose ) AND
        ( operator = '>=' OR operator = '>' ) AND
        ( comp->operator = '<=' OR comp->operator = '<' ) ).
      DATA(opposite_directions_greater) = xsdbool(
        zcl_semver_functions=>cmp(
          a     = semver->version
          op    = '>'
          b     = comp->semver->version
          loose = opt-loose ) AND
        ( operator = '<=' OR operator = '<' ) AND
        ( comp->operator = '>=' OR comp->operator = '>' ) ).

      result = xsdbool(
        same_direction_increasing = abap_true OR
        same_direction_decreasing = abap_true OR
        ( same_semver = abap_true AND different_directions_inclusive = abap_true ) OR
        opposite_directions_less = abap_true OR
        opposite_directions_greater = abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD parse.

    DATA(r) = COND #(
      WHEN options-loose = abap_true
      THEN zcl_semver_re=>re-comparatorloose
      ELSE zcl_semver_re=>re-comparator ).

    TRY.
        DATA(m) = r->create_matcher( text = comp ).

        IF NOT m->match( ).
          zcx_semver_error=>raise( |Invalid comparator: { comp }| ).
        ENDIF.

        operator = m->get_submatch( 1 ).

        IF operator = '='.
          operator = ''.
        ENDIF.

        " if it literally is just '>' or '' then allow anything
        IF m->get_submatch( 2 ) IS INITIAL.
          semver = get_any( ).
        ELSE.
          semver = NEW zcl_semver( version = m->get_submatch( 2 ) loose = options-loose ).
        ENDIF.

      CATCH cx_sy_matcher.
        zcx_semver_error=>raise( |Error evaluating regex for { comp }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD test.

    TRY.
        DATA(test_semver) = NEW zcl_semver( version = version loose = options-loose incpre = options-incpre ).

        IF semver = get_any( ) OR test_semver = get_any( ).
          result = abap_true.
        ELSE.
          result = zcl_semver_functions=>cmp(
            a     = semver->version
            op    = operator
            b     = test_semver->version
            loose = options-loose ).
        ENDIF.

      CATCH zcx_semver_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD to_string.
    result = value.
  ENDMETHOD.
ENDCLASS.
