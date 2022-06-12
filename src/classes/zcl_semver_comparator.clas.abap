CLASS zcl_semver_comparator DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-DATA any_semver TYPE REF TO zcl_semver.

    DATA:
      operator TYPE string READ-ONLY,
      value    TYPE string READ-ONLY,
      semver   TYPE REF TO zcl_semver READ-ONLY.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING
        comp   TYPE string
        loose  TYPE abap_bool DEFAULT abap_false
        incpre TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_semver_error.

    CLASS-METHODS create
      IMPORTING
        comp          TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_semver_comparator
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
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

  PRIVATE SECTION.

    DATA options TYPE zif_semver_definitions=>ty_options.

ENDCLASS.



CLASS zcl_semver_comparator IMPLEMENTATION.


  METHOD class_constructor.

    TRY.
        any_semver = zcl_semver=>create( '9999.9999.9999' ).
      CATCH zcx_semver_error ##NO_HANDLER.
    ENDTRY.

    " any_semver must be valid
    ASSERT any_semver IS BOUND.

  ENDMETHOD.


  METHOD constructor.

    options-loose  = loose.
    options-incpre = incpre.

    parse( comp ).

    IF semver = any_semver.
      value = ''.
    ELSE.
      value = operator && semver->version.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA(kind) = cl_abap_typedescr=>describe_by_data( comp )->type_kind.

    IF kind = cl_abap_typedescr=>typekind_oref AND comp IS INSTANCE OF zcl_semver_comparator.

      result = comp.

      IF result->options-loose = loose AND result->options-incpre = incpre.
        RETURN.
      ENDIF.

      result = NEW zcl_semver_comparator( comp = |{ result->value }| loose = loose incpre = incpre ).

    ELSEIF kind = cl_abap_typedescr=>typekind_char OR kind = cl_abap_typedescr=>typekind_string.

      result = NEW zcl_semver_comparator( comp = |{ comp }| loose = loose incpre = incpre ).

    ELSE.
      zcx_semver_error=>raise( 'Invalid parameter type' ).
    ENDIF.

  ENDMETHOD.


  METHOD intersects ##TODO.

    zcx_semver_error=>raise( 'Not implemented' ).

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
          loose = loose ) AND
        ( operator = '>=' OR operator = '>' ) AND
        ( comp->operator = '<=' OR comp->operator = '<' ) ).
      DATA(opposite_directions_greater) = xsdbool(
        zcl_semver_functions=>cmp(
          a     = semver->version
          op    = '>'
          b     = comp->semver->version
          loose = loose ) AND
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
          semver = any_semver.
        ELSE.
          semver = zcl_semver=>create( version = m->get_submatch( 2 ) loose = options-loose incpre = options-incpre ).
        ENDIF.

      CATCH cx_sy_matcher.
        zcx_semver_error=>raise( |Error evaluating regex for { comp }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD test.

    TRY.
        DATA(testver) = zcl_semver=>create( version = version loose = options-loose incpre = options-incpre ).

        IF semver = any_semver OR testver = any_semver.
          result = abap_true.
        ELSE.
          result = zcl_semver_functions=>cmp(
            a     = testver->version
            op    = operator
            b     = semver->version
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
