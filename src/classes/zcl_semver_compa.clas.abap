CLASS zcl_semver_compa DEFINITION
  PUBLIC
  CREATE PRIVATE.

************************************************************************
* SemVer Comparator
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* ABAP Port by Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    CLASS-DATA any_semver TYPE REF TO zcl_semver.

    DATA:
      operator TYPE string READ-ONLY,
      value    TYPE string READ-ONLY,
      semver   TYPE REF TO zcl_semver READ-ONLY.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING
        !comp   TYPE string
        !loose  TYPE abap_bool DEFAULT abap_false
        !incpre TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_semver_error.

    CLASS-METHODS create
      IMPORTING
        !comp         TYPE any
        !loose        TYPE abap_bool DEFAULT abap_false
        !incpre       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_semver_compa
      RAISING
        zcx_semver_error.

    METHODS parse
      IMPORTING
        !comp TYPE string
      RAISING
        zcx_semver_error.

    METHODS to_string
      RETURNING
        VALUE(result) TYPE string.

    METHODS test
      IMPORTING
        !version      TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS intersects
      IMPORTING
        !comp         TYPE any
        !loose        TYPE abap_bool DEFAULT abap_false
        !incpre       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA options TYPE zif_semver_opts=>ty_options.

ENDCLASS.



CLASS zcl_semver_compa IMPLEMENTATION.


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

    parse( zcl_semver_utils=>trim( comp ) ).

    IF semver = any_semver.
      value = ''.
    ELSE.
      value = operator && semver->version.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA(kind) = cl_abap_typedescr=>describe_by_data( comp )->type_kind.

    IF kind = cl_abap_typedescr=>typekind_oref AND comp IS INSTANCE OF zcl_semver_compa.

      result = comp.

      IF result->options-loose = loose AND result->options-incpre = incpre.
        RETURN.
      ENDIF.

      result = NEW zcl_semver_compa( comp = |{ result->value }| loose = loose incpre = incpre ).

    ELSEIF kind = cl_abap_typedescr=>typekind_char OR kind = cl_abap_typedescr=>typekind_string.

      result = NEW zcl_semver_compa( comp = |{ comp }| loose = loose incpre = incpre ).

    ELSE.
      zcx_semver_error=>raise( 'Invalid parameter type' ).
    ENDIF.

  ENDMETHOD.


  METHOD intersects.

    IF comp IS INITIAL.
      zcx_semver_error=>raise( 'A comparator is required' ).
    ENDIF.

    DATA(semcomp) = zcl_semver_compa=>create( comp ).

    CHECK semcomp IS BOUND.

    IF operator = ''.
      IF value = ''.
        result = abap_true.
      ELSE.
        DATA(semrange) = zcl_semver_range=>create( range = semcomp->value loose = loose incpre = incpre ).

        CHECK semrange IS BOUND.

        result = semrange->test( value ).
      ENDIF.
    ELSEIF semcomp->operator = ''.
      IF semcomp->value = ''.
        result = abap_true.
      ELSE.
        semrange = zcl_semver_range=>create( range = value loose = loose incpre = incpre ).

        CHECK semrange IS BOUND.

        result = semrange->test( semcomp->semver ).
      ENDIF.
    ELSE.
      " Special cases where nothing can possibly be lower
      IF incpre = abap_true AND value = '<0.0.0-0' OR semcomp->value = '<0.0.0-0'.
        result = abap_false.
        RETURN.
      ENDIF.
      IF incpre = abap_false AND value CP '<0.0.0*' OR semcomp->value CP '<0.0.0*'.
        result = abap_false.
        RETURN.
      ENDIF.

      " Same direction increasing (> or >=)
      IF operator CP '>*' AND semcomp->operator CP '>*'.
        result = abap_true.
        RETURN.
      ENDIF.
      " Same direction decreasing (< or <=)
      IF operator CP '<*' AND semcomp->operator CP '<*'.
        result = abap_true.
        RETURN.
      ENDIF.

      " same SemVer and both sides are inclusive (<= or >=)
      IF semver->version = semcomp->semver->version AND operator CA '=' AND semcomp->operator CA '='.
        result = abap_true.
        RETURN.
      ENDIF.

      " opposite directions less than
      IF zcl_semver_funct=>cmp(
        a      = semver->version
        op     = '<'
        b      = semcomp->semver->version
        loose  = loose
        incpre = incpre ) AND operator CP '>*' AND semcomp->operator CP '<*'.
        result = abap_true.
        RETURN.
      ENDIF.
      " opposite directions greater than
      IF zcl_semver_funct=>cmp(
        a      = semver->version
        op     = '>'
        b      = semcomp->semver->version
        loose  = loose
        incpre = incpre ) AND operator CP '<*' AND semcomp->operator CP '>*'.
        result = abap_true.
        RETURN.
      ENDIF.
      result = abap_false.


*      DATA(same_direction_increasing) = xsdbool(
*      ( operator = '>=' OR operator = '>' ) AND
*      ( semcomp->operator = '>=' OR semcomp->operator = '>' ) ).
*      DATA(same_direction_decreasing) = xsdbool(
*      ( operator = '<=' OR operator = '<' ) AND
*      ( semcomp->operator = '<=' OR semcomp->operator = '<' ) ).
*      DATA(same_semver) = xsdbool(
*      semver->version = semcomp->semver->version ).
*      DATA(different_directions_inclusive) = xsdbool(
*      ( operator = '>=' OR operator = '<=' ) AND
*      ( semcomp->operator = '>=' OR semcomp->operator = '<=' ) ).
*      DATA(opposite_directions_less) = xsdbool(
*      zcl_semver_funct=>cmp(
*      a     = semver->version
*      op    = '<'
*      b     = semcomp->semver->version
*      loose = loose ) AND
*      ( operator = '>=' OR operator = '>' ) AND
*      ( semcomp->operator = '<=' OR semcomp->operator = '<' ) ).
*      DATA(opposite_directions_greater) = xsdbool(
*      zcl_semver_funct=>cmp(
*      a     = semver->version
*      op    = '>'
*      b     = semcomp->semver->version
*      loose = loose ) AND
*      ( operator = '<=' OR operator = '<' ) AND
*      ( semcomp->operator = '>=' OR semcomp->operator = '>' ) ).
*
*      result = xsdbool(
*      same_direction_increasing = abap_true OR
*      same_direction_decreasing = abap_true OR
*      ( same_semver = abap_true AND different_directions_inclusive = abap_true ) OR
*      opposite_directions_less = abap_true OR
*      opposite_directions_greater = abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD parse.

    " initial comparator means anything is allowed
    IF comp IS INITIAL.
      semver = any_semver.
      RETURN.
    ENDIF.

    DATA(r) = COND #(
      WHEN options-loose = abap_true
      THEN zcl_semver_re=>token-comparatorloose-safe_regex
      ELSE zcl_semver_re=>token-comparator-safe_regex ).

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
          result = zcl_semver_funct=>cmp(
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
