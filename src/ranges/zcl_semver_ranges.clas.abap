CLASS zcl_semver_ranges DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer Ranges
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* ABAP Port by Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    TYPES: ty_hilo TYPE c LENGTH 1.

    CLASS-METHODS gtr
      IMPORTING
        version       TYPE any
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS intersects
      IMPORTING
        r1            TYPE any
        r2            TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS ltr
      IMPORTING
        version       TYPE any
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS outside
      IMPORTING
        version       TYPE any
        range         TYPE any
        hilo          TYPE ty_hilo OPTIONAL
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS to_comparators
      IMPORTING
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE zcl_semver_range=>ty_comparators
      RAISING
        zcx_semver_error.

    CLASS-METHODS valid_range
      IMPORTING
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_semver_ranges IMPLEMENTATION.


  METHOD gtr.
    result = outside( version = version range = range hilo = '>' loose = loose ).
  ENDMETHOD.


  METHOD intersects.

    DATA(semrange1) = zcl_semver_range=>create( range = r1 loose = loose incpre = incpre ).

    DATA(semrange2) = zcl_semver_range=>create( range = r2 loose = loose incpre = incpre ).

    result = semrange1->intersects( semrange2 ).

  ENDMETHOD.


  METHOD ltr.
    result = outside( version = version range = range hilo = '<' loose = loose ).
  ENDMETHOD.


  METHOD outside.

    DATA:
      comp  TYPE string,
      ecomp TYPE string,
      high  TYPE REF TO zcl_semver_comparator,
      low   TYPE REF TO zcl_semver_comparator.

    DATA(semver) = zcl_semver=>create( version = version loose = loose incpre = incpre ).

    DATA(semrange) = zcl_semver_range=>create( range = range loose = loose incpre = incpre ).

    IF hilo NA '<>'.
      zcx_semver_error=>raise( 'Must provide a hilo val of "<" or ">"' ).
    ENDIF.

    comp = hilo.
    ecomp = comp && '='.

    " If it satisfies the range it is not outside
    IF zcl_semver_functions=>satisfies( version = semver range = semrange loose = loose incpre = incpre ).
      result = abap_false.
      RETURN.
    ENDIF.

    LOOP AT semrange->set ASSIGNING FIELD-SYMBOL(<set>).
      DATA(comparators) = <set>.

      CLEAR: high, low.

      LOOP AT comparators ASSIGNING FIELD-SYMBOL(<comparator>).
        IF <comparator> = zcl_semver_comparator=>any_semver.
          <comparator> = zcl_semver_comparator=>create( '>=0.0.0' ).
        ENDIF.

        IF high IS NOT BOUND.
          high = <comparator>.
        ENDIF.
        IF low IS NOT BOUND.
          low = <comparator>.
        ENDIF.

        CASE hilo.
          WHEN '>'.
            IF zcl_semver_functions=>gt( a = <comparator>->semver b = high->semver loose = loose ).
              high = <comparator>.
            ELSEIF zcl_semver_functions=>lt( a = <comparator>->semver b = low->semver loose = loose ).
              low = <comparator>.
            ENDIF.
          WHEN '<'.
            IF zcl_semver_functions=>lt( a = <comparator>->semver b = high->semver loose = loose ).
              high = <comparator>.
            ELSEIF zcl_semver_functions=>gt( a = <comparator>->semver b = low->semver loose = loose ).
              low = <comparator>.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      " If the edge version comparator has a operator then our version isn't outside it
      IF high->operator = comp OR high->operator = ecomp.
        result = abap_false.
        RETURN.
      ENDIF.

      " If the lowest version comparator has an operator and our version
      " is less than it then it isn't higher than the range
      CASE hilo.
        WHEN '>'.
          IF ( low->operator IS NOT INITIAL OR low->operator = comp ) AND zcl_semver_functions=>lte( a = semver b =  low->semver ).
            result = abap_false.
            RETURN.
          ELSEIF low->operator = ecomp AND zcl_semver_functions=>lt( a = semver b = low->semver ).
            result = abap_false.
            RETURN.
          ENDIF.
        WHEN '<'.
          IF ( low->operator IS NOT INITIAL OR low->operator = comp ) AND zcl_semver_functions=>gte( a = semver b =  low->semver ).
            result = abap_false.
            RETURN.
          ELSEIF low->operator = ecomp AND zcl_semver_functions=>gt( a = semver b = low->semver ).
            result = abap_false.
            RETURN.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    result = abap_true.

  ENDMETHOD.


  METHOD to_comparators.
    " Mostly just for testing and legacy API reasons

    DATA(semrange) = zcl_semver_range=>create( range = range loose = loose incpre = incpre ).

    LOOP AT semrange->set ASSIGNING FIELD-SYMBOL(<set>).
      LOOP AT <set> ASSIGNING FIELD-SYMBOL(<comparator>).
        INSERT <comparator> INTO TABLE result.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD valid_range.

    TRY.
        DATA(semrange) = zcl_semver_range=>create( range = range loose = loose incpre = incpre ).
        result = abap_true.
      CATCH zcx_semver_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
