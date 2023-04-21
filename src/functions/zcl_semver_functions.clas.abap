CLASS zcl_semver_functions DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer Functions
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* ABAP Port by Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS clean
      IMPORTING
        version       TYPE string
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_semver_error.

    CLASS-METHODS cmp
      IMPORTING
        a             TYPE any
        op            TYPE string
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS coerce
      IMPORTING
        version       TYPE string
        rtl           TYPE abap_bool DEFAULT abap_false
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_semver
      RAISING
        zcx_semver_error.

    CLASS-METHODS compare
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS compare_build
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS compare_loose
      IMPORTING
        a             TYPE any
        b             TYPE any
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS diff
      IMPORTING
        version_1     TYPE any
        version_2     TYPE any
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_semver_error.

    CLASS-METHODS eq
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS gt
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS gte
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS inc
      IMPORTING
        version         TYPE any
        release         TYPE string
        identifier      TYPE string OPTIONAL
        identifier_base TYPE string OPTIONAL
        loose           TYPE abap_bool DEFAULT abap_false
        incpre          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE REF TO zcl_semver.

    CLASS-METHODS lt
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS lte
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS major
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS minor
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS neq
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS parse
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
        throw_errors  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_semver
      RAISING
        zcx_semver_error.

    CLASS-METHODS patch
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS prerelease
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_semver_error.

    CLASS-METHODS rcompare
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS rsort
      IMPORTING
        list          TYPE string_table
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_semver_error.

    CLASS-METHODS sort
      IMPORTING
        list          TYPE string_table
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_semver_error.

    CLASS-METHODS satisfies
      IMPORTING
        version       TYPE any
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS valid
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS equality
      IMPORTING
        a             TYPE any
        b             TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

ENDCLASS.



CLASS zcl_semver_functions IMPLEMENTATION.


  METHOD clean.

    DATA(vers) = replace(
      val   = zcl_semver_utils=>version_trim( version )
      regex = '^[=v]+'
      with  = '' ).

    DATA(semver) = parse( version = vers loose = loose incpre = incpre ).

    CHECK semver IS BOUND.

    result = semver->version.

  ENDMETHOD.


  METHOD cmp.

    CASE op.
      WHEN '==='.
        result = equality( a = a b = b ).
      WHEN '!=='.
        result = xsdbool( NOT equality( a = a b = b ) ).
      WHEN '' OR '=' OR '=='.
        result = eq( a = a b = b loose = loose incpre = incpre ).
      WHEN '!=' OR '<>'.
        result = neq( a = a b = b loose = loose incpre = incpre ).
      WHEN '>'.
        result = gt( a = a b = b loose = loose incpre = incpre ).
      WHEN '>='.
        result = gte( a = a b = b loose = loose incpre = incpre ).
      WHEN '<'.
        result = lt( a = a b = b loose = loose incpre = incpre ).
      WHEN '<='.
        result = lte( a = a b = b loose = loose incpre = incpre ).
      WHEN OTHERS.
        zcx_semver_error=>raise( |Invalid operator: { op }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD coerce.

    TYPES:
      BEGIN OF ty_match,
        major  TYPE string,
        minor  TYPE string,
        patch  TYPE string,
        offset TYPE i,
        length TYPE i,
        endpos TYPE i,
      END OF ty_match.

    DATA matches TYPE STANDARD TABLE OF ty_match.

    " cl_abap_matcher has a problem with '1.2.3.4.5.6' so we use FIND REGEX

    DATA(r) = COND #(
      WHEN rtl = abap_true
      THEN zcl_semver_re=>token-coercertl-src
      ELSE zcl_semver_re=>token-coerce-src ).

    IF rtl = abap_false.
      FIND REGEX r IN version SUBMATCHES DATA(rest) DATA(major) DATA(minor) DATA(patch).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE.
      " Find the right-most coercible string that does not share
      " a terminus with a more left-ward coercible string.
      " Eg, '1.2.3.4' wants to coerce '2.3.4', not '3.4' or '4'
      DATA(offset) = 0.
      DO.
        FIND REGEX r IN version+offset(*) SUBMATCHES rest major minor patch.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        INSERT INITIAL LINE INTO TABLE matches ASSIGNING FIELD-SYMBOL(<match>).
        <match>-major  = major.
        <match>-minor  = minor.
        <match>-patch  = patch.
        <match>-offset = offset.
        <match>-length = strlen( |{ major }{ COND #( WHEN minor IS NOT INITIAL THEN '.' && minor ) }{ COND #( WHEN patch IS NOT INITIAL THEN '.' && patch ) }| ).
        <match>-endpos = <match>-offset + <match>-length.
        FIND '.' IN version+offset(*) MATCH OFFSET DATA(next_offset).
        offset += next_offset + 1.
        IF offset >= strlen( version ).
          EXIT.
        ENDIF.
      ENDDO.
      SORT matches BY endpos DESCENDING length DESCENDING.
      READ TABLE matches ASSIGNING <match> INDEX 1.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      major = <match>-major.
      minor = <match>-minor.
      patch = <match>-patch.
    ENDIF.

    IF minor IS INITIAL.
      minor = '0'.
    ENDIF.

    IF patch IS INITIAL.
      patch = '0'.
    ENDIF.

    result = parse( version = |{ major }.{ minor }.{ patch }| loose = loose incpre = incpre ).

  ENDMETHOD.


  METHOD compare.

    DATA(semver_a) = zcl_semver=>create( version = a loose = loose incpre = incpre ).
    DATA(semver_b) = zcl_semver=>create( version = b loose = loose incpre = incpre ).

    CHECK semver_a IS BOUND AND semver_b IS BOUND.

    result = semver_a->compare( semver_b ).

  ENDMETHOD.


  METHOD compare_build.

    DATA(semver_a) = zcl_semver=>create( version = a loose = loose incpre = incpre ).
    DATA(semver_b) = zcl_semver=>create( version = b loose = loose incpre = incpre ).

    CHECK semver_a IS BOUND AND semver_b IS BOUND.

    result = semver_a->compare( semver_b ).
    IF result = 0.
      result = semver_a->compare_build( semver_b ).
    ENDIF.

  ENDMETHOD.


  METHOD compare_loose.
    result = compare( a = a b = b loose = abap_true incpre = incpre ).
  ENDMETHOD.


  METHOD diff.

    DATA(v1) = parse( version = version_1 throw_errors = abap_true ).
    DATA(v2) = parse( version = version_2 throw_errors = abap_true ).

    DATA(comparison) = v1->compare( v2 ).

    IF comparison = 0.
      RETURN.
    ENDIF.

    DATA(v1_higher)    = xsdbool( comparison > 0 ).
    DATA(high_version) = COND #( WHEN v1_higher = abap_true THEN v1 ELSE v2 ).
    DATA(low_version)  = COND #( WHEN v1_higher = abap_true THEN v2 ELSE v1 ).
    DATA(high_has_pre) = xsdbool( high_version->prerelease IS NOT INITIAL ).

    " add the `pre` prefix if we are going to a prerelease version
    DATA(prefix) = COND #( WHEN high_has_pre = abap_true THEN 'pre' ELSE '' ).

    IF v1->major <> v2->major.
      result = prefix && 'major'.
      RETURN.
    ENDIF.

    IF v1->minor <> v2->minor.
      result = prefix && 'minor'.
      RETURN.
    ENDIF.

    IF v1->patch <> v2->patch.
      result = prefix && 'patch'.
      RETURN.
    ENDIF.

    " at this point we know stable versions match but overall versions are not equal,
    " so either they are both prereleases, or the lower version is a prerelease

    IF high_has_pre = abap_true.
      " high and low are preleases
      result = 'prerelease'.
      RETURN.
    ENDIF.

    IF low_version->patch IS NOT INITIAL.
      " anything higher than a patch bump would result in the wrong version
      result = 'patch'.
      RETURN.
    ENDIF.

    IF low_version->minor IS NOT INITIAL.
      " anything higher than a minor bump would result in the wrong version
      result = 'minor'.
      RETURN.
    ENDIF.

    " bumping major/minor/patch all have same result
    result = 'major'.

  ENDMETHOD.


  METHOD eq.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) = 0 ).
  ENDMETHOD.


  METHOD equality.

    DATA semver_a TYPE REF TO zcl_semver.
    DATA semver_b TYPE REF TO zcl_semver.

    IF a IS BOUND AND a IS INSTANCE OF zcl_semver AND b IS BOUND AND b IS INSTANCE OF zcl_semver.
      semver_a ?= a.
      semver_b ?= b.
      result = xsdbool( semver_a->version = semver_b->version ).
    ELSE.
      zcx_semver_error=>raise( |Invalid parameter type| ).
    ENDIF.

  ENDMETHOD.


  METHOD gt.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) > 0 ).
  ENDMETHOD.


  METHOD gte.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) >= 0 ).
  ENDMETHOD.


  METHOD inc.

    DATA semver TYPE REF TO zcl_semver.

    TRY.
        " Create new semver object
        DATA(kind) = cl_abap_typedescr=>describe_by_data( version )->type_kind.

        IF kind = cl_abap_typedescr=>typekind_oref AND version IS INSTANCE OF zcl_semver.
          semver ?= version.
          result = zcl_semver=>create( version = semver->version loose = loose incpre = incpre ).
        ELSE.
          result = zcl_semver=>create( version = version loose = loose incpre = incpre ).
        ENDIF.

        CHECK result IS BOUND.

        result->inc( release = release identifier = identifier identifier_base = identifier_base ).
      CATCH zcx_semver_error.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.


  METHOD lt.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) < 0 ).
  ENDMETHOD.


  METHOD lte.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) <= 0 ).
  ENDMETHOD.


  METHOD major.

    DATA(semver) = zcl_semver=>create( version = version loose = loose ).

    CHECK semver IS BOUND.

    result = semver->major.

  ENDMETHOD.


  METHOD minor.

    DATA(semver) = zcl_semver=>create( version = version loose = loose ).

    CHECK semver IS BOUND.

    result = semver->minor.

  ENDMETHOD.


  METHOD neq.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) <> 0 ).
  ENDMETHOD.


  METHOD parse.

    DATA(kind) = cl_abap_typedescr=>describe_by_data( version )->type_kind.

    IF kind = cl_abap_typedescr=>typekind_oref AND version IS INSTANCE OF zcl_semver.
      result ?= version.
      RETURN.
    ENDIF.

    TRY.
        result = zcl_semver=>create( version = version loose = loose incpre = incpre ).
      CATCH zcx_semver_error INTO DATA(error).
        IF throw_errors = abap_false.
          RETURN.
        ENDIF.

        RAISE EXCEPTION error.
    ENDTRY.

  ENDMETHOD.


  METHOD patch.

    DATA(semver) = zcl_semver=>create( version = version loose = loose ).

    CHECK semver IS BOUND.

    result = semver->patch.

  ENDMETHOD.


  METHOD prerelease.

    DATA(semver) = parse( version = version loose = loose incpre = incpre ).

    CHECK semver IS BOUND.

    result = semver->prerelease.

  ENDMETHOD.


  METHOD rcompare.
    result = compare( a = b b = a loose = loose incpre = incpre ).
  ENDMETHOD.


  METHOD rsort.

    result = list.

    DATA(i) = 1.
    WHILE i < lines( result ).
      DATA(j) = 1.
      WHILE j <= lines( result ) - i.
        IF compare_build( b = result[ j ] a = result[ j + 1 ] loose = loose incpre = incpre ) > 0.
          DATA(temp)      = result[ j ].
          result[ j ]     = result[ j + 1 ].
          result[ j + 1 ] = temp.
        ENDIF.
        j += 1.
      ENDWHILE.
      i += 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD satisfies.

    TRY.
        DATA(semrange) = zcl_semver_range=>create( range = range loose = loose incpre = incpre ).

        IF semrange IS BOUND.
          result = semrange->test( version ).
        ENDIF.
      CATCH zcx_semver_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD sort.

    result = list.

    DATA(i) = 1.
    WHILE i < lines( result ).
      DATA(j) = 1.
      WHILE j <= lines( result ) - i.
        IF compare_build( a = result[ j ] b = result[ j + 1 ] loose = loose incpre = incpre ) > 0.
          DATA(temp)      = result[ j ].
          result[ j ]     = result[ j + 1 ].
          result[ j + 1 ] = temp.
        ENDIF.
        j += 1.
      ENDWHILE.
      i += 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD valid.

    TRY.
        DATA(semver) = parse( version = version loose = loose incpre = incpre ).

        CHECK semver IS BOUND.

        result = semver->version.
      CATCH zcx_semver_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
