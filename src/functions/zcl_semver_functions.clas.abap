CLASS zcl_semver_functions DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS coerce
      IMPORTING
        version       TYPE string
        rtl           TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_semver
      RAISING
        zcx_semver_error.

    CLASS-METHODS compare
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS compare_build
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS compare_loose
      IMPORTING
        a             TYPE any
        b             TYPE any
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
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS gt
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS gte
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS inc
      IMPORTING
        version       TYPE any
        release       TYPE string
        identifier    TYPE string OPTIONAL
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_semver.

    CLASS-METHODS lt
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS lte
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
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
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_semver_error.

    CLASS-METHODS parse
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
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
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    CLASS-METHODS rsort
      IMPORTING
        list          TYPE string_table
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_semver_error.

    CLASS-METHODS sort
      IMPORTING
        list          TYPE string_table
        loose         TYPE abap_bool DEFAULT abap_false
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

    DATA(vers) = condense(
      val = version
      del = | \t\n\r| ).

    vers = replace(
      val   = vers
      regex = '^[=v]+'
      with  = '' ).

    DATA(semver) = parse( version = vers loose = loose incpre = incpre ).

    IF semver IS NOT INITIAL.
      result = semver->version.
    ENDIF.

  ENDMETHOD.


  METHOD cmp.

    CASE op.
      WHEN '==='.
        result = equality( a = a b = b ).
      WHEN '!=='.
        result = xsdbool( NOT equality( a = a b = b ) ).
      WHEN '' OR '=' OR '=='.
        result = eq( a = a b = b loose = loose ).
      WHEN '!=' OR '<>'.
        result = neq( a = a b = b loose = loose ).
      WHEN '>'.
        result = gt( a = a b = b loose = loose ).
      WHEN '>='.
        result = gte( a = a b = b loose = loose ).
      WHEN '<'.
        result = lt( a = a b = b loose = loose ).
      WHEN '<='.
        result = lte( a = a b = b loose = loose ).
      WHEN OTHERS.
        zcx_semver_error=>raise( |Invalid operator: { op }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD coerce.

    DATA(r) = COND #( WHEN rtl = abap_true THEN zcl_semver_re=>re-coercertl ELSE zcl_semver_re=>re-coerce ).

    TRY.
        DATA(m) = r->create_matcher( text = version ).

        IF NOT m->match( ).
          RETURN.
        ENDIF.
      CATCH cx_sy_matcher.
        zcx_semver_error=>raise( |Error evaluating regex for { version }| ).
    ENDTRY.

    IF rtl IS INITIAL.
      TRY.
          DATA(major) = m->get_submatch( 2 ).
        CATCH cx_sy_matcher ##NO_HANDLER.
      ENDTRY.
      TRY.
          DATA(minor) = m->get_submatch( 3 ).
        CATCH cx_sy_matcher ##NO_HANDLER.
      ENDTRY.
      TRY.
          DATA(patch) = m->get_submatch( 4 ).
        CATCH cx_sy_matcher ##NO_HANDLER.
      ENDTRY.
    ELSE.
      zcx_semver_error=>raise( 'Not implemented' ).
    ENDIF.

    IF minor IS INITIAL.
      minor = '0'.
    ENDIF.

    IF patch IS INITIAL.
      patch = '0'.
    ENDIF.

    result = parse( |{ major }.{ minor }.{ patch }| ).

  ENDMETHOD.


  METHOD compare.

    DATA(semver_a) = zcl_semver=>create( version = a loose = loose ).
    DATA(semver_b) = zcl_semver=>create( version = b loose = loose ).

    result = semver_a->compare( semver_b->version ).

  ENDMETHOD.


  METHOD compare_build.

    DATA(semver_a) = zcl_semver=>create( version = a loose = loose ).
    DATA(semver_b) = zcl_semver=>create( version = b loose = loose ).

    result = semver_a->compare( semver_b ).
    IF result = 0.
      result = semver_a->compare_build( semver_b ).
    ENDIF.

  ENDMETHOD.


  METHOD compare_loose.
    result = compare( a = a b = b loose = abap_true ).
  ENDMETHOD.


  METHOD diff.

    IF NOT eq( a = version_1 b = version_2 ).

      DATA(semver_1) = parse( version_1 ).
      DATA(semver_2) = parse( version_2 ).
      DATA(has_pre) = xsdbool( semver_1->prerelease IS NOT INITIAL OR semver_2->prerelease IS NOT INITIAL ).

      IF has_pre = abap_true.
        DATA(prefix) = 'pre'.
        DATA(default_result) = 'prerelease'.
      ENDIF.

      IF semver_1->major <> semver_2->major.
        result = prefix && 'major'.
      ELSEIF semver_1->minor <> semver_2->minor.
        result = prefix && 'minor'.
      ELSEIF semver_1->patch <> semver_2->patch.
        result = prefix && 'patch'.
      ELSE.
        result = default_result. " may be undefined
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD eq.
    result = xsdbool( compare( a = a b = b loose = loose ) = 0 ).
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
    result = xsdbool( compare( a = a b = b loose = loose ) > 0 ).
  ENDMETHOD.


  METHOD gte.
    result = xsdbool( compare( a = a b = b loose = loose ) >= 0 ).
  ENDMETHOD.


  METHOD inc.

    TRY.
        DATA(semver) = zcl_semver=>create( version = version loose = loose incpre = incpre ).

        semver->inc( release = release identifier = identifier ).
      CATCH zcx_semver_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD lt.
    result = xsdbool( compare( a = a b = b loose = loose ) < 0 ).
  ENDMETHOD.


  METHOD lte.
    result = xsdbool( compare( a = a b = b loose = loose ) <= 0 ).
  ENDMETHOD.


  METHOD major.
    DATA(semver) = zcl_semver=>create( version = version loose = loose ).
    result = semver->major.
  ENDMETHOD.


  METHOD minor.
    DATA(semver) = zcl_semver=>create( version = version loose = loose ).
    result = semver->minor.
  ENDMETHOD.


  METHOD neq.
    result = xsdbool( compare( a = a b = b loose = loose ) <> 0 ).
  ENDMETHOD.


  METHOD parse.

    DATA(kind) = cl_abap_typedescr=>describe_by_data( version )->type_kind.

    IF kind = cl_abap_typedescr=>typekind_oref AND version IS INSTANCE OF zcl_semver.
      result ?= version.
      RETURN.
    ENDIF.

    CHECK kind = cl_abap_typedescr=>typekind_char OR kind = cl_abap_typedescr=>typekind_string.

    CHECK strlen( version ) <= zif_semver_constants=>max_length.

    DATA(r) = COND #( WHEN loose = abap_true THEN zcl_semver_re=>re-loose ELSE zcl_semver_re=>re-full ).

    TRY.
        DATA(m) = r->create_matcher( text = version ).

        IF m->match( ).

          TRY.
              result = zcl_semver=>create( version = version loose = loose incpre = incpre ).
            CATCH zcx_semver_error ##NO_HANDLER.
          ENDTRY.

        ENDIF.

      CATCH cx_sy_matcher.
        zcx_semver_error=>raise( |Error evaluating regex for { version }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD patch.
    DATA(semver) = zcl_semver=>create( version = version loose = loose ).
    result = semver->patch.
  ENDMETHOD.


  METHOD prerelease.
    DATA(semver) = parse( version = version loose = loose incpre = incpre ).
    result = semver->prerelease.
  ENDMETHOD.


  METHOD rcompare.
    result = compare( a = b b = a loose = loose ).
  ENDMETHOD.


  METHOD rsort.

    result = list.

    DATA(i) = 1.
    WHILE i < lines( result ).
      DATA(j) = 1.
      WHILE j < lines( result ) - i.
        IF compare_build( b = result[ j ] a = result[ j + 1 ] loose = loose ) < 0.
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

    zcx_semver_error=>raise( 'Not implemented' ).     " TODO

    TRY.
*        DATA(semrange) = zcl_semver=>create_range( range = range opt = opt ).
*
*        result = semrange->test( version ).
      CATCH zcx_semver_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD sort.

    result = list.

    DATA(i) = 1.
    WHILE i < lines( result ).
      DATA(j) = 1.
      WHILE j < lines( result ) - i.
        IF compare_build( a = result[ j ] b = result[ j + 1 ] loose = loose ) < 0.
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

        IF semver IS NOT INITIAL.
          result = semver->version.
        ENDIF.
      CATCH zcx_semver_error.
        result = ''.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
