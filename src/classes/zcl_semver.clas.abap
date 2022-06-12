CLASS zcl_semver DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    DATA:
      version    TYPE string READ-ONLY,
      major      TYPE i READ-ONLY,
      minor      TYPE i READ-ONLY,
      patch      TYPE i READ-ONLY,
      prerelease TYPE string_table READ-ONLY,
      build      TYPE string_table READ-ONLY.

    METHODS constructor
      IMPORTING
        version TYPE string
        loose   TYPE abap_bool DEFAULT abap_false
        incpre  TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_semver_error.

    CLASS-METHODS create
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_semver
      RAISING
        zcx_semver_error.

    METHODS format
      RETURNING
        VALUE(result) TYPE string.

    METHODS to_string
      RETURNING
        VALUE(result) TYPE string.

    METHODS compare
      IMPORTING
        other         TYPE string
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    METHODS compare_main
      IMPORTING
        other         TYPE string
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    METHODS compare_pre
      IMPORTING
        other         TYPE string
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    METHODS compare_build
      IMPORTING
        other         TYPE string
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_semver_error.

    METHODS inc
      IMPORTING
        release       TYPE string
        identifier    TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_semver
      RAISING
        zcx_semver_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      raw     TYPE string,
      options TYPE zif_semver_definitions=>ty_options.

ENDCLASS.



CLASS zcl_semver IMPLEMENTATION.


  METHOD compare.

    DATA(semver) = zcl_semver=>create( version = other loose = options-loose incpre = options-incpre ).

    IF semver->version = version.
      result = 0.
    ELSE.
      DATA(comp_main) = compare_main( other ).

      IF comp_main = 0.
        result = compare_pre( other ).
      ELSE.
        result = comp_main.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD compare_build.

    DATA(semver) = zcl_semver=>create( version = other loose = options-loose incpre = options-incpre ).

    DATA(i) = 1.
    DO.
      DATA(a) = VALUE #( build[ i ] DEFAULT `` ).
      DATA(b) = VALUE #( semver->build[ i ] DEFAULT `` ).
      IF a IS INITIAL AND b IS INITIAL.
        result = 0.
        RETURN.
      ELSEIF b IS INITIAL.
        result = +1.
        RETURN.
      ELSEIF a IS INITIAL.
        result = -1.
        RETURN.
      ELSEIF a <> b.
        result = zcl_semver_identifiers=>compare_identifiers( a = a b = b ).
        RETURN.
      ENDIF.
      i += 1.
    ENDDO.

  ENDMETHOD.


  METHOD compare_main.

    DATA(semver) = zcl_semver=>create( version = other loose = options-loose incpre = options-incpre ).

    result = zcl_semver_identifiers=>compare_identifiers( a = major b = semver->major ).
    IF result = 0.
      result = zcl_semver_identifiers=>compare_identifiers( a = minor b = semver->minor ).
      IF result = 0.
        result = zcl_semver_identifiers=>compare_identifiers( a = patch b = semver->patch ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD compare_pre.

    DATA(semver) = zcl_semver=>create( version = other loose = options-loose incpre = options-incpre ).

    " NOT having a prerelease is > having one
    IF prerelease IS NOT INITIAL AND semver->prerelease IS INITIAL.
      result = -1.
    ELSEIF prerelease IS INITIAL AND semver->prerelease IS NOT INITIAL.
      result = +1.
    ELSEIF prerelease IS INITIAL AND semver->prerelease IS INITIAL.
      result = 0.
    ELSE.
      DATA(i) = 1.
      DO.
        DATA(a) = VALUE #( prerelease[ i ] DEFAULT `` ).
        DATA(b) = VALUE #( semver->prerelease[ i ] DEFAULT `` ).
        IF a IS INITIAL AND b IS INITIAL.
          result = 0.
          RETURN.
        ELSEIF b IS INITIAL.
          result = +1.
          RETURN.
        ELSEIF a IS INITIAL.
          result = -1.
          RETURN.
        ELSEIF a <> b.
          result = zcl_semver_identifiers=>compare_identifiers( a = a b = b ).
          RETURN.
        ENDIF.
        i += 1.
      ENDDO.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    IF strlen( version ) > zif_semver_constants=>max_length.
      zcx_semver_error=>raise( |Version is longer than { zif_semver_constants=>max_length } characters| ).
    ENDIF.

    options-loose  = loose.
    options-incpre = incpre.

    DATA(r) = COND #( WHEN loose = abap_true THEN zcl_semver_re=>re-loose ELSE zcl_semver_re=>re-full ).

    TRY.
        DATA(m) = r->create_matcher( text = condense( version ) ).

        IF NOT m->match( ).
          zcx_semver_error=>raise( |Invalid version { version }| ).
        ENDIF.

        raw = version.

        " these are actually numbers
        DATA(major_num) = CONV decfloat34( m->get_submatch( 1 ) ).
        DATA(minor_num) = CONV decfloat34( m->get_submatch( 2 ) ).
        DATA(patch_num) = CONV decfloat34( m->get_submatch( 3 ) ).

        IF major_num BETWEEN 0 AND zif_semver_constants=>max_safe_integer.
          major = major_num.
        ELSE.
          zcx_semver_error=>raise( |Invalid major version { major_num }| ).
        ENDIF.

        IF minor_num BETWEEN 0 AND zif_semver_constants=>max_safe_integer.
          minor = minor_num.
        ELSE.
          zcx_semver_error=>raise( |Invalid minor version { minor_num }| ).
        ENDIF.

        IF patch_num BETWEEN 0 AND zif_semver_constants=>max_safe_integer.
          patch = patch_num.
        ELSE.
          zcx_semver_error=>raise( |Invalid patch version { patch_num }| ).
        ENDIF.

        DATA(m4) = m->get_submatch( 4 ).
        IF m4 IS NOT INITIAL.
          IF m4 CS '.'.
            SPLIT m4 AT '.' INTO TABLE prerelease.
          ELSE.
            prerelease = VALUE #( ( m4 ) ).
          ENDIF.
        ENDIF.

        DATA(m5) = m->get_submatch( 5 ).
        IF m5 IS NOT INITIAL.
          IF m5 CS '.'.
            SPLIT m5 AT '.' INTO TABLE build.
          ELSE.
            build = VALUE #( ( m5 ) ).
          ENDIF.
        ENDIF.

      CATCH cx_sy_matcher.
        zcx_semver_error=>raise( |Error evaluating regex for { version }| ).
    ENDTRY.

    format( ).

  ENDMETHOD.


  METHOD create.

    DATA(kind) = cl_abap_typedescr=>describe_by_data( version )->type_kind.

    IF kind = cl_abap_typedescr=>typekind_oref AND version IS INSTANCE OF zcl_semver.

      result ?= version.

      IF result->options-loose = loose AND result->options-incpre = incpre.
        RETURN.
      ENDIF.

      result = NEW zcl_semver( version = |{ result->version }| loose = loose incpre = incpre ).

    ELSEIF kind = cl_abap_typedescr=>typekind_char OR kind = cl_abap_typedescr=>typekind_string.

      result = NEW zcl_semver( version = |{ version }| loose = loose incpre = incpre ).

    ELSE.
      zcx_semver_error=>raise( 'Invalid parameter type' ).
    ENDIF.

  ENDMETHOD.


  METHOD format.

    version = |{ major }.{ minor }.{ patch }|.

    IF prerelease IS NOT INITIAL.
      version &&= |-{ concat_lines_of( table = prerelease sep = '.' ) }|.
    ENDIF.

    version = condense( version ).

    result = version.

  ENDMETHOD.


  METHOD inc.

    CASE release.
      WHEN 'premajor'.
        CLEAR prerelease.
        patch = 0.
        minor = 0.
        major += 1.
        inc( release = 'pre' identifier = identifier ).
      WHEN 'preminor'.
        CLEAR prerelease.
        patch = 0.
        minor += 1.
        inc( release = 'pre' identifier = identifier ).
      WHEN 'prepatch'.
        " If this is already a prerelease, it will bump to the next version
        " drop any prereleases that might already exist, since they are not
        " relevant at this point.
        CLEAR prerelease.
        inc( release = 'patch' identifier = identifier ).
        inc( release = 'pre' identifier = identifier ).
      WHEN 'prerelease'.
        " If the input is a non-prerelease version, this acts the same as
        " prepatch.
        IF prerelease IS INITIAL.
          inc( release = 'patch' identifier = identifier ).
        ENDIF.
        inc( release = 'pre' identifier = identifier ).
      WHEN 'major'.
        " If this is a pre-major version, bump up to the same major version.
        " Otherwise increment major.
        " 1.0.0-5 bumps to 1.0.0
        " 1.1.0 bumps to 2.0.0
        IF minor <> 0 OR patch <> 0 OR prerelease IS INITIAL.
          major += 1.
        ENDIF.
        minor = 0.
        patch = 0.
        CLEAR prerelease.
      WHEN 'minor'.
        " If this is a pre-minor version, bump up to the same minor version.
        " Otherwise increment minor.
        " 1.2.0-5 bumps to 1.2.0
        " 1.2.1 bumps to 1.3.0
        IF patch <> 0 OR prerelease IS INITIAL.
          minor += 1.
        ENDIF.
        patch = 0.
        CLEAR prerelease.
      WHEN 'patch'.
        " If this is not a pre-release version, it will increment the patch.
        " If it is a pre-release it will bump up to the same patch version.
        " 1.2.0-5 patches to 1.2.0
        " 1.2.0 patches to 1.2.1
        IF prerelease IS INITIAL.
          patch += 1.
        ENDIF.
        CLEAR prerelease.
      WHEN 'pre'.
        " This probably shouldn't be used publicly.
        " 1.0.0 'pre' would become 1.0.0-0 which is the wrong direction.
        IF prerelease IS INITIAL.
          prerelease = VALUE #( ( `0` ) ).
        ELSE.
          DATA(i) = lines( prerelease ).
          WHILE i > 0.
            IF zcl_semver_utils=>is_numeric( prerelease[ i ] ).
              prerelease[ i ] += 1.
              prerelease[ i ] = condense( prerelease[ i ] ).
              i = 0.
            ENDIF.
            i -= 1.
          ENDWHILE.
          IF i = 0.
            " didn't increment anything
            INSERT `0` INTO TABLE prerelease.
          ENDIF.
        ENDIF.
        IF identifier IS NOT INITIAL.
          " 1.2.0-beta.1 bumps to 1.2.0-beta.2,
          " 1.2.0-beta.fooblz or 1.2.0-beta bumps to 1.2.0-beta.0
          IF zcl_semver_identifiers=>compare_identifiers( a = prerelease[ 1 ] b = identifier ) = 0.
            IF NOT zcl_semver_utils=>is_numeric( VALUE #( prerelease[ 2 ] DEFAULT `-` ) ).
              prerelease = VALUE #( ( identifier ) ( `0` ) ).
            ENDIF.
          ELSE.
            prerelease = VALUE #( ( identifier ) ( `0` ) ).
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        zcx_semver_error=>raise( |Invalid increment argument { release }| ).
    ENDCASE.

    format( ).

    raw = version.

    result = me.

  ENDMETHOD.


  METHOD to_string.
    result = version.
  ENDMETHOD.
ENDCLASS.
