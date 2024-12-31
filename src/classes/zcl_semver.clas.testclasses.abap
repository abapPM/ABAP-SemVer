CLASS ltcl_semver DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      comparisons FOR TESTING RAISING zcx_error,
      equality FOR TESTING RAISING zcx_error,
      to_string FOR TESTING RAISING zcx_error,
      invalid_versions FOR TESTING RAISING zcx_error,
      options FOR TESTING RAISING zcx_error,
      really_big FOR TESTING RAISING zcx_error,
      incrementing FOR TESTING RAISING zcx_error,
      compare_main_vs_pre FOR TESTING RAISING zcx_error,
      compare_build FOR TESTING RAISING zcx_error.

ENDCLASS.

CLASS zcl_semver DEFINITION LOCAL FRIENDS ltcl_semver.

CLASS ltcl_semver IMPLEMENTATION.

  METHOD comparisons.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose }|.
      DATA(s0) = zcl_semver=>create( version = comparison-v0 loose = comparison-loose ).
      DATA(s1) = zcl_semver=>create( version = comparison-v1 loose = comparison-loose ).

      cl_abap_unit_assert=>assert_equals( act = s0->compare( s1->version ) exp = 1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( comparison-v1 ) exp = 1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( s0->version ) exp = -1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( comparison-v0 ) exp = -1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( comparison-v0 ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( comparison-v1 ) exp = 0 msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD equality.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      DATA(msg) = |{ equality-v0 } { equality-v1 }|.
      DATA(s0) = zcl_semver=>create( version = equality-v0 loose = equality-loose ).
      DATA(s1) = zcl_semver=>create( version = equality-v1 loose = equality-loose ).

      cl_abap_unit_assert=>assert_equals( act = s0->compare( s1->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( s0->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( equality-v1 ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( equality-v0 ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( s0->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( s1->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare_pre( s1->version ) exp = 0 msg = msg ). " just to hit that code path
    ENDLOOP.

  ENDMETHOD.

  METHOD to_string.

    DATA(s) = zcl_semver=>create( 'v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = s->to_string( )
      exp = '1.2.3'
      msg = 'to_string does not equal parsed version' ).

  ENDMETHOD.

  METHOD invalid_versions.
    " throws when presented with garbage

    LOOP AT zcl_semver_fixtures=>invalid_versions( ) INTO DATA(invalid_version).
      DATA(msg) = |{ invalid_version-value } { invalid_version-reason }|.

      TRY.
          DATA(s) = zcl_semver=>create( version = invalid_version-value loose = invalid_version-loose ).
          cl_abap_unit_assert=>fail( msg = msg ).
        CATCH zcx_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD options.

    DATA(s) = zcl_semver=>create( version = '1.2.3' loose = abap_true incpre = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_semver=>create( version = s loose = abap_true incpre = abap_true )
      exp = s
      msg = 'Should get same object when options match' ).

    IF zcl_semver=>create( s ) = s.
      cl_abap_unit_assert=>fail( msg = 'Should get new object when options do not match' ).
    ENDIF.

  ENDMETHOD.

  METHOD really_big.
    " test really big numeric prerelease value.

    DATA(s) = zcl_semver=>create( |1.2.3-beta.{ zif_semver_constants=>max_safe_integer }0| ).

    cl_abap_unit_assert=>assert_equals(
      act = s->prerelease[ 1 ]
      exp = 'beta' ).

    cl_abap_unit_assert=>assert_equals(
      act = s->prerelease[ 2 ]
      exp = |{ zif_semver_constants=>max_safe_integer }0| ).

  ENDMETHOD.

  METHOD incrementing.

    LOOP AT zcl_semver_fixtures=>increments( ) INTO DATA(increments).
      DATA(msg) = |{ increments-version } { increments-release } { increments-identifier }|.

      IF increments-res IS INITIAL.
        TRY.
            DATA(s) = zcl_semver=>create( version = increments-version loose = increments-loose ).

            s->inc(
              release         = increments-release
              identifier      = increments-identifier
              identifier_base = increments-identifier_base ).
            cl_abap_unit_assert=>fail( msg = msg ).
          CATCH zcx_error ##NO_HANDLER.
            " throws when presented with garbage
        ENDTRY.
      ELSE.
        s = zcl_semver=>create( version = increments-version loose = increments-loose ).

        DATA(inc) = s->inc(
          release         = increments-release
          identifier      = increments-identifier
          identifier_base = increments-identifier_base ).

        cl_abap_unit_assert=>assert_equals(
          act = inc->version
          exp = increments-res
          msg = msg ).

        IF inc->build IS NOT INITIAL.
          cl_abap_unit_assert=>assert_equals(
            act = inc->raw
            exp = |{ increments-res }+{ concat_lines_of( table = inc->build sep = '.' ) }|
            msg = msg ).
        ELSE.
          cl_abap_unit_assert=>assert_equals(
            act = inc->raw
            exp = increments-res
            msg = msg ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD compare_main_vs_pre.

    DATA(s) = zcl_semver=>create( '1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '2.3.4' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '1.2.4' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '0.1.2' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '1.2.2' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '1.2.3-pre' )
      exp = 0 ).

    DATA(p) = zcl_semver=>create( '1.2.3-alpha.0.pr.1' ).

    cl_abap_unit_assert=>assert_equals(
      act = p->compare_pre( '9.9.9-alpha.0.pr.1' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = p->compare_pre( '1.2.3' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = p->compare_pre( '1.2.3-alpha.0.pr.2' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = p->compare_pre( '1.2.3-alpha.0.2' )
      exp = +1 ).

  ENDMETHOD.

  METHOD compare_build.

    DATA(nobuild) = zcl_semver=>create( '1.0.0' ).
    DATA(build0) = zcl_semver=>create( '1.0.0+0' ).
    DATA(build1) = zcl_semver=>create( '1.0.0+1' ).
    DATA(build10) = zcl_semver=>create( '1.0.0+1.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = nobuild->compare_build( '1.0.0+0' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build0->compare_build( '1.0.0+0' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = build0->compare_build( '1.0.0' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build0->compare_build( '1.0.0+0.0' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build0->compare_build( '1.0.0+1' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build1->compare_build( '1.0.0+0' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build10->compare_build( '1.0.0+1' )
      exp = +1 ).

  ENDMETHOD.

ENDCLASS.
