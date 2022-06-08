CLASS ltcl_semver_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      test_comparisons FOR TESTING RAISING zcx_semver_error,
      test_equality FOR TESTING RAISING zcx_semver_error,
      test_to_string FOR TESTING RAISING zcx_semver_error,
      test_garbage FOR TESTING RAISING zcx_semver_error,
      test_really_big FOR TESTING RAISING zcx_semver_error,
      test_invalid_version_numbers FOR TESTING RAISING zcx_semver_error,
      test_incrementing FOR TESTING RAISING zcx_semver_error,
      test_compare_main_vs_pre FOR TESTING RAISING zcx_semver_error,
      test_compare_build FOR TESTING RAISING zcx_semver_error.

ENDCLASS.

CLASS ltcl_semver_tests IMPLEMENTATION.

  METHOD test_comparisons.

    LOOP AT zcl_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose }|.
      DATA(s0) = NEW zcl_semver( version = comparison-v0 loose = comparison-loose ).
      DATA(s1) = NEW zcl_semver( version = comparison-v1 loose = comparison-loose ).

      cl_abap_unit_assert=>assert_equals( act = s0->compare( s1->version ) exp = 1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( comparison-v1 ) exp = 1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( s0->version ) exp = -1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( comparison-v0 ) exp = -1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( comparison-v0 ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( comparison-v1 ) exp = 0 msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_equality.

    LOOP AT zcl_semver_fixtures=>equality( ) INTO DATA(equality).
      DATA(msg) = |{ equality-v0 } { equality-v1 }|.
      DATA(s0) = NEW zcl_semver( version = equality-v0 loose = equality-loose ).
      DATA(s1) = NEW zcl_semver( version = equality-v1 loose = equality-loose ).

      cl_abap_unit_assert=>assert_equals( act = s0->compare( s1->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( s0->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( equality-v1 ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( equality-v0 ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( s0->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( s1->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare_pre( s1->version ) exp = 0 msg = msg ). " just to hit that code path
    ENDLOOP.

  ENDMETHOD.

  METHOD test_to_string.

    DATA(s) = NEW zcl_semver( 'v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = s->to_string( )
      exp = '1.2.3'
      msg = 'to_string does not equal parsed version' ).

  ENDMETHOD.

  METHOD test_garbage.

    LOOP AT zcl_semver_fixtures=>invalid_versions( ) INTO DATA(invalid_version).
      DATA(msg) = |{ invalid_version-value } { invalid_version-reason }|.

      TRY.
          DATA(s) = NEW zcl_semver( version = invalid_version-value loose = invalid_version-loose ).
          cl_abap_unit_assert=>fail( msg = msg ).
        CATCH zcx_semver_error ##NO_HANDLER.
          " throws when presented with garbage
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD test_really_big.
    " test really big numeric prerelease value.

    DATA(r) = NEW zcl_semver( |1.2.3-beta.{ zif_semver_constants=>max_safe_integer }0| ).

    cl_abap_unit_assert=>assert_equals(
      act = r->prerelease[ 1 ]
      exp = 'beta' ).

    cl_abap_unit_assert=>assert_equals(
      act = r->prerelease[ 2 ]
      exp = '21474836470' ).

  ENDMETHOD.

  METHOD test_invalid_version_numbers.

    DATA invalid_versions TYPE string_table.

    invalid_versions = VALUE #(
      ( `1.2.3.4` )
      ( `NOT VALID` )
      ( `1.2` )
      ( `` ) ).

    LOOP AT invalid_versions INTO DATA(invalid_version).
      TRY.
          DATA(s) = NEW zcl_semver( invalid_version ).
          cl_abap_unit_assert=>fail( msg = |Invalid version { invalid_version }| ).
        CATCH zcx_semver_error ##NO_HANDLER.
          " throws when presented with garbage
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD test_incrementing.

    LOOP AT zcl_semver_fixtures=>increments( ) INTO DATA(increments).
      DATA(msg) = |{ increments-version } { increments-inc } { increments-identifier }|.

      IF increments-expect = ''.
        TRY.
            DATA(s) = NEW zcl_semver( version = increments-version loose = increments-loose ).

            s->inc( release = increments-inc identifier = increments-identifier ).
            cl_abap_unit_assert=>fail( msg = msg ).
          CATCH zcx_semver_error ##NO_HANDLER.
            " throws when presented with garbage
        ENDTRY.
      ELSE.
        s = NEW zcl_semver( version = increments-version loose = increments-loose ).

        cl_abap_unit_assert=>assert_equals(
          act = s->inc( release = increments-inc identifier = increments-identifier )->version
          exp = increments-expect
          msg = msg ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD test_compare_main_vs_pre.

    DATA(s) = NEW zcl_semver( '1.2.3' ).

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

    DATA(p) = NEW zcl_semver( '1.2.3-alpha.0.pr.1' ).

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

  METHOD test_compare_build.

    DATA(nobuild) = NEW zcl_semver( '1.0.0' ).
    DATA(build0) = NEW zcl_semver( '1.0.0+0' ).
    DATA(build1) = NEW zcl_semver( '1.0.0+1' ).
    DATA(build10) = NEW zcl_semver( '1.0.0+1.0' ).

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
