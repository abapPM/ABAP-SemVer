CLASS ltcl_semver_identifiers DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS test FOR TESTING.

ENDCLASS.

CLASS ltcl_semver_identifiers IMPLEMENTATION.

  METHOD test.
    " rcompareIdentifiers and compareIdentifiers

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_identifiers=>compare_identifiers( a = '1' b = '2' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_identifiers=>rcompare_identifiers( a = '1' b = '2' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_identifiers=>compare_identifiers( a = 'alpha' b = 'beta' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_identifiers=>rcompare_identifiers( a = 'alpha' b = 'beta' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_identifiers=>compare_identifiers( a = '0' b = 'beta' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_identifiers=>rcompare_identifiers( a = '0' b = 'beta' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_identifiers=>compare_identifiers( a = '0' b = '0' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_semver_identifiers=>rcompare_identifiers( a = '0' b = '0' )
      exp = 0 ).

  ENDMETHOD.

ENDCLASS.
