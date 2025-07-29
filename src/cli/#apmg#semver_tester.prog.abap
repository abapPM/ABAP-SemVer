************************************************************************
* SemVer Tester
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
REPORT /apmg/semver_tester.

CONSTANTS c_cmd TYPE string VALUE 'semver --help'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  SELECTION-SCREEN:
  COMMENT /1(79) sc_title,
  SKIP.
  PARAMETERS p_cmd TYPE c LENGTH 250 VISIBLE LENGTH 200 LOWER CASE OBLIGATORY.
  SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  sc_title = 'Semantic Version (SemVer)'.
  p_cmd = c_cmd.

START-OF-SELECTION.

  TRY.
      DATA(out) = /apmg/cl_semver_cli=>main( |{ p_cmd }| ).

      LOOP AT out ASSIGNING FIELD-SYMBOL(<out>).
        IF <out> IS INITIAL.
          SKIP.
        ELSE.
          WRITE / <out>.
        ENDIF.
      ENDLOOP.

    CATCH /apmg/cx_error INTO DATA(error).
      DATA(msg) = error->get_text( ).
      MESSAGE msg TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
