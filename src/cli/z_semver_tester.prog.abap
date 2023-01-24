************************************************************************
* SemVer Tester
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* ABAP Port by Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: ISC
************************************************************************
REPORT z_semver_tester.

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
      DATA(out) = zcl_semver_cli=>main( |{ p_cmd }| ).

      LOOP AT out ASSIGNING FIELD-SYMBOL(<out>).
        IF <out> IS INITIAL.
          SKIP.
        ELSE.
          WRITE / <out>.
        ENDIF.
      ENDLOOP.

    CATCH zcx_semver_error INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
