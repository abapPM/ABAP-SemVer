CLASS /apmg/cl_semver_trim_whitespc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* SemVer Trim Comparator Whitespace
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS trim_comparator_whitespace
      IMPORTING
        range         TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_semver_trim_whitespc IMPLEMENTATION.


  METHOD trim_comparator_whitespace.

  ENDMETHOD.
ENDCLASS.
