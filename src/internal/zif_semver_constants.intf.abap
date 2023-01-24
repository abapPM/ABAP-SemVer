INTERFACE zif_semver_constants PUBLIC.

************************************************************************
* SemVer Constants
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* ABAP Port by Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: ISC
************************************************************************
* Based on node semver package v7.3.8 (October 2022)
* https://github.com/npm/node-semver/releases/tag/v7.3.8
************************************************************************

  " Package version
  CONSTANTS version TYPE string VALUE '7.3.8' ##NEEDED.

  " Note: this is the semver.org version of the spec that it implements
  " Not necessarily the package version of this code.
  CONSTANTS semver_spec_version TYPE string VALUE '2.0.0'.

  CONSTANTS max_length TYPE i VALUE 256.
  CONSTANTS max_safe_integer TYPE i VALUE 999999998. " JS: int8

  " Max safe segment length for coercion.
  CONSTANTS max_safe_component_length TYPE i VALUE 9. " JS: 16 for int8

ENDINTERFACE.
