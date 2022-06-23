INTERFACE zif_semver_constants PUBLIC.

  CONSTANTS version TYPE string VALUE '1.0.0' ##NEEDED.

  " Note: this is the semver.org version of the spec that it implements
  " Not necessarily the package version of this code.
  CONSTANTS semver_spec_version TYPE string VALUE '2.0.0'.

  CONSTANTS max_length TYPE i VALUE 256.
  CONSTANTS max_safe_integer TYPE i VALUE cl_abap_math=>max_int4.

  " Max safe segment length for coercion.
  CONSTANTS max_safe_component_length TYPE i VALUE 16.

ENDINTERFACE.
