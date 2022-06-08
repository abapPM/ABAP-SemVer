CLASS zcl_semver_re DEFINITION
  PUBLIC
  CREATE PUBLIC.

* TODO: Migrate to PCRE
* https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenregex_posix_pcre_incompat.htm

  PUBLIC SECTION.

    CONSTANTS:
      caret_trim_replace      TYPE string VALUE '$1^',
      tilde_trim_replace      TYPE string VALUE '$1~',
      comparator_trim_replace TYPE string VALUE '$1$2$3'.

    CLASS-METHODS:
      class_constructor.

    CLASS-DATA:
      BEGIN OF re,
        build                     TYPE REF TO cl_abap_regex,
        buildidentifier           TYPE REF TO cl_abap_regex,
        caret                     TYPE REF TO cl_abap_regex,
        caretloose                TYPE REF TO cl_abap_regex,
        carettrim                 TYPE REF TO cl_abap_regex,
        coerce                    TYPE REF TO cl_abap_regex,
        coercertl                 TYPE REF TO cl_abap_regex,
        comparator                TYPE REF TO cl_abap_regex,
        comparatorloose           TYPE REF TO cl_abap_regex,
        comparatortrim            TYPE REF TO cl_abap_regex,
        full                      TYPE REF TO cl_abap_regex,
        fullplain                 TYPE REF TO cl_abap_regex,
        gte0                      TYPE REF TO cl_abap_regex,
        gte0pre                   TYPE REF TO cl_abap_regex,
        gtlt                      TYPE REF TO cl_abap_regex,
        hyphenrange               TYPE REF TO cl_abap_regex,
        hyphenrangeloose          TYPE REF TO cl_abap_regex,
        lonecaret                 TYPE REF TO cl_abap_regex,
        lonetilde                 TYPE REF TO cl_abap_regex,
        loose                     TYPE REF TO cl_abap_regex,
        looseplain                TYPE REF TO cl_abap_regex,
        mainversion               TYPE REF TO cl_abap_regex,
        mainversionloose          TYPE REF TO cl_abap_regex,
        nonnumericidentifier      TYPE REF TO cl_abap_regex,
        numericidentifier         TYPE REF TO cl_abap_regex,
        numericidentifierloose    TYPE REF TO cl_abap_regex,
        prerelease                TYPE REF TO cl_abap_regex,
        prereleaseidentifier      TYPE REF TO cl_abap_regex,
        prereleaseidentifierloose TYPE REF TO cl_abap_regex,
        prereleaseloose           TYPE REF TO cl_abap_regex,
        star                      TYPE REF TO cl_abap_regex,
        tilde                     TYPE REF TO cl_abap_regex,
        tildeloose                TYPE REF TO cl_abap_regex,
        tildetrim                 TYPE REF TO cl_abap_regex,
        xrange                    TYPE REF TO cl_abap_regex,
        xrangeidentifier          TYPE REF TO cl_abap_regex,
        xrangeidentifierloose     TYPE REF TO cl_abap_regex,
        xrangeloose               TYPE REF TO cl_abap_regex,
        xrangeplain               TYPE REF TO cl_abap_regex,
        xrangeplainloose          TYPE REF TO cl_abap_regex,
      END OF re.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      BEGIN OF src,
        build                     TYPE string,
        buildidentifier           TYPE string,
        caret                     TYPE string,
        caretloose                TYPE string,
        carettrim                 TYPE string,
        coerce                    TYPE string,
        coercertl                 TYPE string,
        comparator                TYPE string,
        comparatorloose           TYPE string,
        comparatortrim            TYPE string,
        full                      TYPE string,
        fullplain                 TYPE string,
        gte0                      TYPE string,
        gte0pre                   TYPE string,
        gtlt                      TYPE string,
        hyphenrange               TYPE string,
        hyphenrangeloose          TYPE string,
        lonecaret                 TYPE string,
        lonetilde                 TYPE string,
        loose                     TYPE string,
        looseplain                TYPE string,
        mainversion               TYPE string,
        mainversionloose          TYPE string,
        nonnumericidentifier      TYPE string,
        numericidentifier         TYPE string,
        numericidentifierloose    TYPE string,
        prerelease                TYPE string,
        prereleaseidentifier      TYPE string,
        prereleaseidentifierloose TYPE string,
        prereleaseloose           TYPE string,
        star                      TYPE string,
        tilde                     TYPE string,
        tildeloose                TYPE string,
        tildetrim                 TYPE string,
        xrange                    TYPE string,
        xrangeidentifier          TYPE string,
        xrangeidentifierloose     TYPE string,
        xrangeloose               TYPE string,
        xrangeplain               TYPE string,
        xrangeplainloose          TYPE string,
      END OF src.

    CLASS-METHODS:
      create_token
        IMPORTING
          name      TYPE string
          value     TYPE string
          is_global TYPE abap_bool DEFAULT abap_false.

ENDCLASS.



CLASS zcl_semver_re IMPLEMENTATION.


  METHOD class_constructor.

    " The following Regular Expressions can be used for tokenizing,
    " validating, and parsing SemVer version strings.

    " ## Numeric Identifier
    " A single `0`, or a non-zero digit followed by zero or more digits.

    create_token(
      name  = 'NUMERICIDENTIFIER'
      value = |0\|[1-9]\\d*| ).
    create_token(
      name  = 'NUMERICIDENTIFIERLOOSE'
      value = |[0-9]+| ).

    " ## Non-numeric Identifier
    " Zero or more digits, followed by a letter or hyphen, and then zero or
    " more letters, digits, or hyphens.

    create_token(
      name  = 'NONNUMERICIDENTIFIER'
      value = |\\d*[a-zA-Z-][a-zA-Z0-9-]*| ).

    " ## Main Version
    " Three dot-separated numeric identifiers.

    create_token(
      name  = 'MAINVERSION'
      value = |({ src-numericidentifier })\\.| &&
              |({ src-numericidentifier })\\.| &&
              |({ src-numericidentifier })| ).

    create_token(
      name  = 'MAINVERSIONLOOSE'
      value = |({ src-numericidentifierloose })\\.| &&
              |({ src-numericidentifierloose })\\.| &&
              |({ src-numericidentifierloose })| ).

    " ## Pre-release Version Identifier
    " A numeric identifier, or a non-numeric identifier.

    create_token(
      name  = 'PRERELEASEIDENTIFIER'
      value = |(?:{ src-numericidentifier }\|| &&
              |{ src-nonnumericidentifier })| ).

    create_token(
      name  = 'PRERELEASEIDENTIFIERLOOSE'
      value = |(?:{ src-numericidentifierloose }\|| &&
              |{ src-nonnumericidentifier })| ).

    " ## Pre-release Version
    " Hyphen, followed by one or more dot-separated pre-release version
    " identifiers.

    create_token(
      name  = 'PRERELEASE'
      value = |(?:-({ src-prereleaseidentifier }| &&
              |(?:\\.{ src-prereleaseidentifier })*))| ).

    create_token(
      name  = 'PRERELEASELOOSE'
      value = |(?:-?({ src-prereleaseidentifierloose }| &&
              |(?:\\.{ src-prereleaseidentifierloose })*))| ).

    " ## Build Metadata Identifier
    " Any combination of digits, letters, or hyphens.

    create_token(
      name  = 'BUILDIDENTIFIER'
      value = |[0-9A-Za-z-]+| ).

    " ## Build Metadata
    " Plus sign, followed by one or more period-separated build metadata
    " identifiers.

    create_token(
      name  = 'BUILD'
      value = |(?:\\+({ src-buildidentifier }| &&
              |(?:\\.{ src-buildidentifier })*))| ).

    " ## Full Version String
    " A main version, followed optionally by a pre-release version and
    " build metadata.

    " Note that the only major, minor, patch, and pre-release sections of
    " the version string are capturing groups.  The build metadata is not a
    " capturing group, because it should not ever be used in version
    " comparison.

    create_token(
      name  = 'FULLPLAIN'
      value = |v?{ src-mainversion }{ src-prerelease }?{ src-build }?| ).

    create_token(
      name  = 'FULL'
      value = |^{ src-fullplain }$| ).

    " like full, but allows v1.2.3 and =1.2.3, which people do sometimes.
    " also, 1.0.0alpha1 (prerelease without the hyphen) which is pretty
    " common in the npm registry.

    create_token(
      name  = 'LOOSEPLAIN'
      value = |[v=\\s]*{ src-mainversionloose }{ src-prereleaseloose }?{ src-build }?| ).

    create_token(
      name  = 'LOOSE'
      value = |^{ src-looseplain }$| ).

    create_token(
      name  = 'GTLT'
      value = |((?:<\|>)?=?)| ).

    " Something like "2.*" or "1.2.x".
    " Note that "x.x" is a valid xRange identifer, meaning "any version"
    " Only the first item is strictly required.

    create_token(
      name  = 'XRANGEIDENTIFIERLOOSE'
      value = |{ src-numericidentifierloose }\|x\|X\|\\*| ).
    create_token(
      name  = 'XRANGEIDENTIFIER'
      value = |{ src-numericidentifier }\|x\|X\|\\*| ).

    create_token(
      name  = 'XRANGEPLAIN'
      value = |[v=\\s]*({ src-xrangeidentifier })| &&
              |(?:\\.({ src-xrangeidentifier })| &&
              |(?:\\.({ src-xrangeidentifier })| &&
              |(?:{ src-prerelease })?| &&
              |{ src-build }?| &&
              |)?)?| ).

    create_token(
      name  = 'XRANGEPLAINLOOSE'
      value = |[v=\\s]*({ src-xrangeidentifierloose })| &&
              |(?:\\.({ src-xrangeidentifierloose })| &&
              |(?:\\.({ src-xrangeidentifierloose })| &&
              |(?:{ src-prereleaseloose })?| &&
              |{ src-build }?| &&
              |)?)?| ).

    create_token(
      name  = 'XRANGE'
      value = |^{ src-gtlt }\\s*{ src-xrangeplain }$| ).
    create_token(
      name  = 'XRANGELOOSE'
      value = |^{ src-gtlt }\\s*{ src-xrangeplainloose }$| ).

    " Coercion.
    " Extract anything that could conceivably be a part of a valid semver

    create_token(
      name  = 'COERCE'
      value = |$(^\|[^\\d])(\\d\{1,{ zif_semver_constants=>max_safe_component_length }\})| &&
              |(?:\\.(\\d\{1,{ zif_semver_constants=>max_safe_component_length }\}))?| &&
              |(?:\\.(\\d\{1,{ zif_semver_constants=>max_safe_component_length }\}))?| &&
              |(?:$\|[^\\d])| ).
    create_token(
      name  = 'COERCERTL'
      value = src-coerce
      is_global = abap_true ).

    " Tilde ranges.
    " Meaning is "reasonably at or greater than"

    create_token(
      name  = 'LONETILDE'
      value = |(?:~>?)| ).

    create_token(
      name  = 'TILDETRIM'
      value = |(\\s*){ src-lonetilde }\\s+|
      is_global = abap_true ).

    create_token(
      name  = 'TILDE'
      value = |^{ src-lonetilde }{ src-xrangeplain }$| ).
    create_token(
      name  = 'TILDELOOSE'
      value = |^{ src-lonetilde }{ src-xrangeplainloose }$| ).

    " Caret ranges.
    " Meaning is "at least and backwards compatible with"

    create_token(
      name  = 'LONECARET'
      value = |(?:\\^)| ).

    create_token(
      name  = 'CARETTRIM'
      value = |(\\s*){ src-lonecaret }\\s+|
      is_global = abap_true ).

    create_token(
      name  = 'CARET'
      value = |^{ src-lonecaret }{ src-xrangeplain }$| ).
    create_token(
      name  = 'CARETLOOSE'
      value = |^{ src-lonecaret }{ src-xrangeplainloose }$| ).

    " A simple gt/lt/eq thing, or just "" to indicate "any version"

    create_token(
      name  = 'COMPARATORLOOSE'
      value = |^{ src-gtlt }\\s*({ src-looseplain })$\|^$| ).
    create_token(
      name  = 'COMPARATOR'
      value = |^{ src-gtlt }\\s*({ src-fullplain })$\|^$| ).

    " An expression to strip any whitespace between the gtlt and the thing
    " it modifies, so that `> 1.2.3` ==> `>1.2.3`

    create_token(
      name  = 'COMPARATORTRIM'
      value = |(\\s*){ src-gtlt }\\s*({ src-looseplain }\|| &&
              |{ src-xrangeplain })|
      is_global = abap_true ).

    " Something like `1.2.3 - 1.2.4`
    " Note that these all use the loose form, because they'll be
    " checked against either the strict or loose comparator form
    " later.

    create_token(
      name  = 'HYPHENRANGE'
      value = |^\\s*({ src-xrangeplain })| &&
              |\\s+-\\s+| &&
              |({ src-xrangeplain })| &&
              |\\s*$| ).

    create_token(
      name  = 'HYPHENRANGELOOSE'
      value = |^\\s*({ src-xrangeplainloose })| &&
              |\\s+-\\s+| &&
              |({ src-xrangeplainloose })| &&
              |\\s*$| ).

    " Star ranges basically just allow anything at all.

    create_token(
      name  = 'STAR'
      value = |(<\|>)?=?\\s*\\*| ).

    " >=0.0.0 is like a star

    create_token(
      name  = 'GTE0'
      value = |^\\s*>=\\s*0\\.0\\.0\\s*$| ).
    create_token(
      name  = 'GTE0PRE'
      value = |^\\s*>=\\s*0\\.0\\.0-0\\s*$| ).

  ENDMETHOD.


  METHOD create_token.

    ASSIGN COMPONENT name OF STRUCTURE src TO FIELD-SYMBOL(<src>).
    ASSERT sy-subrc = 0.

    <src> = value.

    ASSIGN COMPONENT name OF STRUCTURE re TO FIELD-SYMBOL(<regex>).
    ASSERT sy-subrc = 0.

    <regex> = NEW cl_abap_regex( value ).

  ENDMETHOD.
ENDCLASS.
